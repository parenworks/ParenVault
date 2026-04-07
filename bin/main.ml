(** ParenVault - Personal Knowledge Management TUI
    
    Main entry point with CLI argument parsing.
*)

open Cmdliner

(** Generate a new device ID if needed *)
let ensure_device_id config =
  if config.Config.device_id = "" then
    let id = Uuidm.v4_gen (Random.State.make_self_init ()) () |> Uuidm.to_string in
    { config with device_id = id }
  else
    config

(** Initialize the application *)
let init_app config_path =
  (* Load or create config *)
  let config = match Config.load config_path with
    | Ok c -> ensure_device_id c
    | Error msg -> 
      Printf.eprintf "Warning: Could not load config: %s\nUsing defaults.\n" msg;
      ensure_device_id (Config.default ())
  in
  (* Ensure data directory exists *)
  let data_dir = Filename.dirname config.db.local_path in
  if not (Sys.file_exists data_dir) then begin
    Unix.mkdir data_dir 0o755
  end;
  (* Save config with device ID *)
  Config.save config config_path;
  config

(** Run the TUI application *)
let run_tui config =
  Lwt_main.run (Tui.App.run ~config ())

(** CLI Commands *)

(** Main TUI command *)
let tui_cmd =
  let config_file =
    let doc = "Path to configuration file." in
    Arg.(value & opt string (Config.config_path ()) & info ["c"; "config"] ~docv:"FILE" ~doc)
  in
  let run config_path =
    let config = init_app config_path in
    run_tui config
  in
  let doc = "Start the ParenVault TUI" in
  let info = Cmd.info "tui" ~doc in
  Cmd.v info Term.(const run $ config_file)

(** Sync command *)
let sync_cmd =
  let config_file =
    let doc = "Path to configuration file." in
    Arg.(value & opt string (Config.config_path ()) & info ["c"; "config"] ~docv:"FILE" ~doc)
  in
  let run config_path =
    let config = init_app config_path in
    Printf.printf "Syncing with device ID: %s\n" config.device_id;
    match Config.postgres_uri config with
    | Some uri -> 
      Printf.printf "Remote: %s\n" (String.sub uri 0 (min 50 (String.length uri)) ^ "...");
      Printf.printf "Sync not yet implemented.\n"
    | None ->
      Printf.printf "No remote database configured.\n"
  in
  let doc = "Manually trigger sync with remote database" in
  let info = Cmd.info "sync" ~doc in
  Cmd.v info Term.(const run $ config_file)

(** Add task command *)
let add_cmd =
  let config_file =
    let doc = "Path to configuration file." in
    Arg.(value & opt string (Config.config_path ()) & info ["c"; "config"] ~docv:"FILE" ~doc)
  in
  let title =
    let doc = "Task title." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"TITLE" ~doc)
  in
  let priority =
    let doc = "Priority (p0, p1, p2, p3)." in
    Arg.(value & opt string "p2" & info ["p"; "priority"] ~docv:"PRIORITY" ~doc)
  in
  let due =
    let doc = "Due date (YYYY-MM-DD)." in
    Arg.(value & opt (some string) None & info ["d"; "due"] ~docv:"DATE" ~doc)
  in
  let tags =
    let doc = "Tags (comma-separated)." in
    Arg.(value & opt string "" & info ["t"; "tags"] ~docv:"TAGS" ~doc)
  in
  let run config_path title priority _due tags =
    let config = init_app config_path in
    let priority = match Domain.Types.priority_of_string priority with
      | Some p -> p
      | None -> Domain.Types.P2
    in
    let tags = if tags = "" then [] else String.split_on_char ',' tags |> List.map String.trim in
    Lwt_main.run (
      let open Lwt.Syntax in
      let* db_result = Storage.Db.init ~local_path:config.db.local_path () in
      match db_result with
      | Ok ctx ->
        let pool = Storage.Db.local ctx in
        let* result = Storage.Queries.create_task pool ~title ~description:"" ~priority ~tags () in
        (match result with
         | Some id ->
           Printf.printf "Created task: %s (ID: %s)\n" title (String.sub id 0 8);
           Printf.printf "Priority: %s\n" (Domain.Types.priority_to_string priority);
           if tags <> [] then
             Printf.printf "Tags: %s\n" (String.concat ", " tags)
         | None ->
           Printf.eprintf "Failed to create task\n");
        Lwt.return ()
      | Error msg ->
        Printf.eprintf "Failed to connect to database: %s\n" msg;
        Lwt.return ()
    )
  in
  let doc = "Add a new task from command line" in
  let info = Cmd.info "add" ~doc in
  Cmd.v info Term.(const run $ config_file $ title $ priority $ due $ tags)

(** List tasks command *)
let list_cmd =
  let config_file =
    let doc = "Path to configuration file." in
    Arg.(value & opt string (Config.config_path ()) & info ["c"; "config"] ~docv:"FILE" ~doc)
  in
  let status =
    let doc = "Filter by status." in
    Arg.(value & opt (some string) None & info ["s"; "status"] ~docv:"STATUS" ~doc)
  in
  let run config_path _status =
    let config = init_app config_path in
    Lwt_main.run (
      let open Lwt.Syntax in
      let* db_result = Storage.Db.init ~local_path:config.db.local_path () in
      match db_result with
      | Ok ctx ->
        let pool = Storage.Db.local ctx in
        let* tasks = Storage.Queries.list_tasks pool in
        if tasks = [] then
          Printf.printf "No tasks found.\n"
        else begin
          Printf.printf "%-8s  %-4s  %-8s  %s\n" "ID" "Pri" "Status" "Title";
          Printf.printf "%s\n" (String.make 60 '-');
          List.iter (fun (t : Domain.Types.task) ->
            let short_id = if String.length t.id >= 8 then String.sub t.id 0 8 else t.id in
            Printf.printf "%-8s  %-4s  %-8s  %s\n"
              short_id
              (Domain.Types.priority_to_string t.priority)
              (Domain.Types.task_status_to_string t.status)
              t.title
          ) tasks
        end;
        Lwt.return ()
      | Error msg ->
        Printf.eprintf "Failed to connect to database: %s\n" msg;
        Lwt.return ()
    )
  in
  let doc = "List tasks" in
  let info = Cmd.info "list" ~doc in
  Cmd.v info Term.(const run $ config_file $ status)

(** Init command - create config and database *)
let init_cmd =
  let config_file =
    let doc = "Path to configuration file." in
    Arg.(value & opt string (Config.config_path ()) & info ["c"; "config"] ~docv:"FILE" ~doc)
  in
  let run config_path =
    let config = init_app config_path in
    Printf.printf "ParenVault initialized!\n\n";
    Printf.printf "Device ID: %s\n" config.device_id;
    Printf.printf "Device Name: %s\n" config.device_name;
    Printf.printf "Local DB: %s\n" config.db.local_path;
    Printf.printf "Config: %s\n" config_path;
    Printf.printf "\nEdit the config file to set up:\n";
    Printf.printf "  - Remote PostgreSQL connection (via Tailscale)\n";
    Printf.printf "  - Maildir path for aerc integration\n"
  in
  let doc = "Initialize ParenVault configuration" in
  let info = Cmd.info "init" ~doc in
  Cmd.v info Term.(const run $ config_file)

(** Install command - install binary and wrapper to /usr/local/bin *)
let install_cmd =
  let prefix =
    let doc = "Installation prefix (default: /usr/local)." in
    Arg.(value & opt string "/usr/local" & info ["prefix"] ~docv:"PREFIX" ~doc)
  in
  let password =
    let doc = "PostgreSQL password to embed in wrapper script." in
    Arg.(value & opt string "b3l0wz3r0" & info ["password"] ~docv:"PASSWORD" ~doc)
  in
  let run prefix password =
    let exe_path = Sys.executable_name in
    let dest_dir = Filename.concat prefix "bin" in
    let binary_path = Filename.concat dest_dir "parenvault-bin" in
    let wrapper_path = Filename.concat dest_dir "pv" in
    (* Generate wrapper script content *)
    let wrapper_content = Printf.sprintf {|#!/bin/bash
# ParenVault wrapper - sets environment and runs the binary
export PARENVAULT_DB_PASSWORD="%s"
exec "%s" tui "$@"
|} password binary_path in
    (* Check if we need sudo *)
    let need_sudo = 
      try
        let test_file = Filename.concat dest_dir ".pv_test" in
        let oc = open_out test_file in
        close_out oc;
        Sys.remove test_file;
        false
      with _ -> true
    in
    if need_sudo then begin
      Printf.printf "Installing to %s (requires sudo)...\n" dest_dir;
      (* Create temp files for the wrapper *)
      let tmp_wrapper = Filename.temp_file "pv_wrapper" ".sh" in
      let oc = open_out tmp_wrapper in
      output_string oc wrapper_content;
      close_out oc;
      (* Install binary and wrapper with sudo *)
      let cmd = Printf.sprintf 
        "sudo cp '%s' '%s' && sudo chmod +x '%s' && sudo cp '%s' '%s' && sudo chmod +x '%s'" 
        exe_path binary_path binary_path
        tmp_wrapper wrapper_path wrapper_path in
      let ret = Sys.command cmd in
      Sys.remove tmp_wrapper;
      if ret = 0 then begin
        Printf.printf "Installed successfully!\n";
        Printf.printf "  Binary: %s\n" binary_path;
        Printf.printf "  Wrapper: %s\n\n" wrapper_path;
        Printf.printf "You can now run: pv\n"
      end else
        Printf.eprintf "Installation failed (exit code %d)\n" ret
    end else begin
      Printf.printf "Installing to %s...\n" dest_dir;
      (* Copy binary *)
      let ic = open_in_bin exe_path in
      let oc = open_out_bin binary_path in
      (try
        let buf = Bytes.create 4096 in
        let rec copy () =
          let n = input ic buf 0 4096 in
          if n > 0 then (output oc buf 0 n; copy ())
        in
        copy ()
      with e -> close_in ic; close_out oc; raise e);
      close_in ic;
      close_out oc;
      Unix.chmod binary_path 0o755;
      (* Write wrapper script *)
      let oc = open_out wrapper_path in
      output_string oc wrapper_content;
      close_out oc;
      Unix.chmod wrapper_path 0o755;
      Printf.printf "Installed successfully!\n";
      Printf.printf "  Binary: %s\n" binary_path;
      Printf.printf "  Wrapper: %s\n\n" wrapper_path;
      Printf.printf "You can now run: pv\n"
    end
  in
  let doc = "Install ParenVault binary and wrapper to system path" in
  let info = Cmd.info "install" ~doc in
  Cmd.v info Term.(const run $ prefix $ password)

(** Import email as task - reads RFC 2822 email from stdin *)
let import_email_cmd =
  let config_file =
    let doc = "Path to configuration file." in
    Arg.(value & opt string (Config.config_path ()) & info ["c"; "config"] ~docv:"FILE" ~doc)
  in
  let as_note =
    let doc = "Import as note instead of task." in
    Arg.(value & flag & info ["n"; "note"] ~doc)
  in
  let run config_path as_note =
    let config = init_app config_path in
    (* Read email from stdin *)
    let buf = Buffer.create 4096 in
    (try
      while true do
        Buffer.add_string buf (input_line stdin);
        Buffer.add_char buf '\n'
      done
    with End_of_file -> ());
    let content = Buffer.contents buf in
    (* Parse headers *)
    let lines = String.split_on_char '\n' content in
    let rec parse_headers acc = function
      | [] -> acc
      | "" :: _ -> acc
      | line :: rest when String.length line > 0 && (line.[0] = ' ' || line.[0] = '\t') ->
        (match acc with
         | (k, v) :: acc_rest -> parse_headers ((k, v ^ " " ^ String.trim line) :: acc_rest) rest
         | [] -> parse_headers acc rest)
      | line :: rest ->
        (match String.index_opt line ':' with
         | Some i ->
           let key = String.lowercase_ascii (String.sub line 0 i) in
           let value = String.trim (String.sub line (i + 1) (String.length line - i - 1)) in
           parse_headers ((key, value) :: acc) rest
         | None -> parse_headers acc rest)
    in
    let headers = parse_headers [] lines in
    let get key = List.assoc_opt key headers |> Option.value ~default:"" in
    let subject = get "subject" in
    let from = get "from" in
    let date = get "date" in
    (* Extract body - find text/plain part from MIME or use raw body *)
    let raw_body = 
      try
        let pos = Str.search_forward (Str.regexp "\n\n\\|\r\n\r\n") content 0 in
        String.sub content (pos + 2) (String.length content - pos - 2)
      with Not_found -> ""
    in
    (* Strip \r from content *)
    let strip_cr s = String.split_on_char '\r' s |> String.concat "" in
    (* Decode base64 string *)
    let decode_base64 s =
      try
        let clean = String.split_on_char '\n' s |> List.map String.trim |> String.concat "" in
        Base64.decode_exn clean
      with _ -> s
    in
    (* Extract text/plain body from MIME or raw email *)
    let body_text =
      let raw = strip_cr raw_body |> String.trim in
      let lines = String.split_on_char '\n' raw in
      (* State machine: scan for text/plain part, collect its content *)
      let rec scan found_plain is_base64 past_headers acc = function
        | [] ->
          if acc <> [] then
            let text = String.concat "\n" (List.rev acc) in
            if is_base64 then decode_base64 text else text
          else raw
        | line :: rest ->
          let trimmed = String.trim line in
          if String.length trimmed >= 2 && trimmed.[0] = '-' && trimmed.[1] = '-' then begin
            (* Hit a MIME boundary *)
            if found_plain && past_headers && acc <> [] then
              (* We already collected text/plain content, return it *)
              let text = String.concat "\n" (List.rev acc) in
              if is_base64 then decode_base64 text else text
            else
              (* New MIME part starting, reset state *)
              scan false false false [] rest
          end else if not found_plain then begin
            let lower = String.lowercase_ascii trimmed in
            if String.length lower > 0 && (
              try let _ = Str.search_forward (Str.regexp_string "content-type:") lower 0 in
                  let _ = Str.search_forward (Str.regexp_string "text/plain") lower 0 in true
              with Not_found -> false) then
              scan true is_base64 false acc rest
            else if String.length lower > 0 && (
              try let _ = Str.search_forward (Str.regexp_string "content-transfer-encoding:") lower 0 in
                  let _ = Str.search_forward (Str.regexp_string "base64") lower 0 in true
              with Not_found -> false) then
              scan found_plain true past_headers acc rest
            else
              scan false is_base64 false acc rest
          end else if not past_headers then begin
            let lower = String.lowercase_ascii trimmed in
            if trimmed = "" then
              scan true is_base64 true acc rest
            else if String.length lower > 0 && (
              try let _ = Str.search_forward (Str.regexp_string "content-transfer-encoding:") lower 0 in
                  let _ = Str.search_forward (Str.regexp_string "base64") lower 0 in true
              with Not_found -> false) then
              scan true true false acc rest
            else
              scan true is_base64 false acc rest
          end else begin
            scan true is_base64 true (line :: acc) rest
          end
      in
      (* Check if body contains MIME boundaries *)
      let has_mime = try let _ = Str.search_forward (Str.regexp_string "Content-Type:") raw 0 in true with Not_found -> false in
      if has_mime then scan false false false [] lines
      else raw
    in
    let body_preview = if String.length body_text > 2000 then String.sub body_text 0 2000 ^ "\n\n[truncated]" else body_text in
    (* Create task or note *)
    if as_note then begin
      let note = Domain.Entity.create_note
        ~device_id:config.device_id
        ~title:subject
        ~content:(Printf.sprintf "From: %s\nDate: %s\n\n%s" from date body_preview)
        ~tags:["email"]
        ()
      in
      (* Save to database *)
      Lwt_main.run (
        let open Lwt.Syntax in
        let* db_result = Storage.Db.init ~local_path:config.db.local_path () in
        match db_result with
        | Ok ctx ->
          let pool = Storage.Db.local ctx in
          let* _ = Storage.Queries.create_note pool ~title:note.title ~content:note.content ~tags:note.tags () in
          Printf.printf "Created note: %s\n" note.title;
          Lwt.return ()
        | Error msg ->
          Printf.eprintf "Failed to connect to database: %s\n" msg;
          Lwt.return ()
      )
    end else begin
      let task = Domain.Entity.create_task
        ~device_id:config.device_id
        ~title:subject
        ~description:(Printf.sprintf "From: %s\n\n%s" from body_preview)
        ~priority:Domain.Types.P2
        ~tags:["email"]
        ()
      in
      (* Save to database *)
      Lwt_main.run (
        let open Lwt.Syntax in
        let* db_result = Storage.Db.init ~local_path:config.db.local_path () in
        match db_result with
        | Ok ctx ->
          let pool = Storage.Db.local ctx in
          let* _ = Storage.Queries.create_task pool ~title:task.title ~description:(Option.value ~default:"" task.description) ~priority:task.priority ~tags:task.tags () in
          Printf.printf "Created task: %s\n" task.title;
          Lwt.return ()
        | Error msg ->
          Printf.eprintf "Failed to connect to database: %s\n" msg;
          Lwt.return ()
      )
    end
  in
  let doc = "Import email as task (or note with -n). Reads RFC 2822 email from stdin. In aerc: :pipe parenvault import-email (or :pipe parenvault import-email -n for notes)." in
  let info = Cmd.info "import-email" ~doc in
  Cmd.v info Term.(const run $ config_file $ as_note)

(** Send task/note as email - opens aerc compose *)
let send_cmd =
  let config_file =
    let doc = "Path to configuration file." in
    Arg.(value & opt string (Config.config_path ()) & info ["c"; "config"] ~docv:"FILE" ~doc)
  in
  let item_id =
    let doc = "Task or note ID to send." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"ID" ~doc)
  in
  let recipient =
    let doc = "Recipient email address." in
    Arg.(required & pos 1 (some string) None & info [] ~docv:"EMAIL" ~doc)
  in
  let run config_path item_id recipient =
    let config = init_app config_path in
    Lwt_main.run (
      let open Lwt.Syntax in
      let* db_result = Storage.Db.init ~local_path:config.db.local_path () in
      match db_result with
      | Ok ctx ->
        let p = Storage.Db.local ctx in
        (* Try to find as task first, then note *)
        let* tasks = Storage.Queries.list_tasks p in
        let* notes = Storage.Queries.list_notes p in
        let task_opt = List.find_opt (fun (t : Domain.Types.task) -> 
          String.length t.id >= 8 && String.sub t.id 0 8 = item_id
        ) tasks in
        let note_opt = List.find_opt (fun (n : Domain.Types.note) -> 
          String.length n.id >= 8 && String.sub n.id 0 8 = item_id
        ) notes in
        (match task_opt, note_opt with
         | Some task, _ ->
           let subject = Printf.sprintf "[Task] %s" task.title in
           let body = Printf.sprintf "Task: %s\n\nStatus: %s\nPriority: %s\n\n%s\n\n--\nSent from ParenVault"
             task.title
             (Domain.Types.task_status_to_string task.status)
             (Domain.Types.priority_to_string task.priority)
             (Option.value ~default:"" task.description)
           in
           let mailto = Printf.sprintf "mailto:%s?subject=%s&body=%s" 
             recipient 
             (Uri.pct_encode subject)
             (Uri.pct_encode body)
           in
           let _ = Sys.command (Printf.sprintf "aerc '%s' &" mailto) in
           Printf.printf "Opening aerc compose for task: %s\n" task.title;
           Lwt.return ()
         | None, Some note ->
           let subject = Printf.sprintf "[Note] %s" note.title in
           let body = Printf.sprintf "%s\n\n--\nSent from ParenVault" note.content in
           let mailto = Printf.sprintf "mailto:%s?subject=%s&body=%s"
             recipient
             (Uri.pct_encode subject)
             (Uri.pct_encode body)
           in
           let _ = Sys.command (Printf.sprintf "aerc '%s' &" mailto) in
           Printf.printf "Opening aerc compose for note: %s\n" note.title;
           Lwt.return ()
         | None, None ->
           Printf.eprintf "No task or note found with ID starting with: %s\n" item_id;
           Lwt.return ())
      | Error msg ->
        Printf.eprintf "Failed to connect to database: %s\n" msg;
        Lwt.return ()
    )
  in
  let doc = "Send task or note as email via aerc. Usage: parenvault send <ID> <EMAIL>" in
  let info = Cmd.info "send" ~doc in
  Cmd.v info Term.(const run $ config_file $ item_id $ recipient)

(** Main command group *)
let main_cmd =
  let doc = "Personal Knowledge Management TUI with offline-first sync" in
  let info = Cmd.info "parenvault" ~version:"0.1.0" ~doc in
  let default_run =
    let config_file =
      Arg.(value & opt string (Config.config_path ()) & info ["c"; "config"] ~docv:"FILE" ~doc:"Path to configuration file.")
    in
    Term.(const (fun config_path -> let config = init_app config_path in run_tui config) $ config_file)
  in
  Cmd.group info ~default:default_run [tui_cmd; sync_cmd; add_cmd; list_cmd; init_cmd; install_cmd; import_email_cmd; send_cmd]

let () = exit (Cmd.eval main_cmd)
