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
    let tags = if tags = "" then [] else String.split_on_char ',' tags in
    let task = Domain.Entity.create_task 
      ~device_id:config.device_id 
      ~title 
      ~priority
      ~tags
      ()
    in
    Printf.printf "Created task: %s (ID: %s)\n" task.title task.id;
    Printf.printf "Priority: %s\n" (Domain.Types.priority_to_string task.priority);
    if tags <> [] then
      Printf.printf "Tags: %s\n" (String.concat ", " tags)
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
    let _config = init_app config_path in
    Printf.printf "Task listing not yet implemented (requires database).\n"
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

(** Main command group *)
let main_cmd =
  let doc = "Personal Knowledge Management TUI with offline-first sync" in
  let info = Cmd.info "parenvault" ~version:"0.1.0" ~doc in
  let default = Term.(ret (const (`Help (`Pager, None)))) in
  Cmd.group info ~default [tui_cmd; sync_cmd; add_cmd; list_cmd; init_cmd; install_cmd]

let () = exit (Cmd.eval main_cmd)
