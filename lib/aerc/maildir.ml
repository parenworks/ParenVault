(** Maildir integration for aerc.
    
    Watches maildir folders for new emails and can create tasks/notes from them.
    Also supports sending task updates as emails.
*)

open Lwt.Syntax

(** Maildir folder structure *)
type maildir = {
  path: string;
  cur: string;   (** Read messages *)
  new_: string;  (** New messages *)
  tmp: string;   (** Temporary during delivery *)
}

(** Create maildir paths *)
let make_maildir path =
  {
    path;
    cur = Filename.concat path "cur";
    new_ = Filename.concat path "new";
    tmp = Filename.concat path "tmp";
  }

(** Check if a path is a valid maildir *)
let is_maildir path =
  let md = make_maildir path in
  Sys.file_exists md.cur && Sys.is_directory md.cur &&
  Sys.file_exists md.new_ && Sys.is_directory md.new_ &&
  Sys.file_exists md.tmp && Sys.is_directory md.tmp

(** List all message files in a maildir folder *)
let list_messages folder =
  if Sys.file_exists folder && Sys.is_directory folder then
    Sys.readdir folder
    |> Array.to_list
    |> List.filter (fun f -> not (String.get f 0 = '.'))
    |> List.map (Filename.concat folder)
  else
    []

(** Parse email headers from a file *)
let parse_email_headers path =
  let* content = Lwt_io.with_file ~mode:Lwt_io.Input path (fun ic ->
    Lwt_io.read ic
  ) in
  (* Simple header parsing - mrmime would be used for full parsing *)
  let lines = String.split_on_char '\n' content in
  let rec parse_headers acc = function
    | [] -> acc
    | "" :: _ -> acc  (* Empty line ends headers *)
    | line :: rest when String.length line > 0 && (line.[0] = ' ' || line.[0] = '\t') ->
      (* Continuation of previous header *)
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
  Lwt.return (parse_headers [] lines)

(** Extract email info for creating a task *)
type email_info = {
  message_id: string;
  subject: string;
  from: string;
  to_: string list;
  date: string;
  body_preview: string;
}

let extract_email_info path =
  let* headers = parse_email_headers path in
  let get key = List.assoc_opt key headers |> Option.value ~default:"" in
  let* content = Lwt_io.with_file ~mode:Lwt_io.Input path Lwt_io.read in
  (* Get first 500 chars of body as preview *)
  let body_start = 
    match String.index_opt content '\n' with
    | Some _ ->
      (* Find double newline that separates headers from body *)
      (try
        let re = Str.regexp "\n\n\\|\r\n\r\n" in
        let pos = Str.search_forward re content 0 in
        String.sub content (pos + 2) (min 500 (String.length content - pos - 2))
      with Not_found -> "")
    | None -> ""
  in
  Lwt.return {
    message_id = get "message-id";
    subject = get "subject";
    from = get "from";
    to_ = [get "to"];  (* Simplified - would parse properly *)
    date = get "date";
    body_preview = body_start;
  }

(** Create a task from an email *)
let task_from_email ~device_id (email : email_info) =
  let open Domain.Entity in
  create_task 
    ~device_id
    ~title:email.subject
    ~description:(Printf.sprintf "From: %s\n\n%s" email.from email.body_preview)
    ~priority:Domain.Types.P2
    ~tags:["email"]
    ()

(** Create a note from an email *)
let note_from_email ~device_id (email : email_info) =
  let open Domain.Entity in
  create_note
    ~device_id
    ~title:email.subject
    ~content:(Printf.sprintf "# %s\n\nFrom: %s\nDate: %s\n\n%s"
      email.subject email.from email.date email.body_preview)
    ~tags:["email"]
    ()

(** Watch configuration *)
type watch_config = {
  maildir_path: string;
  inbox_folder: string;      (** Folder to watch for new emails *)
  action_folder: string;     (** Folder where emails are moved to create tasks *)
  processed_folder: string;  (** Where to move processed emails *)
}

(** Default aerc maildir location *)
let default_maildir () =
  let home = Sys.getenv_opt "HOME" |> Option.value ~default:"~" in
  Filename.concat home "Mail"

(** Scan for new emails in action folder *)
let scan_action_folder config =
  let action_path = Filename.concat config.maildir_path config.action_folder in
  let md = make_maildir action_path in
  let new_messages = list_messages md.new_ in
  let cur_messages = list_messages md.cur in
  Lwt.return (new_messages @ cur_messages)

(** Process an email from action folder - create task and move to processed *)
let process_action_email ~device_id config email_path =
  let* info = extract_email_info email_path in
  let task = task_from_email ~device_id info in
  (* Would save task to database here *)
  (* Move email to processed folder *)
  let filename = Filename.basename email_path in
  let processed_path = 
    Filename.concat config.maildir_path config.processed_folder
    |> fun p -> Filename.concat p "cur"
    |> fun p -> Filename.concat p filename
  in
  let* () = 
    try 
      Lwt_unix.rename email_path processed_path
    with _ -> Lwt.return ()
  in
  Lwt.return (Some task)
