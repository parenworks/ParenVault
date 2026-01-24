(** Email composition for sending task updates via aerc.
    
    Creates draft emails that can be sent through aerc.
*)

open Lwt.Syntax

(** Email draft *)
type draft = {
  to_: string list;
  cc: string list;
  subject: string;
  body: string;
  in_reply_to: string option;
  references: string list;
}

(** Create a draft from a task for status update *)
let task_status_update (task : Domain.Types.task) ~recipient =
  let open Domain.Types in
  let status_emoji = match task.status with
    | Inbox -> "📥"
    | Todo -> "📋"
    | InProgress -> "🔄"
    | Waiting -> "⏸️"
    | Done -> "✅"
    | Cancelled -> "❌"
  in
  let priority_text = match task.priority with
    | P0 -> "🔴 Critical"
    | P1 -> "🟠 High"
    | P2 -> "🟡 Medium"
    | P3 -> "🟢 Low"
  in
  let due_text = match task.due_date with
    | Some d -> 
      let (y, m, day) = Ptime.to_date d.time in
      Printf.sprintf "Due: %04d-%02d-%02d" y m day
    | None -> ""
  in
  {
    to_ = [recipient];
    cc = [];
    subject = Printf.sprintf "[Task Update] %s %s" status_emoji task.title;
    body = Printf.sprintf {|Task Status Update

%s %s

Status: %s %s
Priority: %s
%s

%s

---
Sent from ParenVault
|}
      status_emoji task.title
      status_emoji (task_status_to_string task.status)
      priority_text
      due_text
      (Option.value ~default:"No description" task.description);
    in_reply_to = None;
    references = [];
  }

(** Create a draft for task assignment notification *)
let task_assignment (task : Domain.Types.task) ~assignee_email =
  let open Domain.Types in
  {
    to_ = [assignee_email];
    cc = [];
    subject = Printf.sprintf "[Assigned] %s" task.title;
    body = Printf.sprintf {|You have been assigned a task:

📋 %s

Priority: %s
%s

%s

---
Sent from ParenVault
|}
      task.title
      (priority_to_string task.priority)
      (match task.due_date with
       | Some d -> 
         let (y, m, day) = Ptime.to_date d.time in
         Printf.sprintf "Due: %04d-%02d-%02d" y m day
       | None -> "No due date")
      (Option.value ~default:"No description" task.description);
    in_reply_to = None;
    references = [];
  }

(** Create a draft for task completion notification *)
let task_completed (task : Domain.Types.task) ~recipient =
  let open Domain.Types in
  {
    to_ = [recipient];
    cc = [];
    subject = Printf.sprintf "[Completed] ✅ %s" task.title;
    body = Printf.sprintf {|Task completed:

✅ %s

Completed at: %s

%s

---
Sent from ParenVault
|}
      task.title
      (match task.completed_at with
       | Some t -> Ptime.to_rfc3339 t.time
       | None -> "Unknown")
      (Option.value ~default:"" task.description);
    in_reply_to = None;
    references = [];
  }

(** Format draft as RFC 2822 email *)
let format_rfc2822 draft =
  let headers = [
    "To: " ^ String.concat ", " draft.to_;
    (if draft.cc <> [] then "Cc: " ^ String.concat ", " draft.cc else "");
    "Subject: " ^ draft.subject;
    "Content-Type: text/plain; charset=utf-8";
    "MIME-Version: 1.0";
    (match draft.in_reply_to with Some id -> "In-Reply-To: " ^ id | None -> "");
    (if draft.references <> [] then "References: " ^ String.concat " " draft.references else "");
  ] |> List.filter (fun s -> s <> "") in
  String.concat "\r\n" headers ^ "\r\n\r\n" ^ draft.body

(** Save draft to aerc drafts folder *)
let save_to_drafts ~maildir_path draft =
  let drafts_path = Filename.concat maildir_path "Drafts" in
  let tmp_path = Filename.concat drafts_path "tmp" in
  let new_path = Filename.concat drafts_path "new" in
  (* Generate unique filename *)
  let timestamp = Unix.gettimeofday () in
  let hostname = Unix.gethostname () in
  let filename = Printf.sprintf "%f.%d.%s" timestamp (Unix.getpid ()) hostname in
  let tmp_file = Filename.concat tmp_path filename in
  let new_file = Filename.concat new_path filename in
  (* Write to tmp, then move to new (maildir protocol) *)
  let content = format_rfc2822 draft in
  let* () = Lwt_io.with_file ~mode:Lwt_io.Output tmp_file (fun oc ->
    Lwt_io.write oc content
  ) in
  let* () = Lwt_unix.rename tmp_file new_file in
  Lwt.return new_file

(** Queue a draft for sending via aerc *)
let queue_for_send ~maildir_path draft =
  let outbox_path = Filename.concat maildir_path "Outbox" in
  let tmp_path = Filename.concat outbox_path "tmp" in
  let new_path = Filename.concat outbox_path "new" in
  let timestamp = Unix.gettimeofday () in
  let hostname = Unix.gethostname () in
  let filename = Printf.sprintf "%f.%d.%s" timestamp (Unix.getpid ()) hostname in
  let tmp_file = Filename.concat tmp_path filename in
  let new_file = Filename.concat new_path filename in
  let content = format_rfc2822 draft in
  let* () = 
    try
      Lwt_io.with_file ~mode:Lwt_io.Output tmp_file (fun oc ->
        Lwt_io.write oc content
      )
    with _ -> Lwt.return ()
  in
  let* () = 
    try Lwt_unix.rename tmp_file new_file
    with _ -> Lwt.return ()
  in
  Lwt.return new_file
