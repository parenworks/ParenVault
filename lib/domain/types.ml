(** Core domain types for ParenVault PKM system *)

(** Unique identifier for all entities *)
type uuid = string

(** Priority levels for tasks *)
type priority = 
  | P0  (** Critical/Urgent *)
  | P1  (** High *)
  | P2  (** Medium *)
  | P3  (** Low *)

let priority_to_string = function
  | P0 -> "critical"
  | P1 -> "high"
  | P2 -> "medium"
  | P3 -> "low"

let priority_of_string = function
  | "critical" | "p0" -> Some P0
  | "high" | "p1" -> Some P1
  | "medium" | "p2" -> Some P2
  | "low" | "p3" -> Some P3
  | _ -> None

(** Task status *)
type task_status =
  | Inbox       (** Newly captured, not yet processed *)
  | Todo        (** Ready to work on *)
  | InProgress  (** Currently being worked on *)
  | Waiting     (** Blocked on external input *)
  | Done        (** Completed *)
  | Cancelled   (** Abandoned *)

let task_status_to_string = function
  | Inbox -> "inbox"
  | Todo -> "todo"
  | InProgress -> "in_progress"
  | Waiting -> "waiting"
  | Done -> "done"
  | Cancelled -> "cancelled"

let task_status_of_string = function
  | "inbox" -> Some Inbox
  | "todo" -> Some Todo
  | "in_progress" -> Some InProgress
  | "waiting" -> Some Waiting
  | "done" -> Some Done
  | "cancelled" -> Some Cancelled
  | _ -> None

(** Timestamps with timezone awareness *)
type timestamp = {
  time: Ptime.t;
  timezone: int option;  (** Timezone offset in seconds *)
}

let now () = 
  { time = Ptime_clock.now (); timezone = None }

(** Sync metadata for offline-first operation *)
type sync_meta = {
  local_id: uuid;           (** Local device ID *)
  version: int;             (** Monotonic version counter *)
  modified_at: timestamp;   (** Last modification time *)
  synced_at: timestamp option;  (** Last successful sync *)
  deleted: bool;            (** Soft delete flag *)
}

(** A tag for categorization *)
type tag = string

(** A task/todo item *)
type task = {
  id: uuid;
  title: string;
  description: string option;
  status: task_status;
  priority: priority;
  tags: tag list;
  due_date: timestamp option;
  scheduled_date: timestamp option;
  completed_at: timestamp option;
  recurrence: string option;    (** None, Daily, Weekly, Monthly, Yearly *)
  parent_id: uuid option;       (** For subtasks *)
  project_id: uuid option;      (** Associated project *)
  created_at: timestamp;
  sync: sync_meta;
}

(** A note/document *)
type note = {
  id: uuid;
  title: string;
  content: string;              (** Markdown content *)
  tags: tag list;
  project_id: uuid option;
  created_at: timestamp;
  sync: sync_meta;
}

(** A calendar event *)
type event = {
  id: uuid;
  title: string;
  description: string option;
  start_time: timestamp;
  end_time: timestamp option;   (** None for all-day or point events *)
  location: string option;
  tags: tag list;
  recurrence: string option;    (** iCal RRULE format *)
  attendees: uuid list;         (** Contact IDs *)
  created_at: timestamp;
  sync: sync_meta;
}

(** A project/area for grouping *)
type project = {
  id: uuid;
  name: string;
  description: string option;
  status: [`Active | `Archived | `Someday];
  tags: tag list;
  parent_id: uuid option;       (** For nested projects/areas *)
  created_at: timestamp;
  sync: sync_meta;
}

(** A contact/person *)
type contact = {
  id: uuid;
  name: string;
  email: string option;
  phone: string option;
  notes: string option;
  tags: tag list;
  created_at: timestamp;
  sync: sync_meta;
}

(** Email message reference (for aerc integration) *)
type email_ref = {
  id: uuid;
  message_id: string;           (** RFC 2822 Message-ID *)
  subject: string;
  from_addr: string;
  to_addrs: string list;
  date: timestamp;
  maildir_path: string option;  (** Path in maildir *)
  linked_task_id: uuid option;  (** Task created from this email *)
  linked_note_id: uuid option;  (** Note created from this email *)
  created_at: timestamp;
  sync: sync_meta;
}

(** Link between entities *)
type link = {
  id: uuid;
  source_type: string;          (** "task", "note", "event", etc. *)
  source_id: uuid;
  target_type: string;
  target_id: uuid;
  link_type: string;            (** "blocks", "related", "parent", etc. *)
  created_at: timestamp;
  sync: sync_meta;
}

(** File attachment *)
type attachment = {
  id: uuid;
  filename: string;             (** Original filename *)
  filepath: string;             (** Path relative to attachments dir *)
  mime_type: string option;     (** MIME type if known *)
  size_bytes: int64;            (** File size in bytes *)
  entity_type: string;          (** "task", "note", "event" *)
  entity_id: uuid;              (** ID of linked entity *)
  created_at: timestamp;
  sync: sync_meta;
}

