(** Entity creation and manipulation functions *)

open Types

(** Generate a new UUID *)
let new_uuid () =
  Uuidm.v4_gen (Random.State.make_self_init ()) () |> Uuidm.to_string

(** Create fresh sync metadata for a new entity *)
let new_sync_meta ~device_id =
  let now = now () in
  {
    local_id = device_id;
    version = 1;
    modified_at = now;
    synced_at = None;
    deleted = false;
  }

(** Mark an entity as modified *)
let touch_sync sync =
  { sync with 
    version = sync.version + 1;
    modified_at = now ();
  }

(** Mark an entity as synced *)
let mark_synced sync =
  { sync with synced_at = Some (now ()) }

(** Soft delete an entity *)
let soft_delete sync =
  { sync with 
    deleted = true;
    version = sync.version + 1;
    modified_at = now ();
  }

(** Create a new task *)
let create_task ~device_id ~title ?description ?(priority = P2) ?(tags = []) 
    ?due_date ?scheduled_date ?parent_id ?project_id ?block_start ?block_end () : task =
  let now = now () in
  {
    id = new_uuid ();
    title;
    description;
    status = Inbox;
    priority;
    tags;
    due_date;
    scheduled_date;
    completed_at = None;
    recurrence = None;
    parent_id;
    project_id;
    block_start;
    block_end;
    created_at = now;
    sync = new_sync_meta ~device_id;
  }

(** Transition task to a new status *)
let set_task_status (task : task) status : task =
  let completed_at = 
    match status with
    | Done -> Some (now ())
    | _ -> None
  in
  { task with 
    status; 
    completed_at;
    sync = touch_sync task.sync;
  }

(** Create a new note *)
let create_note ~device_id ~title ?(content = "") ?(tags = []) ?project_id () : note =
  let now = now () in
  {
    id = new_uuid ();
    title;
    content;
    tags;
    project_id;
    created_at = now;
    sync = new_sync_meta ~device_id;
  }

(** Create a new event *)
let create_event ~device_id ~title ~start_time ?end_time ?description 
    ?location ?(tags = []) ?recurrence () : event =
  let now = now () in
  {
    id = new_uuid ();
    title;
    description;
    start_time;
    end_time;
    location;
    tags;
    recurrence;
    attendees = [];
    created_at = now;
    sync = new_sync_meta ~device_id;
  }

(** Create a new project *)
let create_project ~device_id ~name ?description ?(status = `Active) 
    ?(tags = []) ?parent_id () : project =
  let now = now () in
  {
    id = new_uuid ();
    name;
    description;
    status;
    tags;
    parent_id;
    created_at = now;
    sync = new_sync_meta ~device_id;
  }

(** Create a new contact *)
let create_contact ~device_id ~name ?email ?phone ?notes ?(tags = []) () : contact =
  let now = now () in
  {
    id = new_uuid ();
    name;
    email;
    phone;
    notes;
    tags;
    created_at = now;
    sync = new_sync_meta ~device_id;
  }

(** Create a new company *)
let create_company ~device_id ~name ?website ?industry ?address ?phone ?email 
    ?notes ?(tags = []) () : company =
  let now = now () in
  {
    id = new_uuid ();
    name;
    website;
    industry;
    address;
    phone;
    email;
    notes;
    tags;
    created_at = now;
    sync = new_sync_meta ~device_id;
  }

(** Create a new deal *)
let create_deal ~device_id ~name ?company_id ?contact_id ?(stage = Types.Lead)
    ?value ?(currency = "GBP") ?expected_close_date ?notes ?(tags = []) () : Types.deal =
  let now = now () in
  {
    id = new_uuid ();
    name;
    company_id;
    contact_id;
    stage;
    value;
    currency;
    expected_close_date;
    notes;
    tags;
    created_at = now;
    sync = new_sync_meta ~device_id;
  }

(** Create an email reference *)
let create_email_ref ~device_id ~message_id ~subject ~from_addr ~to_addrs 
    ~date ?maildir_path ?linked_task_id ?linked_note_id () : email_ref =
  let now_ts = now () in
  {
    id = new_uuid ();
    message_id;
    subject;
    from_addr;
    to_addrs;
    date;
    maildir_path;
    linked_task_id;
    linked_note_id;
    created_at = now_ts;
    sync = new_sync_meta ~device_id;
  }
