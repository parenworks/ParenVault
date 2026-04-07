(** Database queries for all entities *)

open Lwt.Syntax
open Domain.Types

module Q = Caqti_request.Infix
module T = Caqti_type

(** UUID generation *)
let new_uuid () = 
  Uuidm.v4_gen (Random.State.make_self_init ()) () |> Uuidm.to_string

(** Current timestamp *)
let now () = Ptime_clock.now ()

(* ============================================
   TASK QUERIES
   ============================================ *)

let task_of_row (id, (title, (description, (status_str, (priority_str, (tags_json, (due_date, (scheduled_date, (completed_at, (recurrence, (project_id, (parent_id, (block_start, (block_end, created_at)))))))))))))) =
  let status = task_status_of_string status_str |> Option.value ~default:Inbox in
  let priority = priority_of_string priority_str |> Option.value ~default:P2 in
  let tags = try Yojson.Safe.from_string tags_json |> Yojson.Safe.Util.to_list |> List.map Yojson.Safe.Util.to_string with _ -> [] in
  let to_timestamp = function None -> None | Some t -> Some { time = t; timezone = None } in
  {
    id;
    title;
    description;
    status;
    priority;
    tags;
    due_date = to_timestamp due_date;
    scheduled_date = to_timestamp scheduled_date;
    completed_at = to_timestamp completed_at;
    recurrence;
    project_id;
    parent_id;
    block_start = to_timestamp block_start;
    block_end = to_timestamp block_end;
    created_at = { time = created_at; timezone = None };
    sync = { local_id = id; version = 1; modified_at = { time = created_at; timezone = None }; synced_at = None; deleted = false };
  }

let task_row_type = T.(t2 string (t2 string (t2 (option string) (t2 string (t2 string (t2 string (t2 (option ptime) (t2 (option ptime) (t2 (option ptime) (t2 (option string) (t2 (option string) (t2 (option string) (t2 (option ptime) (t2 (option ptime) ptime))))))))))))))

let list_tasks_query = Q.(T.unit ->* task_row_type)
  "SELECT id, title, description, status, priority, tags, due_date, scheduled_date, completed_at, recurrence, project_id, parent_id, block_start, block_end, created_at FROM tasks WHERE NOT sync_deleted ORDER BY created_at DESC"

let list_tasks pool =
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.collect_list list_tasks_query ()
  ) in
  match result with
  | Ok rows -> Lwt.return (List.map task_of_row rows)
  | Error _ -> Lwt.return []

let list_deleted_tasks_query = Q.(T.unit ->* task_row_type)
  "SELECT id, title, description, status, priority, tags, due_date, scheduled_date, completed_at, recurrence, project_id, parent_id, block_start, block_end, created_at FROM tasks WHERE sync_deleted ORDER BY created_at DESC"

let list_deleted_tasks pool =
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.collect_list list_deleted_tasks_query ()
  ) in
  match result with
  | Ok rows -> Lwt.return (List.map task_of_row rows)
  | Error _ -> Lwt.return []

let restore_task_query = Q.(T.(t2 ptime string) ->. T.unit)
  "UPDATE tasks SET sync_deleted = false, sync_modified_at = ? WHERE id = ?"

let restore_task pool ~id =
  let now = now () in
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.exec restore_task_query (now, id)
  ) in
  match result with
  | Ok () -> Lwt.return true
  | Error _ -> Lwt.return false

let permanent_delete_task_query = Q.(T.string ->. T.unit)
  "DELETE FROM tasks WHERE id = ?"

let permanent_delete_task pool ~id =
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.exec permanent_delete_task_query id
  ) in
  match result with
  | Ok () -> Lwt.return true
  | Error _ -> Lwt.return false

let insert_task_query = Q.(T.(t2 string (t2 string (t2 (option string) (t2 string (t2 string (t2 string (t2 (option ptime) (t2 (option ptime) (t2 (option string) (t2 (option string) (t2 (option string) (t2 (option ptime) (t2 (option ptime) (t2 ptime (t2 string ptime))))))))))))))) ->. T.unit)
  "INSERT INTO tasks (id, title, description, status, priority, tags, due_date, scheduled_date, recurrence, project_id, parent_id, block_start, block_end, created_at, sync_local_id, sync_version, sync_modified_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 1, ?)"

let create_task pool ~title ?description ?(status=Inbox) ?(priority=P2) ?(tags=[]) ?due_date ?scheduled_date ?recurrence ?project_id ?parent_id ?block_start ?block_end () =
  let id = new_uuid () in
  let now = now () in
  let status_str = task_status_to_string status in
  let priority_str = priority_to_string priority in
  let tags_json = `List (List.map (fun t -> `String t) tags) |> Yojson.Safe.to_string in
  let due = Option.map (fun (ts : timestamp) -> ts.time) due_date in
  let sched = Option.map (fun (ts : timestamp) -> ts.time) scheduled_date in
  let blk_start = Option.map (fun (ts : timestamp) -> ts.time) block_start in
  let blk_end = Option.map (fun (ts : timestamp) -> ts.time) block_end in
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.exec insert_task_query (id, (title, (description, (status_str, (priority_str, (tags_json, (due, (sched, (recurrence, (project_id, (parent_id, (blk_start, (blk_end, (now, (id, now)))))))))))))))
  ) in
  match result with
  | Ok () -> Lwt.return (Some id)
  | Error _ -> Lwt.return None

let update_task_status_query = Q.(T.(t2 string (t2 ptime string)) ->. T.unit)
  "UPDATE tasks SET status = ?, sync_modified_at = ? WHERE id = ?"

let update_task_status pool ~id ~status =
  let status_str = task_status_to_string status in
  let now = now () in
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.exec update_task_status_query (status_str, (now, id))
  ) in
  match result with
  | Ok () -> Lwt.return true
  | Error _ -> Lwt.return false

let update_task_query = Q.(T.(t2 string (t2 (option string) (t2 string (t2 (option string) (t2 (option ptime) (t2 (option ptime) (t2 (option ptime) (t2 (option ptime) (t2 ptime string))))))))) ->. T.unit)
  "UPDATE tasks SET title = ?, description = ?, tags = ?, recurrence = ?, due_date = ?, scheduled_date = ?, block_start = ?, block_end = ?, sync_modified_at = ? WHERE id = ?"

let update_task pool ~id ~title ?description ?(tags=[]) ?recurrence ?due_date ?scheduled_date ?block_start ?block_end () =
  let now = now () in
  let tags_json = `List (List.map (fun t -> `String t) tags) |> Yojson.Safe.to_string in
  let due = Option.map (fun (ts : timestamp) -> ts.time) due_date in
  let sched = Option.map (fun (ts : timestamp) -> ts.time) scheduled_date in
  let blk_start = Option.map (fun (ts : timestamp) -> ts.time) block_start in
  let blk_end = Option.map (fun (ts : timestamp) -> ts.time) block_end in
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.exec update_task_query (title, (description, (tags_json, (recurrence, (due, (sched, (blk_start, (blk_end, (now, id)))))))))
  ) in
  match result with
  | Ok () -> Lwt.return true
  | Error _ -> Lwt.return false

let delete_task_query = Q.(T.(t2 ptime string) ->. T.unit)
  "UPDATE tasks SET sync_deleted = TRUE, sync_modified_at = ? WHERE id = ?"

let delete_task pool ~id =
  let now = now () in
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.exec delete_task_query (now, id)
  ) in
  match result with
  | Ok () -> Lwt.return true
  | Error _ -> Lwt.return false

(* Upsert task for sync - insert or update based on existence *)
let upsert_task_query = Q.(T.(t2 string (t2 string (t2 (option string) (t2 string (t2 string (t2 string (t2 (option ptime) (t2 (option ptime) (t2 (option string) (t2 (option string) (t2 (option string) (t2 (option ptime) (t2 (option ptime) (t2 ptime (t2 string (t2 ptime bool)))))))))))))))) ->. T.unit)
  "INSERT INTO tasks (id, title, description, status, priority, tags, due_date, scheduled_date, recurrence, project_id, parent_id, block_start, block_end, created_at, sync_local_id, sync_modified_at, sync_deleted, sync_version) 
   VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 1)
   ON CONFLICT(id) DO UPDATE SET 
     title = excluded.title, description = excluded.description, status = excluded.status, 
     priority = excluded.priority, tags = excluded.tags, due_date = excluded.due_date,
     scheduled_date = excluded.scheduled_date, recurrence = excluded.recurrence,
     project_id = excluded.project_id, parent_id = excluded.parent_id,
     block_start = excluded.block_start, block_end = excluded.block_end,
     sync_modified_at = excluded.sync_modified_at, sync_deleted = excluded.sync_deleted,
     sync_version = sync_version + 1"

let upsert_task pool (task : task) =
  let status_str = task_status_to_string task.status in
  let priority_str = priority_to_string task.priority in
  let tags_json = `List (List.map (fun t -> `String t) task.tags) |> Yojson.Safe.to_string in
  let due = Option.map (fun (ts : timestamp) -> ts.time) task.due_date in
  let sched = Option.map (fun (ts : timestamp) -> ts.time) task.scheduled_date in
  let blk_start = Option.map (fun (ts : timestamp) -> ts.time) task.block_start in
  let blk_end = Option.map (fun (ts : timestamp) -> ts.time) task.block_end in
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.exec upsert_task_query (task.id, (task.title, (task.description, (status_str, (priority_str, (tags_json, (due, (sched, (task.recurrence, (task.project_id, (task.parent_id, (blk_start, (blk_end, (task.created_at.time, (task.sync.local_id, (task.sync.modified_at.time, task.sync.deleted))))))))))))))))
  ) in
  match result with
  | Ok () -> Lwt.return true
  | Error _ -> Lwt.return false

(* ============================================
   NOTE QUERIES
   ============================================ *)

let note_of_row (id, (title, (content, (tags_json, (project_id, created_at))))) =
  let tags = try Yojson.Safe.from_string tags_json |> Yojson.Safe.Util.to_list |> List.map Yojson.Safe.Util.to_string with _ -> [] in
  {
    id;
    title;
    content;
    tags;
    project_id;
    created_at = { time = created_at; timezone = None };
    sync = { local_id = id; version = 1; modified_at = { time = created_at; timezone = None }; synced_at = None; deleted = false };
  }

let note_row_type = T.(t2 string (t2 string (t2 string (t2 string (t2 (option string) ptime)))))

let list_notes_query = Q.(T.unit ->* note_row_type)
  "SELECT id, title, content, tags, project_id, created_at FROM notes WHERE NOT sync_deleted ORDER BY created_at DESC"

let list_notes pool =
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.collect_list list_notes_query ()
  ) in
  match result with
  | Ok rows -> Lwt.return (List.map note_of_row rows)
  | Error _ -> Lwt.return []

let list_deleted_notes_query = Q.(T.unit ->* note_row_type)
  "SELECT id, title, content, tags, project_id, created_at FROM notes WHERE sync_deleted ORDER BY created_at DESC"

let list_deleted_notes pool =
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.collect_list list_deleted_notes_query ()
  ) in
  match result with
  | Ok rows -> Lwt.return (List.map note_of_row rows)
  | Error _ -> Lwt.return []

let restore_note_query = Q.(T.(t2 ptime string) ->. T.unit)
  "UPDATE notes SET sync_deleted = false, sync_modified_at = ? WHERE id = ?"

let restore_note pool ~id =
  let now = now () in
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.exec restore_note_query (now, id)
  ) in
  match result with
  | Ok () -> Lwt.return true
  | Error _ -> Lwt.return false

let permanent_delete_note_query = Q.(T.string ->. T.unit)
  "DELETE FROM notes WHERE id = ?"

let permanent_delete_note pool ~id =
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.exec permanent_delete_note_query id
  ) in
  match result with
  | Ok () -> Lwt.return true
  | Error _ -> Lwt.return false

(* Upsert note for sync *)
let upsert_note_query = Q.(T.(t2 string (t2 string (t2 string (t2 string (t2 (option string) (t2 ptime (t2 string (t2 ptime bool)))))))) ->. T.unit)
  "INSERT INTO notes (id, title, content, tags, project_id, created_at, sync_local_id, sync_modified_at, sync_deleted, sync_version) 
   VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, 1)
   ON CONFLICT(id) DO UPDATE SET 
     title = excluded.title, content = excluded.content, tags = excluded.tags,
     project_id = excluded.project_id, sync_modified_at = excluded.sync_modified_at, 
     sync_deleted = excluded.sync_deleted, sync_version = sync_version + 1"

let upsert_note pool (note : note) =
  let tags_json = `List (List.map (fun t -> `String t) note.tags) |> Yojson.Safe.to_string in
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.exec upsert_note_query (note.id, (note.title, (note.content, (tags_json, (note.project_id, (note.created_at.time, (note.sync.local_id, (note.sync.modified_at.time, note.sync.deleted))))))))
  ) in
  match result with
  | Ok () -> Lwt.return true
  | Error _ -> Lwt.return false

let insert_note_query = Q.(T.(t2 string (t2 string (t2 string (t2 string (t2 (option string) (t2 ptime (t2 string ptime))))))) ->. T.unit)
  "INSERT INTO notes (id, title, content, tags, project_id, created_at, sync_local_id, sync_version, sync_modified_at) VALUES (?, ?, ?, ?, ?, ?, ?, 1, ?)"

let create_note pool ~title ?(content="") ?(tags=[]) ?project_id () =
  let id = new_uuid () in
  let now = now () in
  let tags_json = `List (List.map (fun t -> `String t) tags) |> Yojson.Safe.to_string in
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.exec insert_note_query (id, (title, (content, (tags_json, (project_id, (now, (id, now)))))))
  ) in
  match result with
  | Ok () -> Lwt.return (Some id)
  | Error _ -> Lwt.return None

let update_note_query = Q.(T.(t2 string (t2 string (t2 ptime string))) ->. T.unit)
  "UPDATE notes SET title = ?, content = ?, sync_modified_at = ? WHERE id = ?"

let update_note pool ~id ~title ~content () =
  let now = now () in
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.exec update_note_query (title, (content, (now, id)))
  ) in
  match result with
  | Ok () -> Lwt.return true
  | Error _ -> Lwt.return false

let delete_note_query = Q.(T.(t2 ptime string) ->. T.unit)
  "UPDATE notes SET sync_deleted = TRUE, sync_modified_at = ? WHERE id = ?"

let delete_note pool ~id =
  let now = now () in
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.exec delete_note_query (now, id)
  ) in
  match result with
  | Ok () -> Lwt.return true
  | Error _ -> Lwt.return false

(* ============================================
   EVENT QUERIES
   ============================================ *)

let event_of_row (id, (title, (description, (start_time, (end_time, (location, (tags_json, (recurrence, created_at)))))))) =
  let tags = try Yojson.Safe.from_string tags_json |> Yojson.Safe.Util.to_list |> List.map Yojson.Safe.Util.to_string with _ -> [] in
  let to_timestamp t = { time = t; timezone = None } in
  {
    id;
    title;
    description;
    start_time = to_timestamp start_time;
    end_time = Option.map to_timestamp end_time;
    location;
    tags;
    recurrence;
    attendees = [];
    created_at = to_timestamp created_at;
    sync = { local_id = id; version = 1; modified_at = { time = created_at; timezone = None }; synced_at = None; deleted = false };
  }

let event_row_type = T.(t2 string (t2 string (t2 (option string) (t2 ptime (t2 (option ptime) (t2 (option string) (t2 string (t2 (option string) ptime))))))))

let list_events_query = Q.(T.unit ->* event_row_type)
  "SELECT id, title, description, start_time, end_time, location, tags, recurrence, created_at FROM events WHERE NOT sync_deleted ORDER BY start_time ASC"

let list_events pool =
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.collect_list list_events_query ()
  ) in
  match result with
  | Ok rows -> Lwt.return (List.map event_of_row rows)
  | Error _ -> Lwt.return []

let list_deleted_events_query = Q.(T.unit ->* event_row_type)
  "SELECT id, title, description, start_time, end_time, location, tags, recurrence, created_at FROM events WHERE sync_deleted ORDER BY start_time ASC"

let list_deleted_events pool =
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.collect_list list_deleted_events_query ()
  ) in
  match result with
  | Ok rows -> Lwt.return (List.map event_of_row rows)
  | Error _ -> Lwt.return []

let restore_event_query = Q.(T.(t2 ptime string) ->. T.unit)
  "UPDATE events SET sync_deleted = false, sync_modified_at = ? WHERE id = ?"

let restore_event pool ~id =
  let now = now () in
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.exec restore_event_query (now, id)
  ) in
  match result with
  | Ok () -> Lwt.return true
  | Error _ -> Lwt.return false

let permanent_delete_event_query = Q.(T.string ->. T.unit)
  "DELETE FROM events WHERE id = ?"

let permanent_delete_event pool ~id =
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.exec permanent_delete_event_query id
  ) in
  match result with
  | Ok () -> Lwt.return true
  | Error _ -> Lwt.return false

(* Upsert event for sync *)
let upsert_event_query = Q.(T.(t2 string (t2 string (t2 (option string) (t2 ptime (t2 (option ptime) (t2 (option string) (t2 string (t2 (option string) (t2 ptime (t2 string (t2 ptime bool))))))))))) ->. T.unit)
  "INSERT INTO events (id, title, description, start_time, end_time, location, tags, recurrence, created_at, sync_local_id, sync_modified_at, sync_deleted, sync_version) 
   VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 1)
   ON CONFLICT(id) DO UPDATE SET 
     title = excluded.title, description = excluded.description, start_time = excluded.start_time,
     end_time = excluded.end_time, location = excluded.location, tags = excluded.tags,
     recurrence = excluded.recurrence, sync_modified_at = excluded.sync_modified_at, 
     sync_deleted = excluded.sync_deleted, sync_version = sync_version + 1"

let upsert_event pool (event : event) =
  let tags_json = `List (List.map (fun t -> `String t) event.tags) |> Yojson.Safe.to_string in
  let end_t = Option.map (fun (ts : timestamp) -> ts.time) event.end_time in
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.exec upsert_event_query (event.id, (event.title, (event.description, (event.start_time.time, (end_t, (event.location, (tags_json, (event.recurrence, (event.created_at.time, (event.sync.local_id, (event.sync.modified_at.time, event.sync.deleted)))))))))))
  ) in
  match result with
  | Ok () -> Lwt.return true
  | Error _ -> Lwt.return false

let insert_event_query = Q.(T.(t2 string (t2 string (t2 (option string) (t2 ptime (t2 (option ptime) (t2 (option string) (t2 string (t2 (option string) (t2 ptime (t2 string ptime)))))))))) ->. T.unit)
  "INSERT INTO events (id, title, description, start_time, end_time, location, tags, recurrence, created_at, sync_local_id, sync_version, sync_modified_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 1, ?)"

let create_event pool ~title ?description ~start_time ?end_time ?location ?(tags=[]) ?recurrence () =
  let id = new_uuid () in
  let now = now () in
  let tags_json = `List (List.map (fun t -> `String t) tags) |> Yojson.Safe.to_string in
  let start_t = start_time.time in
  let end_t = Option.map (fun (ts : timestamp) -> ts.time) end_time in
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.exec insert_event_query (id, (title, (description, (start_t, (end_t, (location, (tags_json, (recurrence, (now, (id, now))))))))))
  ) in
  match result with
  | Ok () -> Lwt.return (Some id)
  | Error _ -> Lwt.return None

let update_event_query = Q.(T.(t2 string (t2 (option string) (t2 (option string) (t2 ptime (t2 (option string) (t2 ptime string)))))) ->. T.unit)
  "UPDATE events SET title = ?, description = ?, location = ?, start_time = ?, recurrence = ?, sync_modified_at = ? WHERE id = ?"

let update_event pool ~id ~title ?description ?location ~start_time ?recurrence () =
  let now = now () in
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.exec update_event_query (title, (description, (location, (start_time.time, (recurrence, (now, id))))))
  ) in
  match result with
  | Ok () -> Lwt.return true
  | Error _ -> Lwt.return false

let delete_event_query = Q.(T.(t2 ptime string) ->. T.unit)
  "UPDATE events SET sync_deleted = TRUE, sync_modified_at = ? WHERE id = ?"

let delete_event pool ~id =
  let now = now () in
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.exec delete_event_query (now, id)
  ) in
  match result with
  | Ok () -> Lwt.return true
  | Error _ -> Lwt.return false

(* ============================================
   CONTACT QUERIES
   ============================================ *)

let contact_of_row (id, (name, (email, (phone, (notes, (tags_json, created_at)))))) =
  let tags = try Yojson.Safe.from_string tags_json |> Yojson.Safe.Util.to_list |> List.map Yojson.Safe.Util.to_string with _ -> [] in
  {
    id;
    name;
    email;
    phone;
    notes;
    tags;
    created_at = { time = created_at; timezone = None };
    sync = { local_id = id; version = 1; modified_at = { time = created_at; timezone = None }; synced_at = None; deleted = false };
  }

let contact_row_type = T.(t2 string (t2 string (t2 (option string) (t2 (option string) (t2 (option string) (t2 string ptime))))))

let list_contacts_query = Q.(T.unit ->* contact_row_type)
  "SELECT id, name, email, phone, notes, tags, created_at FROM contacts WHERE NOT sync_deleted ORDER BY name ASC"

let list_contacts pool =
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.collect_list list_contacts_query ()
  ) in
  match result with
  | Ok rows -> Lwt.return (List.map contact_of_row rows)
  | Error _ -> Lwt.return []

let insert_contact_query = Q.(T.(t2 string (t2 string (t2 (option string) (t2 (option string) (t2 (option string) (t2 string (t2 ptime (t2 string ptime)))))))) ->. T.unit)
  "INSERT INTO contacts (id, name, email, phone, notes, tags, created_at, sync_local_id, sync_version, sync_modified_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?, 1, ?)"

let create_contact pool ~name ?email ?phone ?notes ?(tags=[]) () =
  let id = new_uuid () in
  let now = now () in
  let tags_json = `List (List.map (fun t -> `String t) tags) |> Yojson.Safe.to_string in
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.exec insert_contact_query (id, (name, (email, (phone, (notes, (tags_json, (now, (id, now))))))))
  ) in
  match result with
  | Ok () -> Lwt.return (Some id)
  | Error _ -> Lwt.return None

let update_contact_query = Q.(T.(t2 string (t2 (option string) (t2 (option string) (t2 (option string) (t2 ptime string))))) ->. T.unit)
  "UPDATE contacts SET name = ?, email = ?, phone = ?, notes = ?, sync_modified_at = ? WHERE id = ?"

let update_contact pool ~id ~name ?email ?phone ?notes () =
  let now = now () in
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.exec update_contact_query (name, (email, (phone, (notes, (now, id)))))
  ) in
  match result with
  | Ok () -> Lwt.return true
  | Error _ -> Lwt.return false

let delete_contact_query = Q.(T.(t2 ptime string) ->. T.unit)
  "UPDATE contacts SET sync_deleted = TRUE, sync_modified_at = ? WHERE id = ?"

let delete_contact pool ~id =
  let now = now () in
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.exec delete_contact_query (now, id)
  ) in
  match result with
  | Ok () -> Lwt.return true
  | Error _ -> Lwt.return false

(* ============================================
   COMPANY QUERIES
   ============================================ *)

let company_of_row (id, (name, (website, (industry, (address, (phone, (email, (notes, (tags_json, created_at))))))))) =
  let tags = try Yojson.Safe.from_string tags_json |> Yojson.Safe.Util.to_list |> List.map Yojson.Safe.Util.to_string with _ -> [] in
  {
    id;
    name;
    website;
    industry;
    address;
    phone;
    email;
    notes;
    tags;
    created_at = { time = created_at; timezone = None };
    sync = { local_id = id; version = 1; modified_at = { time = created_at; timezone = None }; synced_at = None; deleted = false };
  }

let company_row_type = T.(t2 string (t2 string (t2 (option string) (t2 (option string) (t2 (option string) (t2 (option string) (t2 (option string) (t2 (option string) (t2 string ptime)))))))))

let list_companies_query = Q.(T.unit ->* company_row_type)
  "SELECT id, name, website, industry, address, phone, email, notes, tags, created_at FROM companies WHERE NOT sync_deleted ORDER BY name ASC"

let list_companies pool =
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.collect_list list_companies_query ()
  ) in
  match result with
  | Ok rows -> Lwt.return (List.map company_of_row rows)
  | Error _ -> Lwt.return []

let insert_company_query = Q.(T.(t2 string (t2 string (t2 (option string) (t2 (option string) (t2 (option string) (t2 (option string) (t2 (option string) (t2 (option string) (t2 string (t2 ptime (t2 string ptime))))))))))) ->. T.unit)
  "INSERT INTO companies (id, name, website, industry, address, phone, email, notes, tags, created_at, sync_local_id, sync_version, sync_modified_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 1, ?)"

let create_company pool ~name ?website ?industry ?address ?phone ?email ?notes ?(tags=[]) () =
  let id = new_uuid () in
  let now = now () in
  let tags_json = `List (List.map (fun t -> `String t) tags) |> Yojson.Safe.to_string in
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.exec insert_company_query (id, (name, (website, (industry, (address, (phone, (email, (notes, (tags_json, (now, (id, now)))))))))))
  ) in
  match result with
  | Ok () -> Lwt.return (Some id)
  | Error _ -> Lwt.return None

let update_company_query = Q.(T.(t2 string (t2 (option string) (t2 (option string) (t2 (option string) (t2 (option string) (t2 (option string) (t2 (option string) (t2 string (t2 ptime string))))))))) ->. T.unit)
  "UPDATE companies SET name = ?, website = ?, industry = ?, address = ?, phone = ?, email = ?, notes = ?, tags = ?, sync_modified_at = ? WHERE id = ?"

let update_company pool ~id ~name ?website ?industry ?address ?phone ?email ?notes ?(tags=[]) () =
  let now = now () in
  let tags_json = `List (List.map (fun t -> `String t) tags) |> Yojson.Safe.to_string in
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.exec update_company_query (name, (website, (industry, (address, (phone, (email, (notes, (tags_json, (now, id)))))))))
  ) in
  match result with
  | Ok () -> Lwt.return true
  | Error _ -> Lwt.return false

let delete_company_query = Q.(T.(t2 ptime string) ->. T.unit)
  "UPDATE companies SET sync_deleted = TRUE, sync_modified_at = ? WHERE id = ?"

let delete_company pool ~id =
  let now = now () in
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.exec delete_company_query (now, id)
  ) in
  match result with
  | Ok () -> Lwt.return true
  | Error _ -> Lwt.return false

let upsert_company_query = Q.(T.(t2 string (t2 string (t2 (option string) (t2 (option string) (t2 (option string) (t2 (option string) (t2 (option string) (t2 (option string) (t2 string (t2 ptime (t2 string (t2 ptime bool)))))))))))) ->. T.unit)
  "INSERT INTO companies (id, name, website, industry, address, phone, email, notes, tags, created_at, sync_local_id, sync_modified_at, sync_deleted, sync_version) 
   VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 1)
   ON CONFLICT(id) DO UPDATE SET 
     name = excluded.name, website = excluded.website, industry = excluded.industry,
     address = excluded.address, phone = excluded.phone, email = excluded.email,
     notes = excluded.notes, tags = excluded.tags, sync_modified_at = excluded.sync_modified_at, 
     sync_deleted = excluded.sync_deleted, sync_version = sync_version + 1"

let upsert_company pool (company : company) =
  let tags_json = `List (List.map (fun t -> `String t) company.tags) |> Yojson.Safe.to_string in
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.exec upsert_company_query (company.id, (company.name, (company.website, (company.industry, (company.address, (company.phone, (company.email, (company.notes, (tags_json, (company.created_at.time, (company.sync.local_id, (company.sync.modified_at.time, company.sync.deleted))))))))))))
  ) in
  match result with
  | Ok () -> Lwt.return true
  | Error _ -> Lwt.return false

(* ============================================
   DEAL QUERIES
   ============================================ *)

let deal_of_row (id, (name, (company_id, (contact_id, (stage_str, (value, (currency, (expected_close_date, (notes, (tags_json, created_at)))))))))) =
  let tags = try Yojson.Safe.from_string tags_json |> Yojson.Safe.Util.to_list |> List.map Yojson.Safe.Util.to_string with _ -> [] in
  let stage = match Domain.Types.deal_stage_of_string stage_str with Some s -> s | None -> Domain.Types.Lead in
  {
    Domain.Types.id;
    name;
    company_id;
    contact_id;
    stage;
    value;
    currency;
    expected_close_date = Option.map (fun t -> { Domain.Types.time = t; timezone = None }) expected_close_date;
    notes;
    tags;
    created_at = { time = created_at; timezone = None };
    sync = { local_id = id; version = 1; modified_at = { time = created_at; timezone = None }; synced_at = None; deleted = false };
  }

let deal_row_type = T.(t2 string (t2 string (t2 (option string) (t2 (option string) (t2 string (t2 (option float) (t2 string (t2 (option ptime) (t2 (option string) (t2 string ptime))))))))))

let list_deals_query = Q.(T.unit ->* deal_row_type)
  "SELECT id, name, company_id, contact_id, stage, value, currency, expected_close_date, notes, tags, created_at FROM deals WHERE NOT sync_deleted ORDER BY created_at DESC"

let list_deals pool =
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.collect_list list_deals_query ()
  ) in
  match result with
  | Ok rows -> Lwt.return (List.map deal_of_row rows)
  | Error _ -> Lwt.return []

let insert_deal_query = Q.(T.(t2 string (t2 string (t2 (option string) (t2 (option string) (t2 string (t2 (option float) (t2 string (t2 (option ptime) (t2 (option string) (t2 string (t2 ptime (t2 string ptime)))))))))))) ->. T.unit)
  "INSERT INTO deals (id, name, company_id, contact_id, stage, value, currency, expected_close_date, notes, tags, created_at, sync_local_id, sync_version, sync_modified_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 1, ?)"

let create_deal pool ~name ?company_id ?contact_id ?(stage=Domain.Types.Lead) ?value ?(currency="GBP") ?expected_close_date ?notes ?(tags=[]) () =
  let id = new_uuid () in
  let now = now () in
  let tags_json = `List (List.map (fun t -> `String t) tags) |> Yojson.Safe.to_string in
  let stage_str = Domain.Types.deal_stage_to_string stage in
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.exec insert_deal_query (id, (name, (company_id, (contact_id, (stage_str, (value, (currency, (expected_close_date, (notes, (tags_json, (now, (id, now))))))))))))
  ) in
  match result with
  | Ok () -> Lwt.return (Some id)
  | Error _ -> Lwt.return None

let update_deal_query = Q.(T.(t2 string (t2 (option string) (t2 (option string) (t2 string (t2 (option float) (t2 string (t2 (option ptime) (t2 (option string) (t2 string (t2 ptime string)))))))))) ->. T.unit)
  "UPDATE deals SET name = ?, company_id = ?, contact_id = ?, stage = ?, value = ?, currency = ?, expected_close_date = ?, notes = ?, tags = ?, sync_modified_at = ? WHERE id = ?"

let update_deal pool ~id ~name ?company_id ?contact_id ~stage ?value ?(currency="GBP") ?expected_close_date ?notes ?(tags=[]) () =
  let now = now () in
  let tags_json = `List (List.map (fun t -> `String t) tags) |> Yojson.Safe.to_string in
  let stage_str = Domain.Types.deal_stage_to_string stage in
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.exec update_deal_query (name, (company_id, (contact_id, (stage_str, (value, (currency, (expected_close_date, (notes, (tags_json, (now, id))))))))))
  ) in
  match result with
  | Ok () -> Lwt.return true
  | Error _ -> Lwt.return false

let delete_deal_query = Q.(T.(t2 ptime string) ->. T.unit)
  "UPDATE deals SET sync_deleted = TRUE, sync_modified_at = ? WHERE id = ?"

let delete_deal pool ~id =
  let now = now () in
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.exec delete_deal_query (now, id)
  ) in
  match result with
  | Ok () -> Lwt.return true
  | Error _ -> Lwt.return false

let upsert_deal_query = Q.(T.(t2 string (t2 string (t2 (option string) (t2 (option string) (t2 string (t2 (option float) (t2 string (t2 (option ptime) (t2 (option string) (t2 string (t2 ptime (t2 string (t2 ptime bool))))))))))))) ->. T.unit)
  "INSERT INTO deals (id, name, company_id, contact_id, stage, value, currency, expected_close_date, notes, tags, created_at, sync_local_id, sync_modified_at, sync_deleted, sync_version) 
   VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 1)
   ON CONFLICT(id) DO UPDATE SET 
     name = excluded.name, company_id = excluded.company_id, contact_id = excluded.contact_id,
     stage = excluded.stage, value = excluded.value, currency = excluded.currency,
     expected_close_date = excluded.expected_close_date, notes = excluded.notes, tags = excluded.tags,
     sync_modified_at = excluded.sync_modified_at, sync_deleted = excluded.sync_deleted, sync_version = sync_version + 1"

let upsert_deal pool (deal : Domain.Types.deal) =
  let tags_json = `List (List.map (fun t -> `String t) deal.tags) |> Yojson.Safe.to_string in
  let stage_str = Domain.Types.deal_stage_to_string deal.stage in
  let close_date = Option.map (fun (ts : Domain.Types.timestamp) -> ts.time) deal.expected_close_date in
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.exec upsert_deal_query (deal.id, (deal.name, (deal.company_id, (deal.contact_id, (stage_str, (deal.value, (deal.currency, (close_date, (deal.notes, (tags_json, (deal.created_at.time, (deal.sync.local_id, (deal.sync.modified_at.time, deal.sync.deleted)))))))))))))
  ) in
  match result with
  | Ok () -> Lwt.return true
  | Error _ -> Lwt.return false

(* ============================================
   PROJECT QUERIES
   ============================================ *)

let project_of_row (id, (name, (description, (status_str, (tags_json, (parent_id, created_at)))))) =
  let tags = try Yojson.Safe.from_string tags_json |> Yojson.Safe.Util.to_list |> List.map Yojson.Safe.Util.to_string with _ -> [] in
  let status = match status_str with
    | "archived" -> `Archived
    | "someday" -> `Someday
    | _ -> `Active
  in
  {
    id;
    name;
    description;
    status;
    tags;
    parent_id;
    created_at = { time = created_at; timezone = None };
    sync = { local_id = id; version = 1; modified_at = { time = created_at; timezone = None }; synced_at = None; deleted = false };
  }

let project_row_type = T.(t2 string (t2 string (t2 (option string) (t2 string (t2 string (t2 (option string) ptime))))))

let list_projects_query = Q.(T.unit ->* project_row_type)
  "SELECT id, name, description, status, tags, parent_id, created_at FROM projects WHERE NOT sync_deleted ORDER BY name ASC"

let list_projects pool =
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.collect_list list_projects_query ()
  ) in
  match result with
  | Ok rows -> Lwt.return (List.map project_of_row rows)
  | Error _ -> Lwt.return []

let insert_project_query = Q.(T.(t2 string (t2 string (t2 (option string) (t2 string (t2 string (t2 (option string) (t2 ptime (t2 string ptime)))))))) ->. T.unit)
  "INSERT INTO projects (id, name, description, status, tags, parent_id, created_at, sync_local_id, sync_version, sync_modified_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?, 1, ?)"

let create_project pool ~name ?description ?(status="active") ?(tags=[]) ?parent_id () =
  let id = new_uuid () in
  let now = now () in
  let tags_json = `List (List.map (fun t -> `String t) tags) |> Yojson.Safe.to_string in
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.exec insert_project_query (id, (name, (description, (status, (tags_json, (parent_id, (now, (id, now))))))))
  ) in
  match result with
  | Ok () -> Lwt.return (Some id)
  | Error _ -> Lwt.return None

(* ==================== Attachments ==================== *)

let attachment_of_row (id, (filename, (filepath, (mime_type, (size_bytes, (entity_type, (entity_id, created_at))))))) =
  let open Domain.Types in
  {
    id;
    filename;
    filepath;
    mime_type;
    size_bytes;
    entity_type;
    entity_id;
    created_at = { time = created_at; timezone = None };
    sync = { local_id = id; version = 1; modified_at = { time = created_at; timezone = None }; synced_at = None; deleted = false };
  }

let attachment_row_type = T.(t2 string (t2 string (t2 string (t2 (option string) (t2 int64 (t2 string (t2 string ptime)))))))

let list_attachments_for_entity_query = Q.(T.(t2 string string) ->* attachment_row_type)
  "SELECT id, filename, filepath, mime_type, size_bytes, entity_type, entity_id, created_at FROM attachments WHERE entity_type = ? AND entity_id = ? AND NOT sync_deleted ORDER BY created_at DESC"

let list_attachments_for_entity pool ~entity_type ~entity_id =
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.collect_list list_attachments_for_entity_query (entity_type, entity_id)
  ) in
  match result with
  | Ok rows -> Lwt.return (List.map attachment_of_row rows)
  | Error _ -> Lwt.return []

let insert_attachment_query = Q.(T.(t2 string (t2 string (t2 string (t2 (option string) (t2 int64 (t2 string (t2 string (t2 ptime (t2 string ptime))))))))) ->. T.unit)
  "INSERT INTO attachments (id, filename, filepath, mime_type, size_bytes, entity_type, entity_id, created_at, sync_local_id, sync_version, sync_modified_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, 1, ?)"

let create_attachment pool ~filename ~filepath ?mime_type ~size_bytes ~entity_type ~entity_id () =
  let id = new_uuid () in
  let now = now () in
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.exec insert_attachment_query (id, (filename, (filepath, (mime_type, (size_bytes, (entity_type, (entity_id, (now, (id, now)))))))))
  ) in
  match result with
  | Ok () -> Lwt.return (Some id)
  | Error _ -> Lwt.return None

let delete_attachment_query = Q.(T.(t2 ptime string) ->. T.unit)
  "UPDATE attachments SET sync_deleted = true, sync_modified_at = ? WHERE id = ?"

let delete_attachment pool ~id =
  let now = now () in
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.exec delete_attachment_query (now, id)
  ) in
  match result with
  | Ok () -> Lwt.return true
  | Error _ -> Lwt.return false

let permanent_delete_attachment_query = Q.(T.string ->. T.unit)
  "DELETE FROM attachments WHERE id = ?"

let permanent_delete_attachment pool ~id =
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.exec permanent_delete_attachment_query id
  ) in
  match result with
  | Ok () -> Lwt.return true
  | Error _ -> Lwt.return false

(* ============================================
   LINK QUERIES
   ============================================ *)

let link_of_row (id, (source_type, (source_id, (target_type, (target_id, (link_type, (created_at, (local_id, (modified_at, deleted))))))))) =
  let open Domain.Types in
  {
    id;
    source_type;
    source_id;
    target_type;
    target_id;
    link_type;
    created_at = { time = created_at; timezone = None };
    sync = {
      local_id;
      version = 1;
      modified_at = { time = modified_at; timezone = None };
      synced_at = None;
      deleted;
    };
  }

let list_links_for_entity_query = Q.(T.(t2 string (t2 string (t2 string string))) ->* T.(t2 string (t2 string (t2 string (t2 string (t2 string (t2 string (t2 ptime (t2 string (t2 ptime bool))))))))))
  "SELECT id, source_type, source_id, target_type, target_id, link_type, created_at, sync_local_id, sync_modified_at, sync_deleted 
   FROM links WHERE ((source_type = ? AND source_id = ?) OR (target_type = ? AND target_id = ?)) AND NOT sync_deleted"

let list_links_for_entity pool ~entity_type ~entity_id =
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.collect_list list_links_for_entity_query (entity_type, (entity_id, (entity_type, entity_id)))
  ) in
  match result with
  | Ok rows -> Lwt.return (List.map link_of_row rows)
  | Error _ -> Lwt.return []

let insert_link_query = Q.(T.(t2 string (t2 string (t2 string (t2 string (t2 string (t2 string (t2 ptime (t2 string ptime)))))))) ->. T.unit)
  "INSERT INTO links (id, source_type, source_id, target_type, target_id, link_type, created_at, sync_local_id, sync_modified_at, sync_version) 
   VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, 1)"

let create_link pool ~source_type ~source_id ~target_type ~target_id ~link_type =
  let id = new_uuid () in
  let now = now () in
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.exec insert_link_query (id, (source_type, (source_id, (target_type, (target_id, (link_type, (now, (id, now))))))))
  ) in
  match result with
  | Ok () -> Lwt.return (Some id)
  | Error e -> 
    Printf.eprintf "Link creation error: %s\n%!" (Caqti_error.show e);
    Lwt.return None

let delete_link_query = Q.(T.(t2 ptime string) ->. T.unit)
  "UPDATE links SET sync_deleted = true, sync_modified_at = ? WHERE id = ?"

let delete_link pool ~id =
  let now = now () in
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.exec delete_link_query (now, id)
  ) in
  match result with
  | Ok () -> Lwt.return true
  | Error _ -> Lwt.return false

(* List all links including deleted for sync *)
let list_all_links_query = Q.(T.unit ->* T.(t2 string (t2 string (t2 string (t2 string (t2 string (t2 string (t2 ptime (t2 string (t2 ptime bool))))))))))
  "SELECT id, source_type, source_id, target_type, target_id, link_type, created_at, sync_local_id, sync_modified_at, sync_deleted FROM links"

let list_all_links pool =
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.collect_list list_all_links_query ()
  ) in
  match result with
  | Ok rows -> Lwt.return (List.map link_of_row rows)
  | Error _ -> Lwt.return []

(* Upsert link for sync *)
let upsert_link_query = Q.(T.(t2 string (t2 string (t2 string (t2 string (t2 string (t2 string (t2 ptime (t2 string (t2 ptime bool))))))))) ->. T.unit)
  "INSERT INTO links (id, source_type, source_id, target_type, target_id, link_type, created_at, sync_local_id, sync_modified_at, sync_deleted, sync_version) 
   VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 1)
   ON CONFLICT(id) DO UPDATE SET 
     source_type = excluded.source_type, source_id = excluded.source_id,
     target_type = excluded.target_type, target_id = excluded.target_id,
     link_type = excluded.link_type, sync_modified_at = excluded.sync_modified_at, 
     sync_deleted = excluded.sync_deleted, sync_version = sync_version + 1"

let upsert_link pool (link : Domain.Types.link) =
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.exec upsert_link_query (link.id, (link.source_type, (link.source_id, (link.target_type, (link.target_id, (link.link_type, (link.created_at.time, (link.sync.local_id, (link.sync.modified_at.time, link.sync.deleted)))))))))
  ) in
  match result with
  | Ok () -> Lwt.return true
  | Error _ -> Lwt.return false
