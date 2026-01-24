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

let task_of_row (id, (title, (description, (status_str, (priority_str, (tags_json, (due_date, (scheduled_date, (completed_at, (recurrence, (project_id, (parent_id, created_at)))))))))))) =
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
    created_at = { time = created_at; timezone = None };
    sync = { local_id = id; version = 1; modified_at = { time = created_at; timezone = None }; synced_at = None; deleted = false };
  }

let task_row_type = T.(t2 string (t2 string (t2 (option string) (t2 string (t2 string (t2 string (t2 (option ptime) (t2 (option ptime) (t2 (option ptime) (t2 (option string) (t2 (option string) (t2 (option string) ptime))))))))))))

let list_tasks_query = Q.(T.unit ->* task_row_type)
  "SELECT id, title, description, status, priority, tags, due_date, scheduled_date, completed_at, recurrence, project_id, parent_id, created_at FROM tasks WHERE NOT sync_deleted ORDER BY created_at DESC"

let list_tasks pool =
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.collect_list list_tasks_query ()
  ) in
  match result with
  | Ok rows -> Lwt.return (List.map task_of_row rows)
  | Error _ -> Lwt.return []

let insert_task_query = Q.(T.(t2 string (t2 string (t2 (option string) (t2 string (t2 string (t2 string (t2 (option ptime) (t2 (option ptime) (t2 (option string) (t2 (option string) (t2 (option string) (t2 ptime (t2 string ptime))))))))))))) ->. T.unit)
  "INSERT INTO tasks (id, title, description, status, priority, tags, due_date, scheduled_date, recurrence, project_id, parent_id, created_at, sync_local_id, sync_version, sync_modified_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 1, ?)"

let create_task pool ~title ?description ?(status=Inbox) ?(priority=P2) ?(tags=[]) ?due_date ?scheduled_date ?recurrence ?project_id ?parent_id () =
  let id = new_uuid () in
  let now = now () in
  let status_str = task_status_to_string status in
  let priority_str = priority_to_string priority in
  let tags_json = `List (List.map (fun t -> `String t) tags) |> Yojson.Safe.to_string in
  let due = Option.map (fun (ts : timestamp) -> ts.time) due_date in
  let sched = Option.map (fun (ts : timestamp) -> ts.time) scheduled_date in
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.exec insert_task_query (id, (title, (description, (status_str, (priority_str, (tags_json, (due, (sched, (recurrence, (project_id, (parent_id, (now, (id, now)))))))))))))
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

let update_task_query = Q.(T.(t2 string (t2 (option string) (t2 string (t2 (option string) (t2 (option ptime) (t2 (option ptime) (t2 ptime string))))))) ->. T.unit)
  "UPDATE tasks SET title = ?, description = ?, tags = ?, recurrence = ?, due_date = ?, scheduled_date = ?, sync_modified_at = ? WHERE id = ?"

let update_task pool ~id ~title ?description ?(tags=[]) ?recurrence ?due_date ?scheduled_date () =
  let now = now () in
  let tags_json = `List (List.map (fun t -> `String t) tags) |> Yojson.Safe.to_string in
  let due = Option.map (fun (ts : timestamp) -> ts.time) due_date in
  let sched = Option.map (fun (ts : timestamp) -> ts.time) scheduled_date in
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.exec update_task_query (title, (description, (tags_json, (recurrence, (due, (sched, (now, id)))))))
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
