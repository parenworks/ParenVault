(** Task repository - CRUD operations for tasks *)

open Lwt.Syntax
module T = Domain.Types
open T

(** Caqti type for task rows *)
let task_row_type = Caqti_type.(
  t2 
    (t4 string string (option string) string)  (* id, title, description, status *)
    (t2 
      (t4 string string (option string) (option string))  (* priority, tags, due_date, scheduled_date *)
      (t2
        (t4 (option string) (option string) (option string) string)  (* completed_at, parent_id, project_id, created_at *)
        (t4 string int string (option string))  (* sync_local_id, sync_version, sync_modified_at, sync_synced_at *)
      )
    )
)

(** Convert database row to task *)
let task_of_row ((id, title, description, status), 
                 ((priority, tags_json, due_date, scheduled_date),
                  ((completed_at, parent_id, project_id, created_at),
                   (sync_local_id, sync_version, sync_modified_at, sync_synced_at)))) =
  let parse_timestamp s = 
    match Ptime.of_rfc3339 s with
    | Ok (t, tz, _) -> { time = t; timezone = tz }
    | Error _ -> { time = Ptime.epoch; timezone = None }
  in
  let parse_timestamp_opt = Option.map parse_timestamp in
  let tags = 
    try 
      match Yojson.Safe.from_string tags_json with
      | `List l -> List.filter_map (function `String s -> Some s | _ -> None) l
      | _ -> []
    with _ -> []
  in
  {
    id;
    title;
    description;
    status = Option.value ~default:Inbox (task_status_of_string status);
    priority = Option.value ~default:P2 (priority_of_string priority);
    tags;
    due_date = parse_timestamp_opt due_date;
    scheduled_date = parse_timestamp_opt scheduled_date;
    completed_at = parse_timestamp_opt completed_at;
    recurrence = None;
    parent_id;
    project_id;
    block_start = None;
    block_end = None;
    created_at = parse_timestamp created_at;
    sync = {
      local_id = sync_local_id;
      version = sync_version;
      modified_at = parse_timestamp sync_modified_at;
      synced_at = parse_timestamp_opt sync_synced_at;
      deleted = false;
    };
  }

(** Queries - using Caqti 2.x Infix syntax *)
module Q = struct
  open Caqti_request.Infix

  let insert = (task_row_type ->. Caqti_type.unit) {|
    INSERT INTO tasks (
      id, title, description, status, priority, tags, 
      due_date, scheduled_date, completed_at, parent_id, project_id, created_at,
      sync_local_id, sync_version, sync_modified_at, sync_synced_at
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
  |}

  let update = (task_row_type ->. Caqti_type.unit) {|
    UPDATE tasks SET
      title = ?, description = ?, status = ?, priority = ?, tags = ?,
      due_date = ?, scheduled_date = ?, completed_at = ?, parent_id = ?, project_id = ?,
      sync_version = ?, sync_modified_at = ?, sync_synced_at = ?
    WHERE id = ?
  |}

  let find_by_id = (Caqti_type.string ->? task_row_type) {|
    SELECT id, title, description, status, priority, tags,
           due_date, scheduled_date, completed_at, parent_id, project_id, created_at,
           sync_local_id, sync_version, sync_modified_at, sync_synced_at
    FROM tasks WHERE id = ? AND sync_deleted = 0
  |}

  let find_all = (Caqti_type.unit ->* task_row_type) {|
    SELECT id, title, description, status, priority, tags,
           due_date, scheduled_date, completed_at, parent_id, project_id, created_at,
           sync_local_id, sync_version, sync_modified_at, sync_synced_at
    FROM tasks WHERE sync_deleted = 0
    ORDER BY created_at DESC
  |}

  let find_by_status = (Caqti_type.string ->* task_row_type) {|
    SELECT id, title, description, status, priority, tags,
           due_date, scheduled_date, completed_at, parent_id, project_id, created_at,
           sync_local_id, sync_version, sync_modified_at, sync_synced_at
    FROM tasks WHERE status = ? AND sync_deleted = 0
    ORDER BY priority, due_date
  |}

  let find_due_before = (Caqti_type.string ->* task_row_type) {|
    SELECT id, title, description, status, priority, tags,
           due_date, scheduled_date, completed_at, parent_id, project_id, created_at,
           sync_local_id, sync_version, sync_modified_at, sync_synced_at
    FROM tasks 
    WHERE due_date <= ? AND status NOT IN ('done', 'cancelled') AND sync_deleted = 0
    ORDER BY due_date, priority
  |}

  let soft_delete = Caqti_type.(t2 string int ->. unit) {|
    UPDATE tasks SET sync_deleted = 1, sync_version = ?, sync_modified_at = datetime('now')
    WHERE id = ?
  |}

  let find_unsynced = (Caqti_type.unit ->* task_row_type) {|
    SELECT id, title, description, status, priority, tags,
           due_date, scheduled_date, completed_at, parent_id, project_id, created_at,
           sync_local_id, sync_version, sync_modified_at, sync_synced_at
    FROM tasks WHERE sync_synced_at IS NULL OR sync_modified_at > sync_synced_at
  |}
end

(** Repository functions *)

let format_timestamp ts =
  Ptime.to_rfc3339 ?tz_offset_s:ts.timezone ts.time

let format_timestamp_opt = Option.map format_timestamp

let tags_to_json tags =
  `List (List.map (fun t -> `String t) tags) |> Yojson.Safe.to_string

let task_to_row (task : T.task) =
  ((task.id, task.title, task.description, task_status_to_string task.status),
   ((priority_to_string task.priority, tags_to_json task.tags, 
     format_timestamp_opt task.due_date, format_timestamp_opt task.scheduled_date),
    ((format_timestamp_opt task.completed_at, task.parent_id, task.project_id, 
      format_timestamp task.created_at),
     (task.sync.local_id, task.sync.version, 
      format_timestamp task.sync.modified_at, format_timestamp_opt task.sync.synced_at))))

let insert pool task =
  Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.exec Q.insert (task_to_row task)
  )

let find_by_id pool id =
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.find_opt Q.find_by_id id
  ) in
  match result with
  | Ok (Some row) -> Lwt.return (Ok (Some (task_of_row row)))
  | Ok None -> Lwt.return (Ok None)
  | Error e -> Lwt.return (Error e)

let find_all pool =
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.collect_list Q.find_all ()
  ) in
  match result with
  | Ok rows -> Lwt.return (Ok (List.map task_of_row rows))
  | Error e -> Lwt.return (Error e)

let find_by_status pool status =
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.collect_list Q.find_by_status (task_status_to_string status)
  ) in
  match result with
  | Ok rows -> Lwt.return (Ok (List.map task_of_row rows))
  | Error e -> Lwt.return (Error e)

let find_due_before pool timestamp =
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.collect_list Q.find_due_before (format_timestamp timestamp)
  ) in
  match result with
  | Ok rows -> Lwt.return (Ok (List.map task_of_row rows))
  | Error e -> Lwt.return (Error e)

let delete pool id =
  let* existing = find_by_id pool id in
  match existing with
  | Ok (Some task) ->
    let new_version = task.sync.version + 1 in
    Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
      C.exec Q.soft_delete (id, new_version)
    )
  | Ok None -> Lwt.return (Ok ())
  | Error e -> Lwt.return (Error e)

let find_unsynced pool =
  let* result = Db.with_pool pool (fun (module C : Caqti_lwt.CONNECTION) ->
    C.collect_list Q.find_unsynced ()
  ) in
  match result with
  | Ok rows -> Lwt.return (Ok (List.map task_of_row rows))
  | Error e -> Lwt.return (Error e)
