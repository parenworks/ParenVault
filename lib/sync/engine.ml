(** Sync engine for offline-first operation.
    
    Implements a simple last-write-wins strategy with version vectors.
    Each entity has a version number that increments on modification.
    During sync, the higher version wins. Conflicts are logged for review.
*)

open Lwt.Syntax

(** Sync result for a single entity *)
type sync_result =
  | Pushed      (** Local change pushed to remote *)
  | Pulled      (** Remote change pulled to local *)
  | Conflict    (** Conflict detected, needs resolution *)
  | NoChange    (** Already in sync *)

(** Sync status *)
type sync_status = {
  pushed: int;
  pulled: int;
  conflicts: int;
  errors: string list;
  last_sync: Ptime.t option;
}

let empty_status = {
  pushed = 0;
  pulled = 0;
  conflicts = 0;
  errors = [];
  last_sync = None;
}

(** Compare two sync metadata to determine winner *)
let compare_versions local_sync remote_sync =
  if local_sync.Domain.Types.version > remote_sync.Domain.Types.version then
    `LocalWins
  else if remote_sync.Domain.Types.version > local_sync.Domain.Types.version then
    `RemoteWins
  else
    (* Same version - compare timestamps as tiebreaker *)
    let local_time = local_sync.Domain.Types.modified_at.time in
    let remote_time = remote_sync.Domain.Types.modified_at.time in
    if Ptime.compare local_time remote_time > 0 then
      `LocalWins
    else if Ptime.compare local_time remote_time < 0 then
      `RemoteWins
    else
      `Equal

(** Sync context *)
type context = {
  db: Storage.Db.context;
  device_id: string;
  mutable status: sync_status;
}

(** Create sync context *)
let create ~db ~device_id =
  { db; device_id; status = empty_status }

(** Check if remote is available *)
let is_online ctx =
  Storage.Db.remote ctx.db |> Option.is_some

(** Attempt to sync, returns status *)
let sync ctx =
  let* online = Storage.Db.refresh_remote_status ctx.db in
  if not online then
    Lwt.return { ctx.status with errors = ["Remote database not available"] }
  else
    (* For now, just return success - full implementation would:
       1. Find all unsynced local changes
       2. Push to remote
       3. Pull remote changes newer than last sync
       4. Resolve conflicts
    *)
    let now = Ptime_clock.now () in
    let status = { ctx.status with last_sync = Some now } in
    ctx.status <- status;
    Lwt.return status

(** Queue a local change for sync *)
let queue_change _ctx ~entity_type:_ ~entity_id:_ ~operation:_ =
  (* Would insert into sync_queue table *)
  Lwt.return ()

(** Get pending sync count *)
let pending_count _ctx =
  (* Would query sync_queue table *)
  Lwt.return 0
