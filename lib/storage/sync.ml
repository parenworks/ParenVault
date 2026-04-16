(** Bidirectional sync between local SQLite and remote PostgreSQL.
    
    Strategy: Local-wins with timestamp-based conflict resolution.
    - All local items are pushed to remote (local is authoritative)
    - Only items that exist on remote but NOT locally are pulled (new from other devices)
    - Deletions propagate: local deletes push to remote, remote-only deletes stay
*)

open Lwt.Syntax

(** Generic sync helper: local-wins bidirectional sync for any entity type.
    
    [list_active pool]   - returns active (non-deleted) items
    [list_deleted pool]  - returns soft-deleted items (with sync.deleted=true)
    [upsert pool item]   - insert-or-update an item
    [get_id item]        - extract the unique ID from an item
*)
let sync_entity ~list_active ~list_deleted ~upsert ~get_id ~local_pool ~remote_pool =
  (* Gather all local items (active + deleted) *)
  let* local_active = list_active local_pool in
  let* local_deleted_items = list_deleted local_pool in
  let all_local = local_active @ local_deleted_items in
  let local_ids = List.map get_id all_local in
  
  (* Push all local items to remote (local wins) *)
  let* push_errors = Lwt_list.fold_left_s (fun errs item ->
    Lwt.catch
      (fun () ->
        let* ok = upsert remote_pool item in
        if not ok then
          Printf.eprintf "Sync: failed to push item %s to remote\n%!" (get_id item);
        Lwt.return (if ok then errs else errs + 1))
      (fun exn ->
        Printf.eprintf "Sync: exception pushing item %s: %s\n%!" (get_id item) (Printexc.to_string exn);
        Lwt.return (errs + 1))
  ) 0 all_local in
  
  (* Gather all remote items *)
  let* remote_active = list_active remote_pool in
  let* remote_deleted_items = list_deleted remote_pool in
  let all_remote = remote_active @ remote_deleted_items in
  
  (* Pull only items that don't exist locally (new from other devices) *)
  let new_remote = List.filter (fun item ->
    not (List.mem (get_id item) local_ids)
  ) all_remote in
  let* pull_errors = Lwt_list.fold_left_s (fun errs item ->
    Lwt.catch
      (fun () ->
        let* ok = upsert local_pool item in
        if not ok then
          Printf.eprintf "Sync: failed to pull item %s to local\n%!" (get_id item);
        Lwt.return (if ok then errs else errs + 1))
      (fun exn ->
        Printf.eprintf "Sync: exception pulling item %s: %s\n%!" (get_id item) (Printexc.to_string exn);
        Lwt.return (errs + 1))
  ) 0 new_remote in
  
  Lwt.return (push_errors + pull_errors, List.length all_local, List.length new_remote)

(** Sync tasks *)
let sync_tasks ~local_pool ~remote_pool =
  sync_entity
    ~list_active:Queries.list_tasks
    ~list_deleted:Queries.list_deleted_tasks
    ~upsert:Queries.upsert_task
    ~get_id:(fun (t : Domain.Types.task) -> t.id)
    ~local_pool ~remote_pool

(** Sync notes *)
let sync_notes ~local_pool ~remote_pool =
  sync_entity
    ~list_active:Queries.list_notes
    ~list_deleted:Queries.list_deleted_notes
    ~upsert:Queries.upsert_note
    ~get_id:(fun (n : Domain.Types.note) -> n.id)
    ~local_pool ~remote_pool

(** Sync events *)
let sync_events ~local_pool ~remote_pool =
  sync_entity
    ~list_active:Queries.list_events
    ~list_deleted:Queries.list_deleted_events
    ~upsert:Queries.upsert_event
    ~get_id:(fun (e : Domain.Types.event) -> e.id)
    ~local_pool ~remote_pool

(** Sync contacts *)
let sync_contacts ~local_pool ~remote_pool =
  sync_entity
    ~list_active:Queries.list_contacts
    ~list_deleted:Queries.list_deleted_contacts
    ~upsert:Queries.upsert_contact
    ~get_id:(fun (c : Domain.Types.contact) -> c.id)
    ~local_pool ~remote_pool

(** Sync companies *)
let sync_companies ~local_pool ~remote_pool =
  sync_entity
    ~list_active:Queries.list_companies
    ~list_deleted:Queries.list_deleted_companies
    ~upsert:Queries.upsert_company
    ~get_id:(fun (c : Domain.Types.company) -> c.id)
    ~local_pool ~remote_pool

(** Sync deals *)
let sync_deals ~local_pool ~remote_pool =
  sync_entity
    ~list_active:Queries.list_deals
    ~list_deleted:Queries.list_deleted_deals
    ~upsert:Queries.upsert_deal
    ~get_id:(fun (d : Domain.Types.deal) -> d.id)
    ~local_pool ~remote_pool

(** Sync projects *)
let sync_projects ~local_pool ~remote_pool =
  sync_entity
    ~list_active:Queries.list_projects
    ~list_deleted:Queries.list_deleted_projects
    ~upsert:Queries.upsert_project
    ~get_id:(fun (p : Domain.Types.project) -> p.id)
    ~local_pool ~remote_pool

(** Sync activities - uses list_all since activities already read sync metadata *)
let sync_activities ~local_pool ~remote_pool =
  let list_all pool =
    let* all = Queries.list_all_activities pool in
    Lwt.return (List.filter (fun (a : Domain.Types.activity) -> not a.sync.deleted) all)
  in
  let list_deleted pool =
    let* all = Queries.list_all_activities pool in
    Lwt.return (List.filter (fun (a : Domain.Types.activity) -> a.sync.deleted) all)
  in
  sync_entity
    ~list_active:list_all
    ~list_deleted:list_deleted
    ~upsert:Queries.upsert_activity
    ~get_id:(fun (a : Domain.Types.activity) -> a.id)
    ~local_pool ~remote_pool

(** Sync links - uses list_all since links already read sync metadata *)
let sync_links ~local_pool ~remote_pool =
  let list_all pool =
    let* all = Queries.list_all_links pool in
    Lwt.return (List.filter (fun (l : Domain.Types.link) -> not l.sync.deleted) all)
  in
  let list_deleted pool =
    let* all = Queries.list_all_links pool in
    Lwt.return (List.filter (fun (l : Domain.Types.link) -> l.sync.deleted) all)
  in
  sync_entity
    ~list_active:list_all
    ~list_deleted:list_deleted
    ~upsert:Queries.upsert_link
    ~get_id:(fun (l : Domain.Types.link) -> l.id)
    ~local_pool ~remote_pool

(** Sync time entries - uses list_all since time_entries already read sync metadata *)
let sync_time_entries ~local_pool ~remote_pool =
  let list_all pool =
    let* all = Queries.list_all_time_entries pool in
    Lwt.return (List.filter (fun (te : Domain.Types.time_entry) -> not te.sync.deleted) all)
  in
  let list_deleted pool =
    let* all = Queries.list_all_time_entries pool in
    Lwt.return (List.filter (fun (te : Domain.Types.time_entry) -> te.sync.deleted) all)
  in
  sync_entity
    ~list_active:list_all
    ~list_deleted:list_deleted
    ~upsert:Queries.upsert_time_entry
    ~get_id:(fun (te : Domain.Types.time_entry) -> te.id)
    ~local_pool ~remote_pool

(** Run full bidirectional sync across all entity types *)
let run ~local_pool ~remote_pool =
  let* (e1, _, _) = sync_tasks ~local_pool ~remote_pool in
  let* (e2, _, _) = sync_notes ~local_pool ~remote_pool in
  let* (e3, _, _) = sync_events ~local_pool ~remote_pool in
  let* (e4, _, _) = sync_contacts ~local_pool ~remote_pool in
  let* (e5, _, _) = sync_companies ~local_pool ~remote_pool in
  let* (e6, _, _) = sync_deals ~local_pool ~remote_pool in
  let* (e7, _, _) = sync_projects ~local_pool ~remote_pool in
  let* (e8, _, _) = sync_activities ~local_pool ~remote_pool in
  let* (e9, _, _) = sync_links ~local_pool ~remote_pool in
  let* (e10, _, _) = sync_time_entries ~local_pool ~remote_pool in
  let total_errors = e1 + e2 + e3 + e4 + e5 + e6 + e7 + e8 + e9 + e10 in
  if total_errors > 0 then
    Printf.eprintf "Sync completed with %d errors\n%!" total_errors;
  Lwt.return ()
