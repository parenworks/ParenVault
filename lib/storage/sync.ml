(** Bidirectional sync between local SQLite and remote PostgreSQL *)

open Lwt.Syntax

(** Sync a single entity type between local and remote *)
let sync_tasks ~local_pool ~remote_pool =
  (* Get all tasks from both databases *)
  let* local_tasks = Queries.list_tasks local_pool in
  let* remote_tasks = Queries.list_tasks remote_pool in
  
  (* Also get deleted tasks for full sync *)
  let* local_deleted = Queries.list_deleted_tasks local_pool in
  let* remote_deleted = Queries.list_deleted_tasks remote_pool in
  
  let all_local = local_tasks @ local_deleted in
  let all_remote = remote_tasks @ remote_deleted in
  
  (* Build maps by ID *)
  let local_map = List.fold_left (fun acc (t : Domain.Types.task) -> 
    (t.id, t) :: acc
  ) [] all_local in
  let remote_map = List.fold_left (fun acc (t : Domain.Types.task) -> 
    (t.id, t) :: acc
  ) [] all_remote in
  
  (* Find tasks to push to remote (local newer or remote missing) *)
  let* () = Lwt_list.iter_s (fun (id, local_task) ->
    match List.assoc_opt id remote_map with
    | None ->
      (* Task exists locally but not remotely - push to remote *)
      let* _result = Queries.upsert_task remote_pool local_task in
      Lwt.return ()
    | Some remote_task ->
      (* Both exist - compare timestamps *)
      if Ptime.is_later local_task.sync.modified_at.time ~than:remote_task.sync.modified_at.time then
        let* _result = Queries.upsert_task remote_pool local_task in
        Lwt.return ()
      else
        Lwt.return ()
  ) local_map in
  
  (* Find tasks to pull from remote (remote newer or local missing) *)
  let* () = Lwt_list.iter_s (fun (id, remote_task) ->
    match List.assoc_opt id local_map with
    | None ->
      (* Task exists remotely but not locally - pull to local *)
      let* _result = Queries.upsert_task local_pool remote_task in
      Lwt.return ()
    | Some local_task ->
      (* Both exist - compare timestamps *)
      if Ptime.is_later remote_task.sync.modified_at.time ~than:local_task.sync.modified_at.time then
        let* _result = Queries.upsert_task local_pool remote_task in
        Lwt.return ()
      else
        Lwt.return ()
  ) remote_map in
  
  Lwt.return ()

(** Sync notes between local and remote *)
let sync_notes ~local_pool ~remote_pool =
  let* local_notes = Queries.list_notes local_pool in
  let* remote_notes = Queries.list_notes remote_pool in
  let* local_deleted = Queries.list_deleted_notes local_pool in
  let* remote_deleted = Queries.list_deleted_notes remote_pool in
  
  let all_local = local_notes @ local_deleted in
  let all_remote = remote_notes @ remote_deleted in
  
  let local_map = List.fold_left (fun acc (n : Domain.Types.note) -> 
    (n.id, n) :: acc
  ) [] all_local in
  let remote_map = List.fold_left (fun acc (n : Domain.Types.note) -> 
    (n.id, n) :: acc
  ) [] all_remote in
  
  let* () = Lwt_list.iter_s (fun (id, local_note) ->
    match List.assoc_opt id remote_map with
    | None ->
      let* _result = Queries.upsert_note remote_pool local_note in
      Lwt.return ()
    | Some remote_note ->
      if Ptime.is_later local_note.sync.modified_at.time ~than:remote_note.sync.modified_at.time then
        let* _result = Queries.upsert_note remote_pool local_note in
        Lwt.return ()
      else
        Lwt.return ()
  ) local_map in
  
  let* () = Lwt_list.iter_s (fun (id, remote_note) ->
    match List.assoc_opt id local_map with
    | None ->
      let* _result = Queries.upsert_note local_pool remote_note in
      Lwt.return ()
    | Some local_note ->
      if Ptime.is_later remote_note.sync.modified_at.time ~than:local_note.sync.modified_at.time then
        let* _result = Queries.upsert_note local_pool remote_note in
        Lwt.return ()
      else
        Lwt.return ()
  ) remote_map in
  
  Lwt.return ()

(** Sync events between local and remote *)
let sync_events ~local_pool ~remote_pool =
  let* local_events = Queries.list_events local_pool in
  let* remote_events = Queries.list_events remote_pool in
  let* local_deleted = Queries.list_deleted_events local_pool in
  let* remote_deleted = Queries.list_deleted_events remote_pool in
  
  let all_local = local_events @ local_deleted in
  let all_remote = remote_events @ remote_deleted in
  
  let local_map = List.fold_left (fun acc (e : Domain.Types.event) -> 
    (e.id, e) :: acc
  ) [] all_local in
  let remote_map = List.fold_left (fun acc (e : Domain.Types.event) -> 
    (e.id, e) :: acc
  ) [] all_remote in
  
  let* () = Lwt_list.iter_s (fun (id, local_event) ->
    match List.assoc_opt id remote_map with
    | None ->
      let* _result = Queries.upsert_event remote_pool local_event in
      Lwt.return ()
    | Some remote_event ->
      if Ptime.is_later local_event.sync.modified_at.time ~than:remote_event.sync.modified_at.time then
        let* _result = Queries.upsert_event remote_pool local_event in
        Lwt.return ()
      else
        Lwt.return ()
  ) local_map in
  
  let* () = Lwt_list.iter_s (fun (id, remote_event) ->
    match List.assoc_opt id local_map with
    | None ->
      let* _result = Queries.upsert_event local_pool remote_event in
      Lwt.return ()
    | Some local_event ->
      if Ptime.is_later remote_event.sync.modified_at.time ~than:local_event.sync.modified_at.time then
        let* _result = Queries.upsert_event local_pool remote_event in
        Lwt.return ()
      else
        Lwt.return ()
  ) remote_map in
  
  Lwt.return ()

(** Sync companies between local and remote *)
let sync_companies ~local_pool ~remote_pool =
  let* local_companies = Queries.list_companies local_pool in
  let* remote_companies = Queries.list_companies remote_pool in
  
  let local_map = List.fold_left (fun acc (c : Domain.Types.company) -> 
    (c.id, c) :: acc
  ) [] local_companies in
  let remote_map = List.fold_left (fun acc (c : Domain.Types.company) -> 
    (c.id, c) :: acc
  ) [] remote_companies in
  
  let* () = Lwt_list.iter_s (fun (id, local_company) ->
    match List.assoc_opt id remote_map with
    | None ->
      let* _result = Queries.upsert_company remote_pool local_company in
      Lwt.return ()
    | Some remote_company ->
      if Ptime.is_later local_company.sync.modified_at.time ~than:remote_company.sync.modified_at.time then
        let* _result = Queries.upsert_company remote_pool local_company in
        Lwt.return ()
      else
        Lwt.return ()
  ) local_map in
  
  let* () = Lwt_list.iter_s (fun (id, remote_company) ->
    match List.assoc_opt id local_map with
    | None ->
      let* _result = Queries.upsert_company local_pool remote_company in
      Lwt.return ()
    | Some local_company ->
      if Ptime.is_later remote_company.sync.modified_at.time ~than:local_company.sync.modified_at.time then
        let* _result = Queries.upsert_company local_pool remote_company in
        Lwt.return ()
      else
        Lwt.return ()
  ) remote_map in
  
  Lwt.return ()

(** Sync deals between local and remote *)
let sync_deals ~local_pool ~remote_pool =
  let* local_deals = Queries.list_deals local_pool in
  let* remote_deals = Queries.list_deals remote_pool in
  
  let local_map = List.fold_left (fun acc (d : Domain.Types.deal) -> 
    (d.id, d) :: acc
  ) [] local_deals in
  let remote_map = List.fold_left (fun acc (d : Domain.Types.deal) -> 
    (d.id, d) :: acc
  ) [] remote_deals in
  
  let* () = Lwt_list.iter_s (fun (id, local_deal) ->
    match List.assoc_opt id remote_map with
    | None ->
      let* _result = Queries.upsert_deal remote_pool local_deal in
      Lwt.return ()
    | Some remote_deal ->
      if Ptime.is_later local_deal.sync.modified_at.time ~than:remote_deal.sync.modified_at.time then
        let* _result = Queries.upsert_deal remote_pool local_deal in
        Lwt.return ()
      else
        Lwt.return ()
  ) local_map in
  
  let* () = Lwt_list.iter_s (fun (id, remote_deal) ->
    match List.assoc_opt id local_map with
    | None ->
      let* _result = Queries.upsert_deal local_pool remote_deal in
      Lwt.return ()
    | Some local_deal ->
      if Ptime.is_later remote_deal.sync.modified_at.time ~than:local_deal.sync.modified_at.time then
        let* _result = Queries.upsert_deal local_pool remote_deal in
        Lwt.return ()
      else
        Lwt.return ()
  ) remote_map in
  
  Lwt.return ()

(** Sync activities between local and remote *)
let sync_activities ~local_pool ~remote_pool =
  let* local_activities = Queries.list_activities local_pool in
  let* remote_activities = Queries.list_activities remote_pool in
  
  let local_map = List.fold_left (fun acc (a : Domain.Types.activity) -> 
    (a.id, a) :: acc
  ) [] local_activities in
  let remote_map = List.fold_left (fun acc (a : Domain.Types.activity) -> 
    (a.id, a) :: acc
  ) [] remote_activities in
  
  let* () = Lwt_list.iter_s (fun (id, local_activity) ->
    match List.assoc_opt id remote_map with
    | None ->
      let* _result = Queries.upsert_activity remote_pool local_activity in
      Lwt.return ()
    | Some remote_activity ->
      if Ptime.is_later local_activity.sync.modified_at.time ~than:remote_activity.sync.modified_at.time then
        let* _result = Queries.upsert_activity remote_pool local_activity in
        Lwt.return ()
      else
        Lwt.return ()
  ) local_map in
  
  let* () = Lwt_list.iter_s (fun (id, remote_activity) ->
    match List.assoc_opt id local_map with
    | None ->
      let* _result = Queries.upsert_activity local_pool remote_activity in
      Lwt.return ()
    | Some local_activity ->
      if Ptime.is_later remote_activity.sync.modified_at.time ~than:local_activity.sync.modified_at.time then
        let* _result = Queries.upsert_activity local_pool remote_activity in
        Lwt.return ()
      else
        Lwt.return ()
  ) remote_map in
  
  Lwt.return ()

(** Sync links between local and remote *)
let sync_links ~local_pool ~remote_pool =
  let* local_links = Queries.list_all_links local_pool in
  let* remote_links = Queries.list_all_links remote_pool in
  
  let local_map = List.fold_left (fun acc (l : Domain.Types.link) -> 
    (l.id, l) :: acc
  ) [] local_links in
  let remote_map = List.fold_left (fun acc (l : Domain.Types.link) -> 
    (l.id, l) :: acc
  ) [] remote_links in
  
  let* () = Lwt_list.iter_s (fun (id, local_link) ->
    match List.assoc_opt id remote_map with
    | None ->
      let* _result = Queries.upsert_link remote_pool local_link in
      Lwt.return ()
    | Some remote_link ->
      if Ptime.is_later local_link.sync.modified_at.time ~than:remote_link.sync.modified_at.time then
        let* _result = Queries.upsert_link remote_pool local_link in
        Lwt.return ()
      else
        Lwt.return ()
  ) local_map in
  
  let* () = Lwt_list.iter_s (fun (id, remote_link) ->
    match List.assoc_opt id local_map with
    | None ->
      let* _result = Queries.upsert_link local_pool remote_link in
      Lwt.return ()
    | Some local_link ->
      if Ptime.is_later remote_link.sync.modified_at.time ~than:local_link.sync.modified_at.time then
        let* _result = Queries.upsert_link local_pool remote_link in
        Lwt.return ()
      else
        Lwt.return ()
  ) remote_map in
  
  Lwt.return ()

(** Run full bidirectional sync *)
let run ~local_pool ~remote_pool =
  let* () = sync_tasks ~local_pool ~remote_pool in
  let* () = sync_notes ~local_pool ~remote_pool in
  let* () = sync_events ~local_pool ~remote_pool in
  let* () = sync_companies ~local_pool ~remote_pool in
  let* () = sync_deals ~local_pool ~remote_pool in
  let* () = sync_activities ~local_pool ~remote_pool in
  let* () = sync_links ~local_pool ~remote_pool in
  Lwt.return ()
