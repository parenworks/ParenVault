(** TUI update logic - handles messages and state transitions *)

open Model

(** Update the model based on a message *)
let update model msg =
  match msg with
  (* Navigation *)
  | Navigate view ->
    { model with 
      view;
      previous_views = model.view :: model.previous_views;
      selected_index = 0;
      scroll_offset = 0;
    }
  
  | GoBack ->
    (match model.previous_views with
     | prev :: rest -> 
       { model with view = prev; previous_views = rest; selected_index = 0 }
     | [] -> 
       (* If no previous views, go to Dashboard *)
       { model with view = Dashboard; selected_index = 0 })
  
  | Quit -> model  (* Handled specially in main loop *)
  
  (* Selection *)
  | SelectNext ->
    let max_idx = max 0 (current_items model - 1) in
    let new_idx = min (model.selected_index + 1) max_idx in
    let visible_height = model.height - 6 in
    let new_offset = 
      if new_idx >= model.scroll_offset + visible_height then
        new_idx - visible_height + 1
      else model.scroll_offset
    in
    { model with selected_index = new_idx; scroll_offset = new_offset }
  
  | SelectPrev ->
    let new_idx = max 0 (model.selected_index - 1) in
    let new_offset =
      if new_idx < model.scroll_offset then new_idx
      else model.scroll_offset
    in
    { model with selected_index = new_idx; scroll_offset = new_offset }
  
  | SelectFirst ->
    { model with selected_index = 0; scroll_offset = 0 }
  
  | SelectLast ->
    let max_idx = max 0 (current_items model - 1) in
    let visible_height = model.height - 6 in
    let new_offset = max 0 (max_idx - visible_height + 1) in
    { model with selected_index = max_idx; scroll_offset = new_offset }
  
  | PageDown ->
    let visible_height = model.height - 6 in
    let max_idx = max 0 (current_items model - 1) in
    let new_idx = min (model.selected_index + visible_height) max_idx in
    let new_offset = min (model.scroll_offset + visible_height) (max 0 (max_idx - visible_height + 1)) in
    { model with selected_index = new_idx; scroll_offset = new_offset }
  
  | PageUp ->
    let visible_height = model.height - 6 in
    let new_idx = max 0 (model.selected_index - visible_height) in
    let new_offset = max 0 (model.scroll_offset - visible_height) in
    { model with selected_index = new_idx; scroll_offset = new_offset }
  
  (* Actions *)
  | OpenSelected ->
    (match model.view with
     | TaskList | Inbox | Dashboard ->
       let tasks = match model.view with
         | Inbox -> List.filter (fun (t : Domain.Types.task) -> t.Domain.Types.status = Domain.Types.Inbox) model.tasks
         | _ -> List.filter (fun (t : Domain.Types.task) -> t.parent_id = None) model.tasks  (* Exclude subtasks *)
       in
       (match List.nth_opt tasks model.selected_index with
        | Some task -> 
          { model with 
            view = TaskDetail task.id;
            previous_views = model.view :: model.previous_views;
            subtask_index = 0;  (* Reset subtask selection *)
          }
        | None -> model)
     | NoteList ->
       (match List.nth_opt model.notes model.selected_index with
        | Some note ->
          { model with
            view = NoteDetail note.id;
            previous_views = model.view :: model.previous_views;
          }
        | None -> model)
     | Calendar ->
       (match List.nth_opt model.events model.selected_index with
        | Some event ->
          { model with
            view = EventDetail event.id;
            previous_views = model.view :: model.previous_views;
          }
        | None -> model)
     | ContactList ->
       (match List.nth_opt model.contacts model.selected_index with
        | Some contact ->
          { model with
            view = ContactDetail contact.id;
            previous_views = model.view :: model.previous_views;
          }
        | None -> model)
     | _ -> model)
  
  | QuickCapture ->
    (* Quick capture - minimal form for fast inbox entry *)
    let form = {
      fields = [
        { name = "Title"; value = ""; field_type = `Text };
      ];
      focused_field = 0;
      entity_id = None;
    } in
    { model with 
      view = TaskEdit None;
      previous_views = model.view :: model.previous_views;
      input_mode = Insert;
      form = Some form;
    }
  
  | DailyNote ->
    (* Find or prepare to create today's daily note *)
    let today = Ptime_clock.now () in
    let (y, m, d) = Ptime.to_date today in
    let months = [|"JAN";"FEB";"MAR";"APR";"MAY";"JUN";"JUL";"AUG";"SEP";"OCT";"NOV";"DEC"|] in
    let today_title = Printf.sprintf "%02d-%s-%04d" d months.(m - 1) y in
    (* Check if a note with today's date already exists *)
    (match List.find_opt (fun (n : Domain.Types.note) -> n.title = today_title) model.notes with
     | Some note ->
       (* Open existing daily note *)
       { model with 
         view = NoteDetail note.id;
         previous_views = model.view :: model.previous_views;
       }
     | None ->
       (* Create new daily note with today's date as title *)
       let form = {
         fields = [
           { name = "Title"; value = today_title; field_type = `Text };
           { name = "Content"; value = ""; field_type = `MultiLine };
           { name = "Tags"; value = "daily"; field_type = `Text };
         ];
         focused_field = 1;  (* Focus on Content field *)
         entity_id = None;
       } in
       { model with
         view = NoteEdit None;
         previous_views = model.view :: model.previous_views;
         input_mode = Insert;
         form = Some form;
       })

  | AddSubtask ->
    (* Add a subtask to the currently viewed task *)
    (match model.view with
     | TaskDetail parent_id ->
       let form = {
         fields = [
           { name = "Title"; value = ""; field_type = `Text };
           { name = "Notes"; value = ""; field_type = `MultiLine };
         ];
         focused_field = 0;
         entity_id = Some parent_id;  (* Store parent_id here temporarily *)
       } in
       { model with
         view = TaskEdit None;  (* None means new task *)
         previous_views = model.view :: model.previous_views;
         input_mode = Insert;
         form = Some form;
       }
     | _ -> model)

  | CreateNew ->
    (match model.view with
     | TaskList | Inbox | Dashboard ->
       let form = {
         fields = [
           { name = "Title"; value = ""; field_type = `Text };
           { name = "Notes"; value = ""; field_type = `MultiLine };
           { name = "Priority"; value = "P2"; field_type = `Select ["P0"; "P1"; "P2"; "P3"] };
           { name = "Due Date"; value = ""; field_type = `Date };
           { name = "Scheduled"; value = ""; field_type = `Date };
           { name = "Recurrence"; value = "None"; field_type = `MultiSelect ["Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat"; "Sun"] };
           { name = "Tags"; value = ""; field_type = `Text };
         ];
         focused_field = 0;
         entity_id = None;
       } in
       { model with 
         view = TaskEdit None;
         previous_views = model.view :: model.previous_views;
         input_mode = Insert;
         form = Some form;
       }
     | NoteList ->
       let form = {
         fields = [
           { name = "Title"; value = ""; field_type = `Text };
           { name = "Content"; value = ""; field_type = `MultiLine };
           { name = "Tags"; value = ""; field_type = `Text };
         ];
         focused_field = 0;
         entity_id = None;
       } in
       { model with
         view = NoteEdit None;
         previous_views = model.view :: model.previous_views;
         input_mode = Insert;
         form = Some form;
       }
     | Calendar ->
       let form = {
         fields = [
           { name = "Title"; value = ""; field_type = `Text };
           { name = "Notes"; value = ""; field_type = `MultiLine };
           { name = "Date"; value = ""; field_type = `Date };
           { name = "Time"; value = ""; field_type = `Text };
           { name = "Location"; value = ""; field_type = `Text };
           { name = "Recurrence"; value = "None"; field_type = `MultiSelect ["Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat"; "Sun"] };
         ];
         focused_field = 0;
         entity_id = None;
       } in
       { model with
         view = EventEdit None;
         previous_views = model.view :: model.previous_views;
         input_mode = Insert;
         form = Some form;
       }
     | ContactList ->
       let form = {
         fields = [
           { name = "Name"; value = ""; field_type = `Text };
           { name = "Email"; value = ""; field_type = `Text };
           { name = "Phone"; value = ""; field_type = `Text };
           { name = "Notes"; value = ""; field_type = `MultiLine };
         ];
         focused_field = 0;
         entity_id = None;
       } in
       { model with
         view = ContactEdit None;
         previous_views = model.view :: model.previous_views;
         input_mode = Insert;
         form = Some form;
       }
     | Projects ->
       let form = {
         fields = [
           { name = "Name"; value = ""; field_type = `Text };
           { name = "Description"; value = ""; field_type = `MultiLine };
           { name = "Status"; value = "Active"; field_type = `Select ["Active"; "Someday"; "Archived"] };
         ];
         focused_field = 0;
         entity_id = None;
       } in
       { model with
         view = ProjectEdit None;
         previous_views = model.view :: model.previous_views;
         input_mode = Insert;
         form = Some form;
       }
     | _ -> model)
  
  | EditSelected ->
    (match model.view with
     | TaskDetail id ->
       (match List.find_opt (fun (t : Domain.Types.task) -> t.id = id) model.tasks with
        | Some task ->
          let recurrence_val = Option.value ~default:"None" task.recurrence in
          let months = [|"JAN";"FEB";"MAR";"APR";"MAY";"JUN";"JUL";"AUG";"SEP";"OCT";"NOV";"DEC"|] in
          let format_date_opt ts_opt = match ts_opt with
            | Some ts -> 
              let (y, m, d) = Ptime.to_date ts.Domain.Types.time in
              Printf.sprintf "%02d-%s-%04d" d months.(m - 1) y
            | None -> ""
          in
          let due_date_val = format_date_opt task.due_date in
          let scheduled_val = format_date_opt task.scheduled_date in
          let form = {
            fields = [
              { name = "Title"; value = task.title; field_type = `Text };
              { name = "Notes"; value = Option.value ~default:"" task.description; field_type = `MultiLine };
              { name = "Priority"; value = Domain.Types.priority_to_string task.priority; field_type = `Select ["P0"; "P1"; "P2"; "P3"] };
              { name = "Due Date"; value = due_date_val; field_type = `Date };
              { name = "Scheduled"; value = scheduled_val; field_type = `Date };
              { name = "Recurrence"; value = recurrence_val; field_type = `MultiSelect ["Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat"; "Sun"] };
              { name = "Tags"; value = String.concat ", " task.tags; field_type = `Text };
            ];
            focused_field = 0;
            entity_id = Some id;
          } in
          { model with view = TaskEdit (Some id); input_mode = Insert; form = Some form }
        | None -> model)
     | NoteDetail id ->
       (match List.find_opt (fun (n : Domain.Types.note) -> n.id = id) model.notes with
        | Some note ->
          let form = {
            fields = [
              { name = "Title"; value = note.title; field_type = `Text };
              { name = "Content"; value = note.content; field_type = `MultiLine };
              { name = "Tags"; value = String.concat ", " note.tags; field_type = `Text };
            ];
            focused_field = 0;
            entity_id = Some id;
          } in
          { model with view = NoteEdit (Some id); input_mode = Insert; form = Some form }
        | None -> model)
     | EventDetail id ->
       (match List.find_opt (fun (e : Domain.Types.event) -> e.id = id) model.events with
        | Some event ->
          (* Convert UTC to local time (UTC+3) for form display *)
          let tz_offset_s = Ptime.Span.of_int_s (3 * 3600) in
          let local_time = Ptime.add_span event.start_time.time tz_offset_s |> Option.value ~default:event.start_time.time in
          let (y, m, d) = Ptime.to_date local_time in
          let months = [|"JAN";"FEB";"MAR";"APR";"MAY";"JUN";"JUL";"AUG";"SEP";"OCT";"NOV";"DEC"|] in
          let date_str = Printf.sprintf "%02d-%s-%04d" d months.(m - 1) y in
          let (_, ((h, mi, _), _)) = Ptime.to_date_time local_time in
          let time_str = Printf.sprintf "%02d:%02d" h mi in
          let recurrence_val = Option.value ~default:"None" event.recurrence in
          let form = {
            fields = [
              { name = "Title"; value = event.title; field_type = `Text };
              { name = "Notes"; value = Option.value ~default:"" event.description; field_type = `MultiLine };
              { name = "Date"; value = date_str; field_type = `Date };
              { name = "Time"; value = time_str; field_type = `Text };
              { name = "Location"; value = Option.value ~default:"" event.location; field_type = `Text };
              { name = "Recurrence"; value = recurrence_val; field_type = `MultiSelect ["Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat"; "Sun"] };
            ];
            focused_field = 0;
            entity_id = Some id;
          } in
          { model with view = EventEdit (Some id); input_mode = Insert; form = Some form }
        | None -> model)
     | ContactDetail id ->
       (match List.find_opt (fun (c : Domain.Types.contact) -> c.id = id) model.contacts with
        | Some contact ->
          let form = {
            fields = [
              { name = "Name"; value = contact.name; field_type = `Text };
              { name = "Email"; value = Option.value ~default:"" contact.email; field_type = `Text };
              { name = "Phone"; value = Option.value ~default:"" contact.phone; field_type = `Text };
              { name = "Notes"; value = Option.value ~default:"" contact.notes; field_type = `MultiLine };
            ];
            focused_field = 0;
            entity_id = Some id;
          } in
          { model with view = ContactEdit (Some id); input_mode = Insert; form = Some form }
        | None -> model)
     | _ -> model)
  
  | DeleteSelected ->
    (* Would trigger actual deletion via command *)
    let status = { 
      text = "Delete not yet implemented"; 
      level = `Warning;
      expires_at = None;
    } in
    { model with status = Some status }

  | RestoreSelected ->
    (* Restore is handled in the main loop with DB access *)
    (* This just provides feedback if not in Archive view *)
    (match model.view with
     | Archive -> model  (* Actual restore happens in main loop *)
     | _ -> 
       { model with status = Some { text = "Restore only works in Archive view (press 9)"; level = `Warning; expires_at = None } })
  
  | ToggleTaskStatus ->
    (* Toggle between Done and Todo for the selected task *)
    (match model.view with
     | TaskList | Inbox | Dashboard ->
       let tasks = match model.view with
         | Inbox -> List.filter (fun (t : Domain.Types.task) -> t.status = Domain.Types.Inbox) model.tasks
         | _ -> List.filter (fun (t : Domain.Types.task) -> t.parent_id = None) model.tasks
       in
       (match List.nth_opt tasks model.selected_index with
        | Some task ->
          let new_status = if task.status = Domain.Types.Done then Domain.Types.Todo else Domain.Types.Done in
          let updated_tasks = List.map (fun (t : Domain.Types.task) ->
            if t.id = task.id then { t with status = new_status } else t
          ) model.tasks in
          let status_text = if new_status = Domain.Types.Done then "Task marked as done" else "Task marked as todo" in
          { model with 
            tasks = updated_tasks; 
            status = Some { text = status_text; level = `Success; expires_at = None } 
          }
        | None -> model)
     | _ -> model)

  | ToggleSubtaskStatus subtask_id ->
    (* Toggle subtask status by ID *)
    (match List.find_opt (fun (t : Domain.Types.task) -> t.id = subtask_id) model.tasks with
     | None ->
       { model with status = Some { text = "Task not found: " ^ subtask_id; level = `Error; expires_at = None } }
     | Some subtask ->
       let new_status = if subtask.status = Domain.Types.Done then Domain.Types.Todo else Domain.Types.Done in
       let updated_tasks = List.map (fun (t : Domain.Types.task) ->
         if t.id = subtask_id then { t with status = new_status } else t
       ) model.tasks in
       (* Check if all subtasks of parent are now done - auto-complete parent *)
       let (final_tasks, extra_msg) = match subtask.parent_id with
         | Some parent_id when new_status = Domain.Types.Done ->
           let siblings = List.filter (fun (t : Domain.Types.task) -> t.parent_id = Some parent_id) updated_tasks in
           let all_done = List.for_all (fun (t : Domain.Types.task) -> t.status = Domain.Types.Done) siblings in
           if all_done then
             let tasks_with_parent_done = List.map (fun (t : Domain.Types.task) ->
               if t.id = parent_id then { t with status = Domain.Types.Done } else t
             ) updated_tasks in
             (tasks_with_parent_done, " - Parent task completed!")
           else
             (updated_tasks, "")
         | _ -> (updated_tasks, "")
       in
       let status_text = (if new_status = Domain.Types.Done then "Subtask done ✓" else "Subtask todo") ^ extra_msg in
       { model with 
         tasks = final_tasks; 
         status = Some { text = status_text; level = `Success; expires_at = None } 
       })
  
  | SetTaskPriority _ ->
    model
  
  (* Input mode *)
  | EnterInsertMode ->
    { model with input_mode = Insert }
  
  | ExitInsertMode ->
    { model with input_mode = Normal; input_buffer = "" }
  
  | UpdateInput s ->
    { model with input_buffer = s }
  
  | SubmitInput ->
    { model with input_mode = Normal }
  
  | CancelInput ->
    (match model.previous_views with
     | prev :: rest ->
       { model with view = prev; previous_views = rest; input_mode = Normal; input_buffer = ""; form = None }
     | [] ->
       { model with input_mode = Normal; input_buffer = ""; form = None })
  
  (* Form navigation *)
  | NextField ->
    (match model.form with
     | Some form ->
       let max_idx = List.length form.fields - 1 in
       let new_idx = min (form.focused_field + 1) max_idx in
       { model with form = Some { form with focused_field = new_idx } }
     | None -> model)
  
  | PrevField ->
    (match model.form with
     | Some form ->
       let new_idx = max 0 (form.focused_field - 1) in
       { model with form = Some { form with focused_field = new_idx } }
     | None -> model)
  
  | UpdateFieldValue s ->
    (match model.form with
     | Some form ->
       let fields = List.mapi (fun i field ->
         if i = form.focused_field then { field with value = s }
         else field
       ) form.fields in
       { model with form = Some { form with fields } }
     | None -> model)
  
  (* Data loading *)
  | TasksLoaded tasks ->
    clamp_selection { model with tasks }
  
  | NotesLoaded notes ->
    clamp_selection { model with notes }
  
  | EventsLoaded events ->
    { model with events }
  
  | ProjectsLoaded projects ->
    { model with projects }
  
  | LoadError msg ->
    let status = { text = "Error: " ^ msg; level = `Error; expires_at = None } in
    { model with status = Some status }
  
  (* Sync *)
  | SyncStatusChanged (online, pending) ->
    { model with sync_online = online; sync_pending = pending }
  
  | SyncCompleted time ->
    let status = { text = "Sync completed"; level = `Success; expires_at = None } in
    { model with last_sync = Some time; status = Some status }
  
  | TriggerSync ->
    (* Mark all items as synced (simulated sync - no backend yet) *)
    let now = Ptime_clock.now () in
    let now_ts = { Domain.Types.time = now; timezone = None } in
    let update_sync sync = { sync with Domain.Types.synced_at = Some now_ts } in
    let tasks = List.map (fun (t : Domain.Types.task) -> 
      { t with sync = update_sync t.sync }
    ) model.tasks in
    let notes = List.map (fun (n : Domain.Types.note) -> 
      { n with sync = update_sync n.sync }
    ) model.notes in
    let events = List.map (fun (e : Domain.Types.event) -> 
      { e with sync = update_sync e.sync }
    ) model.events in
    let contacts = List.map (fun (c : Domain.Types.contact) -> 
      { c with sync = update_sync c.sync }
    ) model.contacts in
    let status = { text = "✓ Sync complete"; level = `Success; expires_at = None } in
    { model with tasks; notes; events; contacts; last_sync = Some now; status = Some status }
  
  (* Status *)
  | ShowStatus status ->
    { model with status = Some status }
  
  | ClearStatus ->
    { model with status = None }
  
  (* Search *)
  | StartSearch ->
    { model with 
      view = Search "";
      previous_views = model.view :: model.previous_views;
      input_mode = Insert;
      search_query = "";
      search_results = [];
    }
  
  | UpdateSearch query ->
    (* Perform search across all entities *)
    let query_lower = String.lowercase_ascii query in
    let matches_query s = String.lowercase_ascii s |> fun sl -> 
      String.length query_lower > 0 && 
      (try let _ = Str.search_forward (Str.regexp_string query_lower) sl 0 in true with Not_found -> false)
    in
    let task_results = List.filter (fun (t : Domain.Types.task) -> 
      matches_query t.title || 
      Option.value ~default:"" t.description |> matches_query ||
      List.exists matches_query t.tags
    ) model.tasks |> List.map (fun t -> `Task t) in
    let note_results = List.filter (fun (n : Domain.Types.note) ->
      matches_query n.title || matches_query n.content ||
      List.exists matches_query n.tags
    ) model.notes |> List.map (fun n -> `Note n) in
    let event_results = List.filter (fun (e : Domain.Types.event) ->
      matches_query e.title ||
      Option.value ~default:"" e.description |> matches_query ||
      Option.value ~default:"" e.location |> matches_query
    ) model.events |> List.map (fun e -> `Event e) in
    let contact_results = List.filter (fun (c : Domain.Types.contact) ->
      matches_query c.name ||
      Option.value ~default:"" c.email |> matches_query ||
      Option.value ~default:"" c.phone |> matches_query
    ) model.contacts |> List.map (fun c -> `Contact c) in
    let results = task_results @ note_results @ event_results @ contact_results in
    { model with 
      view = Search query;
      search_query = query;
      search_results = results;
      selected_index = 0;
    }
  
  | ExecuteSearch ->
    (* Open the selected search result *)
    (match List.nth_opt model.search_results model.selected_index with
     | Some (`Task (t : Domain.Types.task)) -> 
       { model with view = TaskDetail t.id; input_mode = Normal; previous_views = model.view :: model.previous_views }
     | Some (`Note (n : Domain.Types.note)) -> 
       { model with view = NoteDetail n.id; input_mode = Normal; previous_views = model.view :: model.previous_views }
     | Some (`Event (e : Domain.Types.event)) -> 
       { model with view = EventDetail e.id; input_mode = Normal; previous_views = model.view :: model.previous_views }
     | Some (`Contact (c : Domain.Types.contact)) -> 
       { model with view = ContactDetail c.id; input_mode = Normal; previous_views = model.view :: model.previous_views }
     | None -> { model with input_mode = Normal })
  
  (* Terminal *)
  | Resize (w, h) ->
    { model with width = w; height = h }
  
  | Tick ->
    (* Check if status message should expire *)
    match model.status with
    | Some s ->
      (match s.expires_at with
       | Some exp when Ptime.is_earlier exp ~than:(Ptime_clock.now ()) ->
         { model with status = None }
       | _ -> model)
    | None -> model

(** Map a key event to a message *)
let key_to_msg model key =
  match model.input_mode with
  | Normal ->
    (match key with
     (* Navigation *)
     | "j" | "down" -> Some SelectNext
     | "k" | "up" -> Some SelectPrev
     | "g" -> Some SelectFirst
     | "G" -> Some SelectLast
     | "ctrl+d" -> Some PageDown
     | "ctrl+u" -> Some PageUp
     | "enter" -> Some OpenSelected
     | "esc" | "backspace" -> Some GoBack
     | "q" -> Some Quit
     
     (* Actions *)
     | "n" -> Some CreateNew
     | "d" -> Some DeleteSelected
     | "x" -> Some ToggleTaskStatus
     | "s" -> Some TriggerSync
     | "i" -> Some EnterInsertMode
     
     (* View switching *)
     | "1" -> Some (Navigate Dashboard)
     | "2" -> Some (Navigate TaskList)
     | "3" -> Some (Navigate NoteList)
     | "4" -> Some (Navigate Calendar)
     | "5" -> Some (Navigate Projects)
     | "0" -> Some (Navigate Inbox)
     
     (* Priority shortcuts *)
     | "!" -> Some (SetTaskPriority Domain.Types.P0)
     | "@" -> Some (SetTaskPriority Domain.Types.P1)
     | "#" -> Some (SetTaskPriority Domain.Types.P2)
     | "$" -> Some (SetTaskPriority Domain.Types.P3)
     
     | _ -> None)
  
  | Insert ->
    (match key with
     | "esc" -> Some ExitInsertMode
     | "ctrl+s" -> Some SubmitInput
     | "ctrl+c" -> Some CancelInput
     | _ -> None)  (* Text input handled separately *)
  
  | Command ->
    (match key with
     | "esc" -> Some ExitInsertMode
     | "enter" -> Some SubmitInput
     | _ -> None)
