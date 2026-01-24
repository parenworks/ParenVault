(** TUI application using Notty *)

open Lwt.Syntax
open Notty
open Notty_lwt
open Model

(** Render the header bar *)
let render_header model width =
  let view_name = match model.view with
    | Dashboard -> "Dashboard"
    | TaskList -> "Tasks"
    | TaskDetail _ -> "Task"
    | TaskEdit None -> "New Task"
    | TaskEdit (Some _) -> "Edit Task"
    | NoteList -> "Notes"
    | NoteDetail _ -> "Note"
    | NoteEdit None -> "New Note"
    | NoteEdit (Some _) -> "Edit Note"
    | Calendar -> "Calendar"
    | EventDetail _ -> "Event"
    | EventEdit None -> "New Event"
    | EventEdit (Some _) -> "Edit Event"
    | ContactList -> "Contacts"
    | ContactDetail _ -> "Contact"
    | ContactEdit None -> "New Contact"
    | ContactEdit (Some _) -> "Edit Contact"
    | Projects -> "Projects"
    | ProjectDetail _ -> "Project"
    | ProjectEdit None -> "New Project"
    | ProjectEdit (Some _) -> "Edit Project"
    | Inbox -> "Inbox"
    | Archive -> "Archive"
    | Search q -> "Search: " ^ q
    | LinkPicker _ -> "Link To..."
  in
  let sync_indicator = 
    if model.sync_online then
      I.string A.(fg green) "● Online"
    else
      I.string A.(fg red) "● Offline"
  in
  let pending = 
    if model.sync_pending > 0 then
      I.string A.(fg yellow) (Printf.sprintf " (%d pending)" model.sync_pending)
    else I.empty
  in
  let title = I.string A.(fg white ++ bg blue ++ st bold) " ParenVault " in
  let view_label = I.string A.(fg white) (" │ " ^ view_name ^ " ") in
  let header_content = I.(title <|> view_label <|> sync_indicator <|> pending) in
  let header_line = I.hsnap ~align:`Left width header_content in
  let separator = I.string A.(fg blue) (String.make width '-') in
  I.(header_line <-> separator)

(** Render a task line *)
let render_task_line ~selected ~width ~subtask_progress (task : Domain.Types.task) =
  let open Domain.Types in
  let attr = if selected then A.(fg white ++ bg blue) else A.empty in
  let status_char = match task.Domain.Types.status with
    | Inbox -> "📥"
    | Todo -> "○ "
    | InProgress -> "◐ "
    | Waiting -> "⏸ "
    | Done -> "✓ "
    | Cancelled -> "✗ "
  in
  let priority_attr, priority_str = match task.Domain.Types.priority with
    | P0 -> A.(fg white ++ bg red), " P0 "
    | P1 -> A.(fg black ++ bg yellow), " P1 "
    | P2 -> A.(fg white ++ bg blue), " P2 "
    | P3 -> A.(fg (gray 12)), " P3 "
  in
  let prefix = if selected then " > " else "   " in
  let now = Ptime_clock.now () in
  let today = Ptime.to_date now in
  let (due_str, due_attr) = match task.due_date with
    | Some d -> 
      let due_date = Ptime.to_date d.time in
      let (y, m, day) = due_date in
      let months = [|""; "JAN"; "FEB"; "MAR"; "APR"; "MAY"; "JUN"; "JUL"; "AUG"; "SEP"; "OCT"; "NOV"; "DEC"|] in
      let date_str = Printf.sprintf " 📅 %02d-%s-%04d" day months.(m) y in
      (* Compare dates for color coding *)
      let is_overdue = due_date < today && task.status <> Done && task.status <> Cancelled in
      let is_today = due_date = today in
      let attr = 
        if is_overdue then A.(fg red ++ st bold)
        else if is_today then A.(fg yellow ++ st bold)
        else A.(fg (gray 12))
      in
      (date_str, attr)
    | None -> ("", A.empty)
  in
  let tags_str = match task.tags with
    | [] -> ""
    | ts -> " #" ^ String.concat " #" ts
  in
  (* Sync status indicator *)
  let (sync_char, sync_attr) = match task.sync.synced_at with
    | None -> ("●", A.(fg (gray 8)))  (* Local only - never synced *)
    | Some synced -> 
      if Ptime.is_later task.sync.modified_at.time ~than:synced.time 
      then ("⟳", A.(fg yellow))  (* Pending - modified since last sync *)
      else ("✓", A.(fg green))   (* Synced *)
  in
  (* Subtask progress indicator *)
  let progress_str = match subtask_progress with
    | Some (done_count, total) when total > 0 ->
      let pct = (done_count * 100) / total in
      Printf.sprintf " [%d/%d %d%%]" done_count total pct
    | _ -> ""
  in
  let line = I.(
    string sync_attr sync_char <|>
    string attr prefix <|>
    string attr status_char <|>
    string priority_attr priority_str <|>
    string attr (" " ^ task.title) <|>
    string A.(fg green) progress_str <|>
    string due_attr due_str <|>
    string A.(fg cyan) tags_str
  ) in
  I.hsnap ~align:`Left width line

(** Render a note line *)
let render_note_line ~selected ~width (note : Domain.Types.note) =
  let attr = if selected then A.(fg white ++ bg blue) else A.empty in
  let prefix = if selected then " > " else "   " in
  let tags_str = match note.Domain.Types.tags with
    | [] -> ""
    | ts -> " #" ^ String.concat " #" ts
  in
  (* Sync status indicator *)
  let (sync_char, sync_attr) = match note.sync.synced_at with
    | None -> ("●", A.(fg (gray 8)))  (* Local only *)
    | Some synced -> 
      if Ptime.is_later note.sync.modified_at.time ~than:synced.time 
      then ("⟳", A.(fg yellow))  (* Pending *)
      else ("✓", A.(fg green))   (* Synced *)
  in
  let line = I.(
    string sync_attr sync_char <|>
    string attr prefix <|>
    string attr "📝 " <|>
    string attr note.Domain.Types.title <|>
    string A.(fg cyan) tags_str
  ) in
  I.hsnap ~align:`Left width line

(** Render task list *)
let render_task_list model width height =
  let all_tasks = model.tasks in
  let tasks = match model.view with
    | Inbox -> List.filter (fun (t : Domain.Types.task) -> t.Domain.Types.status = Domain.Types.Inbox) all_tasks
    | _ -> (* Filter out subtasks from main list - only show top-level tasks *)
      List.filter (fun (t : Domain.Types.task) -> t.parent_id = None) all_tasks
  in
  let visible_height = height - 4 in
  let start_idx = model.scroll_offset in
  let visible_tasks = 
    tasks 
    |> List.filteri (fun i _ -> i >= start_idx && i < start_idx + visible_height)
  in
  let lines = List.mapi (fun i (task : Domain.Types.task) ->
    let actual_idx = start_idx + i in
    (* Calculate subtask progress for this task *)
    let subtasks = List.filter (fun (t : Domain.Types.task) -> t.parent_id = Some task.id) all_tasks in
    let subtask_progress = 
      if subtasks = [] then None
      else
        let total = List.length subtasks in
        let done_count = List.filter (fun (st : Domain.Types.task) -> st.Domain.Types.status = Domain.Types.Done) subtasks |> List.length in
        Some (done_count, total)
    in
    render_task_line ~selected:(actual_idx = model.selected_index) ~width ~subtask_progress task
  ) visible_tasks in
  if lines = [] then
    I.string A.(fg (gray 12)) "  No tasks"
  else
    I.vcat lines

(** Render note list *)
let render_note_list model width height =
  let visible_height = height - 4 in
  let start_idx = model.scroll_offset in
  let visible_notes = 
    model.notes 
    |> List.filteri (fun i _ -> i >= start_idx && i < start_idx + visible_height)
  in
  let lines = List.mapi (fun i note ->
    let actual_idx = start_idx + i in
    render_note_line ~selected:(actual_idx = model.selected_index) ~width note
  ) visible_notes in
  if lines = [] then
    I.string A.(fg (gray 12)) "  No notes"
  else
    I.vcat lines

(** Format date as DD-MMM-YYYY *)
let format_date_dmy (y, m, d) =
  let month = [|"JAN";"FEB";"MAR";"APR";"MAY";"JUN";"JUL";"AUG";"SEP";"OCT";"NOV";"DEC"|].(m - 1) in
  Printf.sprintf "%02d-%s-%04d" d month y

(** Render dashboard *)
let render_dashboard model width =
  let open Domain.Types in
  let now = Ptime_clock.now () in
  let today = Ptime.to_date now in
  let today_str = format_date_dmy today in
  
  (* Filter tasks due today *)
  let tasks_due_today = List.filter (fun (t : task) -> 
    match t.due_date with
    | Some d -> Ptime.to_date d.time = today && t.status <> Done && t.status <> Cancelled
    | None -> false
  ) model.tasks in
  
  (* Filter events for today *)
  let events_today = List.filter (fun (e : event) ->
    Ptime.to_date e.start_time.time = today
  ) model.events in
  
  (* Stats *)
  let inbox_count = List.filter (fun (t : task) -> t.status = Inbox) model.tasks |> List.length in
  let in_progress = List.filter (fun (t : task) -> t.status = InProgress) model.tasks |> List.length in
  let overdue = List.filter (fun (t : task) ->
    match t.due_date with
    | Some d -> Ptime.is_earlier d.time ~than:now && t.status <> Done && t.status <> Cancelled
    | None -> false
  ) model.tasks |> List.length in
  
  (* Header with today's date *)
  let date_header = I.string A.(st bold ++ fg cyan) (Printf.sprintf "  📅 Today: %s" today_str) in
  
  (* Today's tasks section *)
  let today_tasks_section = 
    if List.length tasks_due_today = 0 then
      I.string A.(fg (gray 12)) "  No tasks due today"
    else
      let task_lines = List.map (fun (t : task) ->
        let status_char = match t.status with
          | Inbox -> "📥" | Todo -> "○ " | InProgress -> "◐ " 
          | Waiting -> "⏸ " | Done -> "✓ " | Cancelled -> "✗ "
        in
        I.string A.empty (Printf.sprintf "    %s %s" status_char t.title)
      ) tasks_due_today in
      I.vcat task_lines
  in
  
  (* Today's events section *)
  let today_events_section =
    if List.length events_today = 0 then
      I.string A.(fg (gray 12)) "  No events today"
    else
      let event_lines = List.map (fun (e : event) ->
        (* Convert UTC to local time (UTC+3) for display *)
        let tz_offset_s = Ptime.Span.of_int_s (3 * 3600) in
        let local_time = Ptime.add_span e.start_time.time tz_offset_s |> Option.value ~default:e.start_time.time in
        let time_str = Ptime.to_date_time local_time |> fun (_, ((h, m, _), _)) -> Printf.sprintf "%02d:%02d" h m in
        I.string A.empty (Printf.sprintf "    🕐 %s  %s" time_str e.title)
      ) events_today in
      I.vcat event_lines
  in
  
  (* Quick stats row *)
  let stats_line = I.string A.empty (Printf.sprintf "  📥 Inbox: %d  |  🔄 In Progress: %d  |  ⚠️  Overdue: %d  |  👥 Contacts: %d" 
    inbox_count in_progress overdue (List.length model.contacts)) in
  let sep_width = min width 60 in
  let separator = I.string A.(fg (gray 12)) (String.make sep_width '-') in
  
  I.(
    void 0 1 <->
    date_header <->
    void 0 1 <->
    string A.(st bold) "  Tasks Due Today:" <->
    today_tasks_section <->
    void 0 1 <->
    string A.(st bold) "  Events Today:" <->
    today_events_section <->
    void 0 1 <->
    separator <->
    stats_line
  )

(** Render footer/status bar *)
let render_footer model term_width =
  let mode_str = match model.input_mode with
    | Normal -> "NORMAL"
    | Insert -> "INSERT"
    | Command -> "COMMAND"
  in
  let mode = I.string A.(st bold) (Printf.sprintf "[%s]" mode_str) in
  let nav_hint = "1:Dash 2:Tasks 3:Notes 4:Cal 5:Proj 6:Contacts 0:Inbox 9:Archive" in
  let help = match model.view with
    | TaskList | Inbox -> "j/k:nav Enter:open n:new c:capture x:done d:del D:daily | " ^ nav_hint
    | Dashboard -> "j/k:nav Enter:open n:new c:capture D:daily | " ^ nav_hint
    | NoteList -> "j/k:nav Enter:open n:new d:del D:daily | " ^ nav_hint
    | Calendar -> "j/k:nav Enter:open n:new d:del | " ^ nav_hint
    | ContactList -> "j/k:nav Enter:open n:new d:del | " ^ nav_hint
    | Projects -> "j/k:nav Enter:open n:new d:del | " ^ nav_hint
    | Archive -> "j/k:nav r:restore d:delete Esc:back | " ^ nav_hint
    | TaskDetail _ -> "e:edit a:subtask A:attach L:link o:open d:del Esc:back | " ^ nav_hint
    | NoteDetail _ -> "e:edit A:attach L:link o:open d:del Esc:back | " ^ nav_hint
    | EventDetail _ -> "e:edit L:link d:del Esc:back | " ^ nav_hint
    | ContactDetail _ -> "e:edit L:link d:del Esc:back | " ^ nav_hint
    | ProjectDetail _ -> "e:edit L:link d:del Esc:back | " ^ nav_hint
    | TaskEdit _ | NoteEdit _ | EventEdit _ | ContactEdit _ | ProjectEdit _ -> "Tab/↓:next ↑:prev Enter:save Esc:cancel"
    | Search _ -> "↑/↓:nav Enter:open Esc:cancel | type to search"
    | LinkPicker _ -> "j/k:nav Enter:link Esc:cancel"
  in
  let help_img = I.string A.(fg (gray 16)) (" " ^ help) in
  let status_img = match model.status with
    | Some s ->
      let attr = match s.level with
        | `Info -> A.(fg blue)
        | `Success -> A.(fg green)
        | `Warning -> A.(fg yellow)
        | `Error -> A.(fg red)
      in
      I.string attr ("  " ^ s.text)
    | None -> I.empty
  in
  (* Show input buffer in Command mode *)
  let command_input = 
    if model.input_mode = Command then
      I.string A.(fg white ++ st underline) model.input_buffer
    else I.empty
  in
  let separator = I.string A.(fg blue) (String.make term_width '-') in
  let footer_content = I.(mode <|> help_img <|> status_img <|> command_input) in
  I.(separator <-> hsnap ~align:`Left term_width footer_content)

(** Render a form with multiple fields - hacker aesthetic *)
let render_form ~title (form : Model.form_state) term_width =
  let box_width = min 70 (term_width - 4) in
  let h_line = String.concat "" (List.init box_width (fun _ -> "─")) in
  let field_lines = List.mapi (fun i (field : Model.form_field) ->
    let focused = i = form.focused_field in
    let indicator = if focused then "▶" else " " in
    let label_attr = if focused then A.(st bold ++ fg cyan) else A.(fg (gray 14)) in
    let value_attr = if focused then A.(fg white ++ bg (gray 3)) else A.(fg (gray 16)) in
    let border_attr = if focused then A.(fg cyan) else A.(fg (gray 8)) in
    match field.field_type with
    | `MultiLine ->
      (* Multi-line text area with box *)
      let lines = String.split_on_char '\n' field.value in
      let display_lines = if lines = [""] then [""] else lines in
      let min_lines = 4 in
      let padded_lines = 
        if List.length display_lines < min_lines then
          display_lines @ List.init (min_lines - List.length display_lines) (fun _ -> "")
        else display_lines
      in
      let content_width = box_width - 4 in
      let text_lines = List.map (fun line ->
        let padded = if String.length line < content_width 
          then line ^ String.make (content_width - String.length line) ' '
          else String.sub line 0 content_width in
        I.(string border_attr "  │ " <|> string value_attr padded <|> string border_attr " │")
      ) padded_lines in
      let hint = if focused then " (Ctrl+Enter for newline)" else "" in
      let box_h_line = String.concat "" (List.init (content_width + 2) (fun _ -> "─")) in
      I.vcat ([
        I.(string A.empty ("  " ^ indicator ^ " ") <|> string label_attr field.name <|> string A.(fg (gray 10)) hint);
        I.(string border_attr ("  ┌" ^ box_h_line ^ "┐"));
      ] @ text_lines @ [
        I.(string border_attr ("  └" ^ box_h_line ^ "┘"));
      ])
    | `Select _options ->
      let hint = if focused then " ◀ ▶" else "" in
      let value_display = if field.value = "" then "---" else field.value in
      I.(string A.empty ("  " ^ indicator ^ " ") <|> 
         string label_attr (field.name ^ ": ") <|>
         string border_attr "[" <|>
         string value_attr (" " ^ value_display ^ " ") <|>
         string border_attr "]" <|>
         string A.(fg yellow) hint)
    | `MultiSelect _ ->
      let hint = if focused then " (M T W R F S U)" else "" in
      let value_display = if field.value = "" || field.value = "None" then "---" else field.value in
      I.(string A.empty ("  " ^ indicator ^ " ") <|> 
         string label_attr (field.name ^ ": ") <|>
         string border_attr "[" <|>
         string value_attr (" " ^ value_display ^ " ") <|>
         string border_attr "]" <|>
         string A.(fg yellow) hint)
    | `Date ->
      let hint = if focused then " (YYYY-MM-DD)" else "" in
      let value_display = if field.value = "" then "____-__-__" else field.value in
      I.(string A.empty ("  " ^ indicator ^ " ") <|> 
         string label_attr (field.name ^ ": ") <|>
         string border_attr "[" <|>
         string value_attr (" " ^ value_display ^ " ") <|>
         string border_attr "]" <|>
         string A.(fg (gray 10)) hint)
    | `DateTime ->
      let hint = if focused then " (YYYY-MM-DD HH:MM)" else "" in
      let value_display = if field.value = "" then "____-__-__ __:__" else field.value in
      I.(string A.empty ("  " ^ indicator ^ " ") <|> 
         string label_attr (field.name ^ ": ") <|>
         string border_attr "[" <|>
         string value_attr (" " ^ value_display ^ " ") <|>
         string border_attr "]" <|>
         string A.(fg (gray 10)) hint)
    | `Text ->
      let max_len = 50 in
      let value_display = 
        if field.value = "" then String.make (min 20 max_len) '_'
        else if String.length field.value > max_len 
        then String.sub field.value 0 (max_len - 3) ^ "..."
        else field.value ^ String.make (max 0 (20 - String.length field.value)) ' '
      in
      I.(string A.empty ("  " ^ indicator ^ " ") <|> 
         string label_attr (field.name ^ ": ") <|>
         string value_attr value_display)
  ) form.fields in
  let header_line = I.(string A.(fg cyan) ("  ┌" ^ h_line ^ "┐")) in
  let title_line = I.(string A.(fg cyan) "  │ " <|> string A.(st bold ++ fg white) title <|> 
                      string A.empty (String.make (box_width - String.length title - 2) ' ') <|>
                      string A.(fg cyan) " │") in
  let sep_line = I.(string A.(fg cyan) ("  ├" ^ h_line ^ "┤")) in
  let footer_line = I.(string A.(fg cyan) ("  └" ^ h_line ^ "┘")) in
  let help_text = I.(string A.(fg (gray 10)) "  ↑/↓:navigate  Enter:save  Esc:cancel") in
  I.vcat ([header_line; title_line; sep_line] @ 
          [I.void 0 1] @ 
          (List.concat_map (fun f -> [f; I.void 0 0]) field_lines) @
          [I.void 0 1; footer_line; help_text])

(** Main render function *)
let render model (width, height) =
  let header = render_header model width in
  let footer = render_footer model width in
  let content_height = height - 4 in
  let content = match model.view with
    | Dashboard -> render_dashboard model width
    | TaskList | Inbox -> render_task_list model width height
    | NoteList -> render_note_list model width height
    | TaskDetail id ->
      (match List.find_opt (fun (t : Domain.Types.task) -> t.Domain.Types.id = id) model.tasks with
       | Some task ->
         let tags_str = if task.tags = [] then "" else "Tags: " ^ String.concat ", " task.tags in
         let notes_str = Option.value ~default:"" task.description in
         let recurrence_str = match task.recurrence with
           | Some r -> "🔄 " ^ r
           | None -> ""
         in
         let months = [|"JAN";"FEB";"MAR";"APR";"MAY";"JUN";"JUL";"AUG";"SEP";"OCT";"NOV";"DEC"|] in
         let format_date_opt ts_opt = match ts_opt with
           | Some ts -> 
             let (y, m, d) = Ptime.to_date ts.Domain.Types.time in
             Printf.sprintf "%02d-%s-%04d" d months.(m - 1) y
           | None -> ""
         in
         let due_str = format_date_opt task.due_date in
         let sched_str = format_date_opt task.scheduled_date in
         (* Format time block *)
         let format_datetime_opt ts_opt = match ts_opt with
           | Some ts -> 
             let (y, m, d) = Ptime.to_date ts.Domain.Types.time in
             let ((hh, mm, _), _) = Ptime.to_date_time ts.Domain.Types.time in
             Printf.sprintf "%02d-%s-%04d %02d:%02d" d months.(m - 1) y hh mm
           | None -> ""
         in
         let block_start_str = format_datetime_opt task.block_start in
         let block_end_str = format_datetime_opt task.block_end in
         let time_block_str = 
           if block_start_str <> "" && block_end_str <> "" then
             Printf.sprintf "⏰ %s → %s" block_start_str block_end_str
           else if block_start_str <> "" then
             Printf.sprintf "⏰ %s →" block_start_str
           else ""
         in
         (* Find subtasks *)
         let subtasks = List.filter (fun (t : Domain.Types.task) -> t.parent_id = Some id) model.tasks in
         let (subtasks_section, progress_str) = 
           if subtasks = [] then (I.empty, "")
           else
             let total = List.length subtasks in
             let done_count = List.filter (fun (st : Domain.Types.task) -> st.status = Domain.Types.Done) subtasks |> List.length in
             let pct = if total > 0 then (done_count * 100) / total else 0 in
             let progress = Printf.sprintf " [%d/%d - %d%%]" done_count total pct in
             let subtask_lines = List.mapi (fun i (st : Domain.Types.task) ->
               let selected = i = model.subtask_index in
               let status_char = if st.status = Domain.Types.Done then "✓" else "○" in
               let prefix = if selected then "    ▶ " else "      " in
               let attr = if selected then A.(fg white ++ st bold) else A.(fg (gray 14)) in
               I.string attr (Printf.sprintf "%s%s %s" prefix status_char st.title)
             ) subtasks in
             (I.(void 0 1 <-> string A.(st bold ++ fg cyan) ("  Subtasks:" ^ progress ^ " (j/k:nav x:toggle)") <-> vcat subtask_lines), progress)
         in
         let _ = progress_str in (* suppress unused warning *)
         (* Attachments section *)
         let attachments_section = 
           if model.current_attachments = [] then I.empty
           else
             let att_lines = List.mapi (fun i (att : Domain.Types.attachment) ->
               let selected = i = model.attachment_index && model.attachment_index >= 0 in
               let prefix = if selected then "    ▶ " else "      " in
               let attr = if selected then A.(fg white ++ st bold) else A.(fg (gray 14)) in
               let size_str = 
                 if att.size_bytes < 1024L then Printf.sprintf "%Ld B" att.size_bytes
                 else if att.size_bytes < 1048576L then Printf.sprintf "%.1f KB" (Int64.to_float att.size_bytes /. 1024.)
                 else Printf.sprintf "%.1f MB" (Int64.to_float att.size_bytes /. 1048576.)
               in
               I.string attr (Printf.sprintf "%s📎 %s (%s)" prefix att.filename size_str)
             ) model.current_attachments in
             I.(void 0 1 <-> string A.(st bold ++ fg yellow) "  Attachments: (A:add Enter:open d:delete)" <-> vcat att_lines)
         in
         (* Links section *)
         let links_section = 
           if model.current_links = [] then I.empty
           else
             let link_lines = List.mapi (fun i (link : Domain.Types.link) ->
               let selected = i = model.link_index in
               let attr = if selected then A.(fg white ++ bg blue) else A.(fg cyan) in
               let prefix = if selected then "    ▶ " else "      " in
               let (other_type, other_id) = 
                 if link.target_id = id then (link.source_type, link.source_id)
                 else (link.target_type, link.target_id)
               in
               let name = match other_type with
                 | "task" -> (match List.find_opt (fun (t : Domain.Types.task) -> t.id = other_id) model.tasks with Some t -> "📋 " ^ t.title | None -> "📋 (unknown)")
                 | "note" -> (match List.find_opt (fun (n : Domain.Types.note) -> n.id = other_id) model.notes with Some n -> "📝 " ^ n.title | None -> "📝 (unknown)")
                 | "project" -> (match List.find_opt (fun (p : Domain.Types.project) -> p.id = other_id) model.projects with Some p -> "📁 " ^ p.name | None -> "📁 (unknown)")
                 | "event" -> (match List.find_opt (fun (e : Domain.Types.event) -> e.id = other_id) model.events with Some e -> "📅 " ^ e.title | None -> "📅 (unknown)")
                 | "contact" -> (match List.find_opt (fun (c : Domain.Types.contact) -> c.id = other_id) model.contacts with Some c -> "👤 " ^ c.name | None -> "👤 (unknown)")
                 | _ -> "🔗 " ^ other_type
               in
               I.string attr (prefix ^ name)
             ) model.current_links in
             I.(void 0 1 <-> string A.(st bold ++ fg cyan) "  🔗 Links: (L:add)" <-> vcat link_lines)
         in
         I.(
           void 0 1 <->
           string A.(st bold) ("  " ^ task.Domain.Types.title) <->
           void 0 1 <->
           string A.empty ("  Status: " ^ Domain.Types.task_status_to_string task.Domain.Types.status) <->
           string A.empty ("  Priority: " ^ Domain.Types.priority_to_string task.Domain.Types.priority) <->
           (if due_str = "" then empty else string A.empty ("  Due: " ^ due_str)) <->
           (if sched_str = "" then empty else string A.(fg blue) ("  📅 Scheduled: " ^ sched_str)) <->
           (if time_block_str = "" then empty else string A.(fg green) ("  " ^ time_block_str)) <->
           (if recurrence_str = "" then empty else string A.(fg magenta) ("  " ^ recurrence_str)) <->
           (if tags_str = "" then empty else string A.(fg cyan) ("  " ^ tags_str)) <->
           void 0 1 <->
           (if notes_str = "" then string A.(fg (gray 12)) "  (no notes)" else string A.empty ("  " ^ notes_str)) <->
           subtasks_section <->
           attachments_section <->
           links_section
         )
       | None -> I.string A.(fg red) "Task not found")
    | NoteDetail id ->
      (match List.find_opt (fun (n : Domain.Types.note) -> n.Domain.Types.id = id) model.notes with
       | Some note ->
         let attachments_section = 
           if model.current_attachments = [] then I.empty
           else
             let att_lines = List.mapi (fun i (att : Domain.Types.attachment) ->
               let selected = i = model.attachment_index in
               let prefix = if selected then "    ▶ " else "      " in
               let attr = if selected then A.(fg white ++ st bold) else A.(fg (gray 14)) in
               let size_str = 
                 if att.size_bytes < 1024L then Printf.sprintf "%Ld B" att.size_bytes
                 else if att.size_bytes < 1048576L then Printf.sprintf "%.1f KB" (Int64.to_float att.size_bytes /. 1024.)
                 else Printf.sprintf "%.1f MB" (Int64.to_float att.size_bytes /. 1048576.)
               in
               I.string attr (Printf.sprintf "%s📎 %s (%s)" prefix att.filename size_str)
             ) model.current_attachments in
             I.(void 0 1 <-> string A.(st bold ++ fg yellow) "  Attachments: (A:add Enter:open d:delete)" <-> vcat att_lines)
         in
         let links_section = 
           if model.current_links = [] then I.empty
           else
             let link_lines = List.mapi (fun i (link : Domain.Types.link) ->
               let selected = i = model.link_index in
               let attr = if selected then A.(fg white ++ bg blue) else A.(fg cyan) in
               let prefix = if selected then "    ▶ " else "      " in
               let (other_type, other_id) = 
                 if link.target_id = id then (link.source_type, link.source_id)
                 else (link.target_type, link.target_id)
               in
               let name = match other_type with
                 | "task" -> (match List.find_opt (fun (t : Domain.Types.task) -> t.id = other_id) model.tasks with Some t -> "📋 " ^ t.title | None -> "📋 (unknown)")
                 | "note" -> (match List.find_opt (fun (n : Domain.Types.note) -> n.id = other_id) model.notes with Some n -> "📝 " ^ n.title | None -> "📝 (unknown)")
                 | "project" -> (match List.find_opt (fun (p : Domain.Types.project) -> p.id = other_id) model.projects with Some p -> "📁 " ^ p.name | None -> "📁 (unknown)")
                 | "event" -> (match List.find_opt (fun (e : Domain.Types.event) -> e.id = other_id) model.events with Some e -> "📅 " ^ e.title | None -> "📅 (unknown)")
                 | "contact" -> (match List.find_opt (fun (c : Domain.Types.contact) -> c.id = other_id) model.contacts with Some c -> "👤 " ^ c.name | None -> "👤 (unknown)")
                 | _ -> "🔗 " ^ other_type
               in
               I.string attr (prefix ^ name)
             ) model.current_links in
             I.(void 0 1 <-> string A.(st bold ++ fg cyan) "  🔗 Links: (L:add)" <-> vcat link_lines)
         in
         I.(
           void 0 1 <->
           string A.(st bold) note.Domain.Types.title <->
           void 0 1 <->
           string A.empty note.Domain.Types.content <->
           attachments_section <->
           links_section
         )
       | None -> I.string A.(fg red) "Note not found")
    | Calendar ->
      let upcoming_events = model.events |> List.filteri (fun i _ -> i < 10) in
      if List.length upcoming_events = 0 then
        I.(void 0 1 <-> string A.(fg (gray 12)) "  No upcoming events. Press 'n' to create one.")
      else
        let event_lines = List.mapi (fun i (ev : Domain.Types.event) ->
          let selected = i = model.selected_index in
          let attr = if selected then A.(fg white ++ bg blue) else A.empty in
          (* Convert UTC to local time (UTC+3) for display *)
          let tz_offset_s = Ptime.Span.of_int_s (3 * 3600) in
          let local_time = Ptime.add_span ev.start_time.time tz_offset_s |> Option.value ~default:ev.start_time.time in
          let (y, m, d) = Ptime.to_date local_time in
          let months = [|"JAN";"FEB";"MAR";"APR";"MAY";"JUN";"JUL";"AUG";"SEP";"OCT";"NOV";"DEC"|] in
          let date_str = Printf.sprintf "%02d-%s-%04d" d months.(m - 1) y in
          let (_, ((h, mi, _), _)) = Ptime.to_date_time local_time in
          let time_str = Printf.sprintf "%02d:%02d" h mi in
          (* Sync status indicator *)
          let (sync_char, sync_attr) = match ev.sync.synced_at with
            | None -> ("●", A.(fg (gray 8)))  (* Local only *)
            | Some synced -> 
              if Ptime.is_later ev.sync.modified_at.time ~than:synced.time 
              then ("⟳", A.(fg yellow))  (* Pending *)
              else ("✓", A.(fg green))   (* Synced *)
          in
          let prefix = if selected then " > " else "   " in
          I.hsnap ~align:`Left width I.(string sync_attr sync_char <|> string attr (Printf.sprintf "%s%s %s  %s" prefix date_str time_str ev.title))
        ) upcoming_events in
        I.(void 0 1 <-> string A.(st bold) "  Upcoming Events" <-> void 0 1 <-> vcat event_lines)
    | EventDetail id ->
      (match List.find_opt (fun (e : Domain.Types.event) -> e.id = id) model.events with
       | Some event ->
         (* Convert UTC to local time (UTC+3) for display *)
         let tz_offset_s = Ptime.Span.of_int_s (3 * 3600) in
         let local_time = Ptime.add_span event.start_time.time tz_offset_s |> Option.value ~default:event.start_time.time in
         let (y, m, d) = Ptime.to_date local_time in
         let months = [|"JAN";"FEB";"MAR";"APR";"MAY";"JUN";"JUL";"AUG";"SEP";"OCT";"NOV";"DEC"|] in
         let date_str = Printf.sprintf "%02d-%s-%04d" d months.(m - 1) y in
         let (_, ((h, mi, _), _)) = Ptime.to_date_time local_time in
         let time_str = Printf.sprintf "%02d:%02d" h mi in
         I.(
           void 0 1 <->
           string A.(st bold) ("  " ^ event.title) <->
           void 0 1 <->
           string A.empty ("  Date: " ^ date_str) <->
           string A.empty ("  Time: " ^ time_str) <->
           string A.empty ("  Location: " ^ Option.value ~default:"(none)" event.location) <->
           (match event.recurrence with
            | Some r -> string A.(fg magenta) ("  🔄 " ^ r)
            | None -> empty) <->
           void 0 1 <->
           string A.empty ("  " ^ Option.value ~default:"" event.description)
         )
       | None -> I.string A.(fg red) "  Event not found")
    | EventEdit id_opt ->
      (match model.form with
       | Some form -> 
         let title = if id_opt = None then "New Event" else "Edit Event" in
         render_form ~title form width
       | None -> I.string A.(fg red) "  Form not initialized")
    | ContactList ->
      if List.length model.contacts = 0 then
        I.(void 0 1 <-> string A.(fg (gray 12)) "  No contacts. Press 'n' to add one.")
      else
        let contact_lines = List.mapi (fun i (c : Domain.Types.contact) ->
          let selected = i = model.selected_index in
          let attr = if selected then A.(fg white ++ bg blue) else A.empty in
          let email_str = Option.value ~default:"" c.email in
          let phone_str = Option.value ~default:"" c.phone in
          let info = if email_str <> "" then email_str else phone_str in
          I.hsnap ~align:`Left width (I.string attr (Printf.sprintf "  %-20s  %s" c.name info))
        ) model.contacts in
        I.(void 0 1 <-> vcat contact_lines)
    | ContactDetail id ->
      (match List.find_opt (fun (c : Domain.Types.contact) -> c.id = id) model.contacts with
       | Some contact ->
         I.(
           void 0 1 <->
           string A.(st bold) contact.name <->
           void 0 1 <->
           string A.empty ("Email: " ^ Option.value ~default:"(none)" contact.email) <->
           string A.empty ("Phone: " ^ Option.value ~default:"(none)" contact.phone) <->
           void 0 1 <->
           string A.empty (Option.value ~default:"" contact.notes)
         )
       | None -> I.string A.(fg red) "Contact not found")
    | ContactEdit id_opt ->
      (match model.form with
       | Some form -> 
         let title = if id_opt = None then "New Contact" else "Edit Contact" in
         render_form ~title form width
       | None -> I.string A.(fg red) "  Form not initialized")
    | Projects ->
      if List.length model.projects = 0 then
        I.(void 0 1 <-> string A.(fg (gray 12)) "  No projects. Press 'n' to create one.")
      else
        let project_lines = List.mapi (fun i (p : Domain.Types.project) ->
          let selected = i = model.selected_index in
          let attr = if selected then A.(fg white ++ bg blue) else A.empty in
          let status_str = match p.status with
            | `Active -> "[Active]"
            | `Someday -> "[Someday]"
            | `Archived -> "[Archived]"
          in
          I.string attr (Printf.sprintf "  %s %s" p.name status_str)
        ) model.projects in
        I.vcat (I.void 0 1 :: project_lines)
    | ProjectDetail id ->
      (match List.find_opt (fun (p : Domain.Types.project) -> p.id = id) model.projects with
       | Some project ->
         let status_str = match project.status with
           | `Active -> "Active"
           | `Someday -> "Someday"
           | `Archived -> "Archived"
         in
         (* Display linked items *)
         let link_lines = if model.current_links = [] then
           [I.string A.(fg (gray 12)) "    No linked items"]
         else
           List.mapi (fun i (link : Domain.Types.link) ->
             let selected = i = model.link_index in
             let attr = if selected then A.(fg white ++ bg blue) else A.(fg cyan) in
             let prefix = if selected then "  ▶ " else "    " in
             (* Find the linked entity name *)
             let (other_type, other_id) = 
               if link.target_id = id then (link.source_type, link.source_id)
               else (link.target_type, link.target_id)
             in
             let name = match other_type with
               | "task" -> 
                 (match List.find_opt (fun (t : Domain.Types.task) -> t.id = other_id) model.tasks with
                  | Some t -> "📋 " ^ t.title | None -> "📋 (unknown task)")
               | "note" ->
                 (match List.find_opt (fun (n : Domain.Types.note) -> n.id = other_id) model.notes with
                  | Some n -> "📝 " ^ n.title | None -> "📝 (unknown note)")
               | "event" ->
                 (match List.find_opt (fun (e : Domain.Types.event) -> e.id = other_id) model.events with
                  | Some e -> "📅 " ^ e.title | None -> "📅 (unknown event)")
               | "contact" ->
                 (match List.find_opt (fun (c : Domain.Types.contact) -> c.id = other_id) model.contacts with
                  | Some c -> "👤 " ^ c.name | None -> "👤 (unknown contact)")
               | _ -> "🔗 " ^ other_type
             in
             I.string attr (prefix ^ name)
           ) model.current_links
         in
         I.(
           void 0 1 <->
           string A.(st bold) ("  " ^ project.name) <->
           void 0 1 <->
           string A.empty ("  Status: " ^ status_str) <->
           void 0 1 <->
           string A.empty ("  " ^ Option.value ~default:"" project.description) <->
           void 0 1 <->
           string A.(st bold ++ fg cyan) "  🔗 Linked Items:" <->
           vcat link_lines
         )
       | None -> I.string A.(fg red) "  Project not found")
    | ProjectEdit id_opt ->
      (match model.form with
       | Some form -> 
         let title = if id_opt = None then "New Project" else "Edit Project" in
         render_form ~title form width
       | None -> I.string A.(fg red) "  Form not initialized")
    | TaskEdit id_opt ->
      (match model.form with
       | Some form -> 
         let title = if id_opt = None then "New Task" else "Edit Task" in
         render_form ~title form width
       | None -> I.string A.(fg red) "  Form not initialized")
    | NoteEdit id_opt ->
      (match model.form with
       | Some form -> 
         let title = if id_opt = None then "New Note" else "Edit Note" in
         render_form ~title form width
       | None -> I.string A.(fg red) "  Form not initialized")
    | Archive ->
      (* Render archived/deleted items *)
      let all_archived = 
        List.map (fun t -> `Task t) model.archived_tasks @
        List.map (fun n -> `Note n) model.archived_notes @
        List.map (fun e -> `Event e) model.archived_events
      in
      if all_archived = [] then
        I.(void 0 1 <-> string A.(fg (gray 12)) "  No archived items")
      else
        let lines = List.mapi (fun i item ->
          let selected = i = model.selected_index in
          let attr = if selected then A.(fg white ++ bg blue) else A.empty in
          let prefix = if selected then " > " else "   " in
          let (icon, title) = match item with
            | `Task (t : Domain.Types.task) -> ("📋 ", t.title)
            | `Note (n : Domain.Types.note) -> ("📝 ", n.title)
            | `Event (e : Domain.Types.event) -> ("📅 ", e.title)
          in
          I.string attr (prefix ^ icon ^ title)
        ) all_archived in
        I.(void 0 1 <-> string A.(fg (gray 12)) "  Press 'r' to restore selected item" <-> void 0 1 <-> vcat lines)
    | Search _ ->
      let search_line = I.(
        void 0 1 <->
        string A.(st bold) "  Search: " <|> string A.(st underline ++ fg white) model.search_query <|>
        string A.(fg (gray 12)) (if model.search_query = "" then " (type to search)" else "")
      ) in
      let result_count = I.string A.(fg (gray 12)) (Printf.sprintf "  %d results" (List.length model.search_results)) in
      let result_lines = List.mapi (fun i result ->
        let selected = i = model.selected_index in
        let attr = if selected then A.(fg white ++ bg blue) else A.empty in
        let prefix = if selected then " > " else "   " in
        let (icon, title) : (string * string) = match result with
          | `Task (t : Domain.Types.task) -> ("📋 ", t.title)
          | `Note (n : Domain.Types.note) -> ("📝 ", n.title)
          | `Event (e : Domain.Types.event) -> ("📅 ", e.title)
          | `Contact (c : Domain.Types.contact) -> ("👤 ", c.name)
        in
        I.string attr (prefix ^ icon ^ title)
      ) model.search_results in
      I.vcat ([search_line; result_count; I.void 0 1] @ result_lines)
    | LinkPicker (source_type, _source_id) ->
      (* Show list of linkable items - notes, projects, contacts, other tasks *)
      let header_line = I.(void 0 1 <-> string A.(st bold ++ fg cyan) "  🔗 Link to..." <-> void 0 1) in
      let type_hint = I.string A.(fg (gray 12)) "  Press: n=Notes  p=Projects  c=Contacts  t=Tasks  Esc=Cancel" in
      (* Build list of items based on what we're linking from *)
      let items = 
        List.mapi (fun i (n : Domain.Types.note) -> (i, "note", n.id, "📝 " ^ n.title)) model.notes @
        List.mapi (fun i (p : Domain.Types.project) -> (i + List.length model.notes, "project", p.id, "📁 " ^ p.name)) model.projects @
        List.mapi (fun i (c : Domain.Types.contact) -> (i + List.length model.notes + List.length model.projects, "contact", c.id, "👤 " ^ c.name)) model.contacts @
        (if source_type <> "task" then 
          List.mapi (fun i (t : Domain.Types.task) -> (i + List.length model.notes + List.length model.projects + List.length model.contacts, "task", t.id, "📋 " ^ t.title)) model.tasks
        else [])
      in
      let item_lines = List.map (fun (i, _typ, _id, title) ->
        let selected = i = model.selected_index in
        let attr = if selected then A.(fg white ++ bg blue) else A.empty in
        let prefix = if selected then " ▶ " else "   " in
        I.string attr (prefix ^ title)
      ) items in
      I.vcat ([header_line; type_hint; I.void 0 1] @ item_lines)
  in
  let content_area = I.vsnap ~align:`Top content_height content in
  I.(header <-> content_area <-> footer)

(** Handle keyboard input *)
let handle_key model key =
  let open Update in
  match key with
  | `ASCII 'q' when model.input_mode = Normal -> `Quit
  | `Escape when model.input_mode = Normal -> 
    `Continue (update model GoBack)
  | `Escape -> 
    `Continue (update model ExitInsertMode)
  | `ASCII 'j' | `Arrow `Down when model.input_mode = Normal -> 
    (match model.view with
     | LinkPicker (source_type, _) ->
       (* Navigate linkable items *)
       let num_items = 
         List.length model.notes + List.length model.projects + List.length model.contacts +
         (if source_type <> "task" then List.length model.tasks else 0)
       in
       let new_idx = min (model.selected_index + 1) (num_items - 1) in
       `Continue { model with selected_index = max 0 new_idx }
     | TaskDetail id ->
       (* Navigate: subtasks first, then attachments *)
       let subtasks = List.filter (fun (t : Domain.Types.task) -> t.parent_id = Some id) model.tasks in
       let num_subtasks = List.length subtasks in
       let num_attachments = List.length model.current_attachments in
       (* Combined navigation: subtask_index 0..n-1 for subtasks, then attachment_index for attachments *)
       if num_subtasks > 0 && model.subtask_index < num_subtasks - 1 then
         (* Still navigating subtasks *)
         `Continue { model with subtask_index = model.subtask_index + 1 }
       else if num_subtasks > 0 && model.subtask_index = num_subtasks - 1 && num_attachments > 0 && model.attachment_index = -1 then
         (* Move from last subtask to first attachment *)
         `Continue { model with attachment_index = 0 }
       else if num_attachments > 0 && model.attachment_index >= 0 && model.attachment_index < num_attachments - 1 then
         (* Navigate attachments *)
         `Continue { model with attachment_index = model.attachment_index + 1 }
       else if num_subtasks = 0 && num_attachments > 0 then
         (* No subtasks, just attachments *)
         let new_idx = min (model.attachment_index + 1) (num_attachments - 1) in
         `Continue { model with attachment_index = max 0 new_idx }
       else
         `Continue model
     | NoteDetail _ ->
       (* Navigate attachments in note detail *)
       if model.current_attachments <> [] then
         let max_idx = max 0 (List.length model.current_attachments - 1) in
         let new_idx = min (model.attachment_index + 1) max_idx in
         `Continue { model with attachment_index = max 0 new_idx }
       else
         `Continue model
     | ProjectDetail _ ->
       (* Navigate links in project detail *)
       if model.current_links <> [] then
         let max_idx = max 0 (List.length model.current_links - 1) in
         let new_idx = min (model.link_index + 1) max_idx in
         `Continue { model with link_index = new_idx }
       else
         `Continue model
     | _ -> `Continue (update model SelectNext))
  | `ASCII 'k' | `Arrow `Up when model.input_mode = Normal -> 
    (match model.view with
     | LinkPicker _ ->
       (* Navigate linkable items *)
       let new_idx = max 0 (model.selected_index - 1) in
       `Continue { model with selected_index = new_idx }
     | TaskDetail id ->
       (* Navigate: attachments first (going up), then subtasks *)
       let subtasks = List.filter (fun (t : Domain.Types.task) -> t.parent_id = Some id) model.tasks in
       let num_subtasks = List.length subtasks in
       let num_attachments = List.length model.current_attachments in
       if num_attachments > 0 && model.attachment_index > 0 then
         (* Still navigating attachments *)
         `Continue { model with attachment_index = model.attachment_index - 1 }
       else if num_attachments > 0 && model.attachment_index = 0 && num_subtasks > 0 then
         (* Move from first attachment back to last subtask *)
         `Continue { model with attachment_index = -1; subtask_index = num_subtasks - 1 }
       else if num_subtasks > 0 && model.attachment_index < 0 && model.subtask_index > 0 then
         (* Navigate subtasks *)
         `Continue { model with subtask_index = model.subtask_index - 1 }
       else if num_subtasks = 0 && num_attachments > 0 then
         (* No subtasks, just attachments *)
         let new_idx = max 0 (model.attachment_index - 1) in
         `Continue { model with attachment_index = new_idx }
       else
         `Continue model
     | NoteDetail _ ->
       (* Navigate attachments in note detail *)
       if model.current_attachments <> [] then
         let new_idx = max 0 (model.attachment_index - 1) in
         `Continue { model with attachment_index = new_idx }
       else
         `Continue model
     | ProjectDetail _ ->
       (* Navigate links in project detail *)
       if model.current_links <> [] then
         let new_idx = max 0 (model.link_index - 1) in
         `Continue { model with link_index = new_idx }
       else
         `Continue model
     | _ -> `Continue (update model SelectPrev))
  | `ASCII 'g' when model.input_mode = Normal -> 
    `Continue (update model SelectFirst)
  | `ASCII 'G' when model.input_mode = Normal -> 
    `Continue (update model SelectLast)
  | `Enter when model.input_mode = Normal -> 
    (match model.view with
     | TaskDetail parent_id ->
       (* Open selected subtask *)
       let subtasks = List.filter (fun (t : Domain.Types.task) -> t.parent_id = Some parent_id) model.tasks in
       (match List.nth_opt subtasks model.subtask_index with
        | Some subtask -> 
          `Continue { model with 
            view = TaskDetail subtask.id;
            previous_views = model.view :: model.previous_views;
            subtask_index = 0;
          }
        | None -> `Continue model)
     | ProjectDetail id ->
       (* Open selected linked item *)
       (match List.nth_opt model.current_links model.link_index with
        | Some link ->
          let (other_type, other_id) = 
            if link.target_id = id then (link.source_type, link.source_id)
            else (link.target_type, link.target_id)
          in
          let new_view = match other_type with
            | "task" -> Some (TaskDetail other_id)
            | "note" -> Some (NoteDetail other_id)
            | "event" -> Some (EventDetail other_id)
            | "contact" -> Some (ContactDetail other_id)
            | "project" -> Some (ProjectDetail other_id)
            | _ -> None
          in
          (match new_view with
           | Some v -> `Continue { model with view = v; previous_views = model.view :: model.previous_views }
           | None -> `Continue model)
        | None -> `Continue model)
     | _ -> `Continue (update model OpenSelected))
  | `ASCII 'n' when model.input_mode = Normal -> 
    `Continue (update model CreateNew)
  | `ASCII 'c' when model.input_mode = Normal -> 
    `Continue (update model QuickCapture)
  | `ASCII 'e' when model.input_mode = Normal -> 
    `Continue (update model EditSelected)
  | `ASCII 'a' when model.input_mode = Normal -> 
    `Continue (update model AddSubtask)
  | `ASCII 'd' when model.input_mode = Normal -> 
    `Continue (update model DeleteSelected)
  | `ASCII 'D' when model.input_mode = Normal -> 
    `Continue (update model DailyNote)
  | `ASCII 'L' when model.input_mode = Normal ->
    (* Open link picker from detail views *)
    (match model.view with
     | TaskDetail id -> 
       `Continue { model with view = LinkPicker ("task", id); selected_index = 0; previous_views = model.view :: model.previous_views }
     | NoteDetail id -> 
       `Continue { model with view = LinkPicker ("note", id); selected_index = 0; previous_views = model.view :: model.previous_views }
     | EventDetail id -> 
       `Continue { model with view = LinkPicker ("event", id); selected_index = 0; previous_views = model.view :: model.previous_views }
     | ContactDetail id -> 
       `Continue { model with view = LinkPicker ("contact", id); selected_index = 0; previous_views = model.view :: model.previous_views }
     | ProjectDetail id -> 
       `Continue { model with view = LinkPicker ("project", id); selected_index = 0; previous_views = model.view :: model.previous_views }
     | _ -> `Continue model)
  | `ASCII 'x' when model.input_mode = Normal -> 
    (match model.view with
     | TaskDetail task_id ->
       (* Check if this task has subtasks *)
       let subtasks = List.filter (fun (t : Domain.Types.task) -> t.parent_id = Some task_id) model.tasks in
       if subtasks <> [] then
         (* Toggle selected subtask status *)
         (match List.nth_opt subtasks model.subtask_index with
          | Some subtask -> `Continue (update model (ToggleSubtaskStatus subtask.id))
          | None -> `Continue model)
       else
         (* This task has no subtasks - toggle its own status (it might be a subtask itself) *)
         `Continue (update model (ToggleSubtaskStatus task_id))
     | _ -> `Continue (update model ToggleTaskStatus))
  | `ASCII 's' when model.input_mode = Normal -> 
    `Continue (update model TriggerSync)
  | `ASCII '/' when model.input_mode = Normal -> 
    `Continue (update model StartSearch)
  | `ASCII '1' when model.input_mode = Normal -> 
    `Continue (update model (Navigate Dashboard))
  | `ASCII '2' when model.input_mode = Normal -> 
    `Continue (update model (Navigate TaskList))
  | `ASCII '3' when model.input_mode = Normal -> 
    `Continue (update model (Navigate NoteList))
  | `ASCII '4' when model.input_mode = Normal -> 
    `Continue (update model (Navigate Calendar))
  | `ASCII '5' when model.input_mode = Normal -> 
    `Continue (update model (Navigate Projects))
  | `ASCII '6' when model.input_mode = Normal -> 
    `Continue (update model (Navigate ContactList))
  | `ASCII '0' when model.input_mode = Normal -> 
    `Continue (update model (Navigate Inbox))
  | `ASCII '9' when model.input_mode = Normal -> 
    `Continue (update model (Navigate Archive))
  | `ASCII 'r' when model.input_mode = Normal -> 
    `Continue (update model RestoreSelected)
  | `Page `Down -> 
    `Continue (update model PageDown)
  | `Page `Up -> 
    `Continue (update model PageUp)
  (* Insert mode: form field navigation *)
  | `Tab when model.input_mode = Insert ->
    `Continue (update model NextField)
  | `Arrow `Down when model.input_mode = Insert ->
    (match model.view with
     | Search _ -> 
       let max_idx = max 0 (List.length model.search_results - 1) in
       let new_idx = min (model.selected_index + 1) max_idx in
       `Continue { model with selected_index = new_idx }
     | _ -> `Continue (update model NextField))
  | `Arrow `Up when model.input_mode = Insert ->
    (match model.view with
     | Search _ -> 
       let new_idx = max 0 (model.selected_index - 1) in
       `Continue { model with selected_index = new_idx }
     | _ -> `Continue (update model PrevField))
  (* Arrow left/right for Select fields *)
  | `Arrow `Left when model.input_mode = Insert ->
    (match model.form with
     | Some form ->
       let field = List.nth form.fields form.focused_field in
       (match field.field_type with
        | `Select options ->
          let current_idx = match List.find_index (fun o -> o = field.value) options with
            | Some i -> i | None -> 0
          in
          let new_idx = max 0 (current_idx - 1) in
          let new_value = List.nth options new_idx in
          `Continue (update model (UpdateFieldValue new_value))
        | _ -> `Continue model)
     | None -> `Continue model)
  | `Arrow `Right when model.input_mode = Insert ->
    (match model.form with
     | Some form ->
       let field = List.nth form.fields form.focused_field in
       (match field.field_type with
        | `Select options ->
          let current_idx = match List.find_index (fun o -> o = field.value) options with
            | Some i -> i | None -> 0
          in
          let new_idx = min (List.length options - 1) (current_idx + 1) in
          let new_value = List.nth options new_idx in
          `Continue (update model (UpdateFieldValue new_value))
        | _ -> `Continue model)
     | None -> `Continue model)
  (* Insert mode: text input for form fields *)
  | `ASCII c when model.input_mode = Insert ->
    (match model.view, model.form with
     | Search _, _ ->
       (* In search mode, update search query *)
       let new_query = model.search_query ^ String.make 1 c in
       `Continue (update model (UpdateSearch new_query))
     | _, Some form ->
       let field = List.nth form.fields form.focused_field in
       (match field.field_type with
        | `MultiSelect _ ->
          (* Toggle weekday based on key pressed *)
          let day_map = [('m', "Mon"); ('M', "Mon"); ('t', "Tue"); ('T', "Tue"); 
                         ('w', "Wed"); ('W', "Wed"); ('r', "Thu"); ('R', "Thu");
                         ('f', "Fri"); ('F', "Fri"); ('s', "Sat"); ('S', "Sat");
                         ('u', "Sun"); ('U', "Sun")] in
          (match List.assoc_opt c day_map with
           | Some day ->
             let current_days = if field.value = "" || field.value = "None" then [] 
               else String.split_on_char ',' field.value |> List.map String.trim in
             let new_days = 
               if List.mem day current_days 
               then List.filter (fun d -> d <> day) current_days
               else current_days @ [day]
             in
             let day_order = ["Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat"; "Sun"] in
             let sorted_days = List.filter (fun d -> List.mem d new_days) day_order in
             let new_value = if sorted_days = [] then "None" else String.concat "," sorted_days in
             `Continue (update model (UpdateFieldValue new_value))
           | None -> `Continue model)
        | `Select _ ->
          (* For Select fields, don't append characters *)
          `Continue model
        | _ ->
          let new_value = field.value ^ String.make 1 c in
          `Continue (update model (UpdateFieldValue new_value)))
     | _, None ->
       let new_buffer = model.input_buffer ^ String.make 1 c in
       `Continue (update model (UpdateInput new_buffer)))
  | `Backspace when model.input_mode = Insert ->
    (match model.view, model.form with
     | Search _, _ ->
       (* In search mode, update search query *)
       let len = String.length model.search_query in
       let new_query = if len > 0 then String.sub model.search_query 0 (len - 1) else "" in
       `Continue (update model (UpdateSearch new_query))
     | _, Some form ->
       let field = List.nth form.fields form.focused_field in
       let len = String.length field.value in
       let new_value = if len > 0 then String.sub field.value 0 (len - 1) else "" in
       `Continue (update model (UpdateFieldValue new_value))
     | _, None ->
       let len = String.length model.input_buffer in
       let new_buffer = if len > 0 then String.sub model.input_buffer 0 (len - 1) else "" in
       `Continue (update model (UpdateInput new_buffer)))
  | `Enter when model.input_mode = Insert ->
    (match model.view with
     | Search _ -> `Continue (update model ExecuteSearch)
     | _ -> `Continue (update model SubmitInput))
  | _ -> `Continue model

(** Main application loop *)
let run ~config () =
  let device_id = config.Config.device_id in
  (* Try to connect to remote database *)
  let* db_ctx = 
    let remote_uri = Config.postgres_uri config in
    Storage.Db.init ~local_path:config.Config.db.local_path ?remote_uri ()
  in
  let sync_online, pool, remote_pool = match db_ctx with
    | Ok ctx -> ctx.Storage.Db.remote_available, Some (Storage.Db.local ctx), Storage.Db.remote ctx
    | Error _ -> false, None, None
  in
  (* Run bidirectional sync if both local and remote are available *)
  let* () = match pool, remote_pool with
    | Some local_pool, Some remote_pool ->
      Storage.Sync.run ~local_pool ~remote_pool
    | _ -> Lwt.return ()
  in
  (* Use remote pool if available, otherwise local *)
  let db_pool = match remote_pool with Some p -> Some p | None -> pool in
  (* Load initial data from database *)
  let* tasks = match db_pool with
    | Some p -> Storage.Queries.list_tasks p
    | None -> Lwt.return []
  in
  let* notes = match db_pool with
    | Some p -> Storage.Queries.list_notes p
    | None -> Lwt.return []
  in
  let* events = match db_pool with
    | Some p -> Storage.Queries.list_events p
    | None -> Lwt.return []
  in
  let* projects = match db_pool with
    | Some p -> Storage.Queries.list_projects p
    | None -> Lwt.return []
  in
  (* Load archived/deleted items *)
  let* archived_tasks = match db_pool with
    | Some p -> Storage.Queries.list_deleted_tasks p
    | None -> Lwt.return []
  in
  let* archived_notes = match db_pool with
    | Some p -> Storage.Queries.list_deleted_notes p
    | None -> Lwt.return []
  in
  let* archived_events = match db_pool with
    | Some p -> Storage.Queries.list_deleted_events p
    | None -> Lwt.return []
  in
  let term = Term.create () in
  let (init_w, init_h) = Term.size term in
  (* Load contacts *)
  let* contacts = match db_pool with
    | Some p -> Storage.Queries.list_contacts p
    | None -> Lwt.return []
  in
  (* Helper to get form field value by name *)
  let get_field_value form name =
    match List.find_opt (fun (f : Model.form_field) -> f.name = name) form.Model.fields with
    | Some f -> if f.value = "" then None else Some f.value
    | None -> None
  in
  let get_field_value_or form name default =
    match get_field_value form name with Some v -> v | None -> default
  in
  (* Handle saving new items *)
  let save_and_update model =
    match model.view, db_pool, model.form with
    | TaskEdit None, Some p, Some form ->
      let title = get_field_value_or form "Title" "" in
      if title = "" then
        Lwt.return { model with status = Some { text = "Title is required"; level = `Error; expires_at = None } }
      else
        let description = get_field_value form "Notes" in
        let tags = match get_field_value form "Tags" with
          | Some s -> String.split_on_char ',' s |> List.map String.trim |> List.filter (fun s -> s <> "")
          | None -> []
        in
        let recurrence = match get_field_value form "Recurrence" with
          | Some "None" | None -> None
          | Some r -> Some r
        in
        (* Parse date fields *)
        let parse_date s =
          try
            let months = [("JAN",1);("FEB",2);("MAR",3);("APR",4);("MAY",5);("JUN",6);
                          ("JUL",7);("AUG",8);("SEP",9);("OCT",10);("NOV",11);("DEC",12)] in
            Scanf.sscanf s "%d-%3s-%d" (fun d m y ->
              let mo = List.assoc (String.uppercase_ascii m) months in
              Some (y, mo, d))
          with _ -> None
        in
        let make_timestamp date_opt =
          match date_opt with
          | Some (y, mo, d) ->
            (match Ptime.of_date (y, mo, d) with
             | Some t -> Some { Domain.Types.time = t; timezone = None }
             | None -> None)
          | None -> None
        in
        let due_date = Option.bind (get_field_value form "Due Date") parse_date |> make_timestamp in
        let scheduled_date = Option.bind (get_field_value form "Scheduled") parse_date |> make_timestamp in
        (* Parse datetime fields for time blocking *)
        let parse_datetime s =
          try
            Scanf.sscanf s "%d-%d-%d %d:%d" (fun y mo d hh mm ->
              Some (y, mo, d, hh, mm))
          with _ -> None
        in
        let make_datetime_timestamp dt_opt =
          match dt_opt with
          | Some (y, mo, d, hh, mm) ->
            (match Ptime.of_date_time ((y, mo, d), ((hh, mm, 0), 0)) with
             | Some t -> Some { Domain.Types.time = t; timezone = None }
             | None -> None)
          | None -> None
        in
        let block_start = Option.bind (get_field_value form "Block Start") parse_datetime |> make_datetime_timestamp in
        let block_end = Option.bind (get_field_value form "Block End") parse_datetime |> make_datetime_timestamp in
        (* Check if this is a subtask (entity_id holds parent_id) *)
        let parent_id = form.entity_id in
        let* result = Storage.Queries.create_task p ~title ?description ~tags ?recurrence ?due_date ?scheduled_date ?block_start ?block_end ?parent_id () in
        let* tasks = Storage.Queries.list_tasks p in
        let (status, return_view) = match result, parent_id with
          | Some _, Some pid -> (Some { text = "Subtask created"; level = `Success; expires_at = None }, TaskDetail pid)
          | Some _, None -> (Some { text = "Task created"; level = `Success; expires_at = None }, TaskList)
          | None, _ -> (Some { text = "Failed to create task"; level = `Error; expires_at = None }, TaskList)
        in
        Lwt.return { model with tasks; view = return_view; previous_views = []; input_mode = Normal; form = None; status }
    | NoteEdit None, Some p, Some form ->
      let title = get_field_value_or form "Title" "" in
      if title = "" then
        Lwt.return { model with status = Some { text = "Title is required"; level = `Error; expires_at = None } }
      else
        let content = get_field_value_or form "Content" "" in
        let tags = match get_field_value form "Tags" with
          | Some s -> String.split_on_char ',' s |> List.map String.trim |> List.filter (fun s -> s <> "")
          | None -> []
        in
        let* result = Storage.Queries.create_note p ~title ~content ~tags () in
        let* notes = Storage.Queries.list_notes p in
        let status = match result with
          | Some _ -> Some { text = "Note created"; level = `Success; expires_at = None }
          | None -> Some { text = "Failed to create note"; level = `Error; expires_at = None }
        in
        Lwt.return { model with notes; view = NoteList; previous_views = []; input_mode = Normal; form = None; status }
    | EventEdit None, Some p, Some form ->
      let title = get_field_value_or form "Title" "" in
      if title = "" then
        Lwt.return { model with status = Some { text = "Title is required"; level = `Error; expires_at = None } }
      else
        let description = get_field_value form "Notes" in
        let location = get_field_value form "Location" in
        (* Parse date and time from form fields *)
        let date_str = get_field_value_or form "Date" "" in
        let time_str = get_field_value_or form "Time" "" in
        let start_time = 
          (* Try to parse date (DD-MMM-YYYY) and time (HH:MM) *)
          let parse_date s =
            try
              let parts = String.split_on_char '-' s in
              match parts with
              | [d; m; y] ->
                let day = int_of_string d in
                let month = match String.uppercase_ascii m with
                  | "JAN" -> 1 | "FEB" -> 2 | "MAR" -> 3 | "APR" -> 4
                  | "MAY" -> 5 | "JUN" -> 6 | "JUL" -> 7 | "AUG" -> 8
                  | "SEP" -> 9 | "OCT" -> 10 | "NOV" -> 11 | "DEC" -> 12
                  | _ -> int_of_string m
                in
                let year = int_of_string y in
                Some (year, month, day)
              | _ -> None
            with _ -> None
          in
          let parse_time s =
            try
              let parts = String.split_on_char ':' s in
              match parts with
              | [h; m] -> Some (int_of_string h, int_of_string m, 0)
              | _ -> None
            with _ -> None
          in
          (* UTC+3 offset in seconds (3 hours = 10800 seconds) *)
          let tz_offset_s = 3 * 3600 in
          match parse_date date_str, parse_time time_str with
          | Some (y, mo, d), Some (h, mi, s) ->
            (* Create time with timezone offset to convert local to UTC *)
            (match Ptime.of_date_time ((y, mo, d), ((h, mi, s), tz_offset_s)) with
             | Some t -> { Domain.Types.time = t; timezone = Some tz_offset_s }
             | None -> { Domain.Types.time = Ptime_clock.now (); timezone = None })
          | Some (y, mo, d), None ->
            (match Ptime.of_date_time ((y, mo, d), ((0, 0, 0), tz_offset_s)) with
             | Some t -> { Domain.Types.time = t; timezone = Some tz_offset_s }
             | None -> { Domain.Types.time = Ptime_clock.now (); timezone = None })
          | _ -> { Domain.Types.time = Ptime_clock.now (); timezone = None }
        in
        let recurrence = match get_field_value form "Recurrence" with
          | Some "None" | None -> None
          | Some r -> Some r
        in
        let* result = Storage.Queries.create_event p ~title ?description ~start_time ?location ?recurrence () in
        let* events = Storage.Queries.list_events p in
        let status = match result with
          | Some _ -> Some { text = "Event created"; level = `Success; expires_at = None }
          | None -> Some { text = "Failed to create event"; level = `Error; expires_at = None }
        in
        Lwt.return { model with events; view = Calendar; previous_views = []; input_mode = Normal; form = None; status }
    | ContactEdit None, Some p, Some form ->
      let name = get_field_value_or form "Name" "" in
      if name = "" then
        Lwt.return { model with status = Some { text = "Name is required"; level = `Error; expires_at = None } }
      else
        let email = get_field_value form "Email" in
        let phone = get_field_value form "Phone" in
        let notes = get_field_value form "Notes" in
        let* result = Storage.Queries.create_contact p ~name ?email ?phone ?notes () in
        let* contacts = Storage.Queries.list_contacts p in
        let status = match result with
          | Some _ -> Some { text = "Contact created"; level = `Success; expires_at = None }
          | None -> Some { text = "Failed to create contact"; level = `Error; expires_at = None }
        in
        Lwt.return { model with contacts; view = ContactList; previous_views = []; input_mode = Normal; form = None; status }
    | ProjectEdit None, Some p, Some form ->
      let name = get_field_value_or form "Name" "" in
      if name = "" then
        Lwt.return { model with status = Some { text = "Name is required"; level = `Error; expires_at = None } }
      else
        let description = get_field_value form "Description" in
        let* result = Storage.Queries.create_project p ~name ?description () in
        let* projects = Storage.Queries.list_projects p in
        let status = match result with
          | Some _ -> Some { text = "Project created"; level = `Success; expires_at = None }
          | None -> Some { text = "Failed to create project"; level = `Error; expires_at = None }
        in
        Lwt.return { model with projects; view = Projects; previous_views = []; input_mode = Normal; form = None; status }
    (* Edit existing entities *)
    | TaskEdit (Some id), Some p, Some form ->
      let title = get_field_value_or form "Title" "" in
      if title = "" then
        Lwt.return { model with status = Some { text = "Title is required"; level = `Error; expires_at = None } }
      else
        let description = get_field_value form "Notes" in
        let tags = match get_field_value form "Tags" with
          | Some s -> String.split_on_char ',' s |> List.map String.trim |> List.filter (fun s -> s <> "")
          | None -> []
        in
        let recurrence = match get_field_value form "Recurrence" with
          | Some "None" | None -> None
          | Some r -> Some r
        in
        (* Parse date fields *)
        let parse_date s =
          try
            let months = [("JAN",1);("FEB",2);("MAR",3);("APR",4);("MAY",5);("JUN",6);
                          ("JUL",7);("AUG",8);("SEP",9);("OCT",10);("NOV",11);("DEC",12)] in
            Scanf.sscanf s "%d-%3s-%d" (fun d m y ->
              let mo = List.assoc (String.uppercase_ascii m) months in
              Some (y, mo, d))
          with _ -> None
        in
        let make_timestamp date_opt =
          match date_opt with
          | Some (y, mo, d) ->
            (match Ptime.of_date (y, mo, d) with
             | Some t -> Some { Domain.Types.time = t; timezone = None }
             | None -> None)
          | None -> None
        in
        let due_date = Option.bind (get_field_value form "Due Date") parse_date |> make_timestamp in
        let scheduled_date = Option.bind (get_field_value form "Scheduled") parse_date |> make_timestamp in
        (* Parse datetime fields for time blocking *)
        let parse_datetime s =
          try
            Scanf.sscanf s "%d-%d-%d %d:%d" (fun y mo d hh mm ->
              Some (y, mo, d, hh, mm))
          with _ -> None
        in
        let make_datetime_timestamp dt_opt =
          match dt_opt with
          | Some (y, mo, d, hh, mm) ->
            (match Ptime.of_date_time ((y, mo, d), ((hh, mm, 0), 0)) with
             | Some t -> Some { Domain.Types.time = t; timezone = None }
             | None -> None)
          | None -> None
        in
        let block_start = Option.bind (get_field_value form "Block Start") parse_datetime |> make_datetime_timestamp in
        let block_end = Option.bind (get_field_value form "Block End") parse_datetime |> make_datetime_timestamp in
        let* _result = Storage.Queries.update_task p ~id ~title ?description ~tags ?recurrence ?due_date ?scheduled_date ?block_start ?block_end () in
        let* tasks = Storage.Queries.list_tasks p in
        let status = Some { text = "Task updated"; level = `Success; expires_at = None } in
        Lwt.return { model with tasks; view = TaskDetail id; previous_views = []; input_mode = Normal; form = None; status }
    | NoteEdit (Some id), Some p, Some form ->
      let title = get_field_value_or form "Title" "" in
      if title = "" then
        Lwt.return { model with status = Some { text = "Title is required"; level = `Error; expires_at = None } }
      else
        let content = get_field_value_or form "Content" "" in
        let* _result = Storage.Queries.update_note p ~id ~title ~content () in
        let* notes = Storage.Queries.list_notes p in
        let status = Some { text = "Note updated"; level = `Success; expires_at = None } in
        Lwt.return { model with notes; view = NoteDetail id; previous_views = []; input_mode = Normal; form = None; status }
    | EventEdit (Some id), Some p, Some form ->
      let title = get_field_value_or form "Title" "" in
      if title = "" then
        Lwt.return { model with status = Some { text = "Title is required"; level = `Error; expires_at = None } }
      else
        let description = get_field_value form "Notes" in
        let location = get_field_value form "Location" in
        (* Parse date and time from form fields *)
        let date_str = get_field_value_or form "Date" "" in
        let time_str = get_field_value_or form "Time" "" in
        let start_time = 
          let parse_date s =
            try
              let parts = String.split_on_char '-' s in
              match parts with
              | [d; m; y] ->
                let day = int_of_string d in
                let month = match String.uppercase_ascii m with
                  | "JAN" -> 1 | "FEB" -> 2 | "MAR" -> 3 | "APR" -> 4
                  | "MAY" -> 5 | "JUN" -> 6 | "JUL" -> 7 | "AUG" -> 8
                  | "SEP" -> 9 | "OCT" -> 10 | "NOV" -> 11 | "DEC" -> 12
                  | _ -> int_of_string m
                in
                let year = int_of_string y in
                Some (year, month, day)
              | _ -> None
            with _ -> None
          in
          let parse_time s =
            try
              let parts = String.split_on_char ':' s in
              match parts with
              | [h; m] -> Some (int_of_string h, int_of_string m, 0)
              | _ -> None
            with _ -> None
          in
          let tz_offset_s = 3 * 3600 in
          match parse_date date_str, parse_time time_str with
          | Some (y, mo, d), Some (h, mi, s) ->
            (match Ptime.of_date_time ((y, mo, d), ((h, mi, s), tz_offset_s)) with
             | Some t -> { Domain.Types.time = t; timezone = Some tz_offset_s }
             | None -> { Domain.Types.time = Ptime_clock.now (); timezone = None })
          | Some (y, mo, d), None ->
            (match Ptime.of_date_time ((y, mo, d), ((0, 0, 0), tz_offset_s)) with
             | Some t -> { Domain.Types.time = t; timezone = Some tz_offset_s }
             | None -> { Domain.Types.time = Ptime_clock.now (); timezone = None })
          | _ -> { Domain.Types.time = Ptime_clock.now (); timezone = None }
        in
        let recurrence = match get_field_value form "Recurrence" with
          | Some "None" | None -> None
          | Some r -> Some r
        in
        let* _result = Storage.Queries.update_event p ~id ~title ?description ?location ~start_time ?recurrence () in
        let* events = Storage.Queries.list_events p in
        let status = Some { text = "Event updated"; level = `Success; expires_at = None } in
        Lwt.return { model with events; view = EventDetail id; previous_views = []; input_mode = Normal; form = None; status }
    | ContactEdit (Some id), Some p, Some form ->
      let name = get_field_value_or form "Name" "" in
      if name = "" then
        Lwt.return { model with status = Some { text = "Name is required"; level = `Error; expires_at = None } }
      else
        let email = get_field_value form "Email" in
        let phone = get_field_value form "Phone" in
        let notes = get_field_value form "Notes" in
        let* _result = Storage.Queries.update_contact p ~id ~name ?email ?phone ?notes () in
        let* contacts = Storage.Queries.list_contacts p in
        let status = Some { text = "Contact updated"; level = `Success; expires_at = None } in
        Lwt.return { model with contacts; view = ContactDetail id; previous_views = []; input_mode = Normal; form = None; status }
    | _ ->
      Lwt.return (Update.update model SubmitInput)
  in
  (* Handle deleting items *)
  let delete_and_update model =
    match model.view, db_pool with
    | TaskDetail id, Some p ->
      (* Check if a link is selected - delete link, otherwise delete task *)
      if model.current_links <> [] && model.link_index >= 0 && model.link_index < List.length model.current_links then
        (match List.nth_opt model.current_links model.link_index with
         | Some link ->
           let* _result = Storage.Queries.delete_link p ~id:link.Domain.Types.id in
           let* links = Storage.Queries.list_links_for_entity p ~entity_type:"task" ~entity_id:id in
           let status = Some { text = "Link deleted"; level = `Success; expires_at = None } in
           Lwt.return { model with current_links = links; link_index = max 0 (model.link_index - 1); status }
         | None -> Lwt.return model)
      else if model.current_attachments <> [] && model.attachment_index >= 0 && model.attachment_index < List.length model.current_attachments then
        (match List.nth_opt model.current_attachments model.attachment_index with
         | Some att ->
           let* _result = Storage.Queries.delete_attachment p ~id:att.Domain.Types.id in
           let* attachments = Storage.Queries.list_attachments_for_entity p ~entity_type:"task" ~entity_id:id in
           let status = Some { text = "Attachment deleted"; level = `Success; expires_at = None } in
           Lwt.return { model with current_attachments = attachments; attachment_index = max 0 (model.attachment_index - 1); status }
         | None -> Lwt.return model)
      else
        (* No link or attachment selected - delete the task itself *)
        let* _result = Storage.Queries.delete_task p ~id in
        let* tasks = Storage.Queries.list_tasks p in
        let status = Some { text = "Task deleted"; level = `Success; expires_at = None } in
        Lwt.return { model with tasks; view = TaskList; previous_views = []; status }
    | NoteDetail id, Some p ->
      let* _result = Storage.Queries.delete_note p ~id in
      let* notes = Storage.Queries.list_notes p in
      let status = Some { text = "Note deleted"; level = `Success; expires_at = None } in
      Lwt.return { model with notes; view = NoteList; previous_views = []; status }
    | EventDetail id, Some p ->
      let* _result = Storage.Queries.delete_event p ~id in
      let* events = Storage.Queries.list_events p in
      let status = Some { text = "Event deleted"; level = `Success; expires_at = None } in
      Lwt.return { model with events; view = Calendar; previous_views = []; status }
    | ContactDetail id, Some p ->
      let* _result = Storage.Queries.delete_contact p ~id in
      let* contacts = Storage.Queries.list_contacts p in
      let status = Some { text = "Contact deleted"; level = `Success; expires_at = None } in
      Lwt.return { model with contacts; view = ContactList; previous_views = []; status }
    | (TaskList | Inbox), Some p ->
      (* Delete selected task from list *)
      let tasks_list = match model.view with
        | Inbox -> List.filter (fun (t : Domain.Types.task) -> t.status = Domain.Types.Inbox) model.tasks
        | _ -> model.tasks
      in
      (match List.nth_opt tasks_list model.selected_index with
       | Some task ->
         let* _result = Storage.Queries.delete_task p ~id:task.id in
         let* tasks = Storage.Queries.list_tasks p in
         let status = Some { text = "Task deleted"; level = `Success; expires_at = None } in
         Lwt.return { model with tasks; status; selected_index = max 0 (model.selected_index - 1) }
       | None -> Lwt.return model)
    | NoteList, Some p ->
      (match List.nth_opt model.notes model.selected_index with
       | Some note ->
         let* _result = Storage.Queries.delete_note p ~id:note.id in
         let* notes = Storage.Queries.list_notes p in
         let status = Some { text = "Note deleted"; level = `Success; expires_at = None } in
         Lwt.return { model with notes; status; selected_index = max 0 (model.selected_index - 1) }
       | None -> Lwt.return model)
    | Calendar, Some p ->
      (match List.nth_opt model.events model.selected_index with
       | Some event ->
         let* _result = Storage.Queries.delete_event p ~id:event.id in
         let* events = Storage.Queries.list_events p in
         let status = Some { text = "Event deleted"; level = `Success; expires_at = None } in
         Lwt.return { model with events; status; selected_index = max 0 (model.selected_index - 1) }
       | None -> Lwt.return model)
    | ContactList, Some p ->
      (match List.nth_opt model.contacts model.selected_index with
       | Some contact ->
         let* _result = Storage.Queries.delete_contact p ~id:contact.id in
         let* contacts = Storage.Queries.list_contacts p in
         let status = Some { text = "Contact deleted"; level = `Success; expires_at = None } in
         Lwt.return { model with contacts; status; selected_index = max 0 (model.selected_index - 1) }
       | None -> Lwt.return model)
    | Archive, Some p ->
      (* Permanent delete from archive *)
      let all_archived = 
        List.map (fun t -> `Task t) model.archived_tasks @
        List.map (fun n -> `Note n) model.archived_notes @
        List.map (fun e -> `Event e) model.archived_events
      in
      (match List.nth_opt all_archived model.selected_index with
       | Some (`Task task) ->
         let* _result = Storage.Queries.permanent_delete_task p ~id:task.Domain.Types.id in
         let* archived_tasks = Storage.Queries.list_deleted_tasks p in
         let status = Some { text = "Task permanently deleted"; level = `Success; expires_at = None } in
         Lwt.return { model with archived_tasks; status; selected_index = max 0 (model.selected_index - 1) }
       | Some (`Note note) ->
         let* _result = Storage.Queries.permanent_delete_note p ~id:note.Domain.Types.id in
         let* archived_notes = Storage.Queries.list_deleted_notes p in
         let status = Some { text = "Note permanently deleted"; level = `Success; expires_at = None } in
         Lwt.return { model with archived_notes; status; selected_index = max 0 (model.selected_index - 1) }
       | Some (`Event event) ->
         let* _result = Storage.Queries.permanent_delete_event p ~id:event.Domain.Types.id in
         let* archived_events = Storage.Queries.list_deleted_events p in
         let status = Some { text = "Event permanently deleted"; level = `Success; expires_at = None } in
         Lwt.return { model with archived_events; status; selected_index = max 0 (model.selected_index - 1) }
       | None -> Lwt.return model)
    | ProjectDetail id, Some p ->
      (* Delete selected link *)
      (match List.nth_opt model.current_links model.link_index with
       | Some link ->
         let* _result = Storage.Queries.delete_link p ~id:link.Domain.Types.id in
         let* links = Storage.Queries.list_links_for_entity p ~entity_type:"project" ~entity_id:id in
         let status = Some { text = "Link deleted"; level = `Success; expires_at = None } in
         Lwt.return { model with current_links = links; link_index = max 0 (model.link_index - 1); status }
       | None -> Lwt.return model)
    | _ ->
      let status = Some { text = "Cannot delete from this view"; level = `Warning; expires_at = None } in
      Lwt.return { model with status }
  in
  let rec loop model =
    let (w, h) = Term.size term in
    let model = { model with width = w; height = h } in
    let img = render model (w, h) in
    let* () = Term.image term img in
    let* event = Lwt_stream.get (Term.events term) in
    match event with
    | Some (`Key (key, _mods)) ->
      (match key, model.input_mode with
       | `Enter, Insert ->
         (match model.view with
          | Search _ ->
            (* Execute search - open selected result *)
            let new_model = Update.update model ExecuteSearch in
            loop new_model
          | _ ->
            let* new_model = save_and_update model in
            loop new_model)
       | `Enter, Command ->
         (* Handle command mode input - currently used for file attachment *)
         (match model.view, db_pool with
          | (TaskDetail entity_id | NoteDetail entity_id), Some p ->
            let filepath = String.trim model.input_buffer in
            if filepath = "" then
              loop { model with input_mode = Normal; input_buffer = ""; status = Some { text = "Cancelled"; level = `Info; expires_at = None } }
            else if not (Sys.file_exists filepath) then
              loop { model with status = Some { text = "File not found: " ^ filepath; level = `Error; expires_at = None } }
            else
              let filename = Filename.basename filepath in
              let size_bytes = (Unix.stat filepath).Unix.st_size |> Int64.of_int in
              let entity_type = match model.view with TaskDetail _ -> "task" | NoteDetail _ -> "note" | _ -> "task" in
              (* Copy file to attachments directory *)
              let attachments_dir = Filename.concat (Sys.getenv "HOME") ".local/share/parenvault/attachments" in
              let () = if not (Sys.file_exists attachments_dir) then Unix.mkdir attachments_dir 0o755 in
              let dest_filename = Printf.sprintf "%s_%s" (Storage.Queries.new_uuid ()) filename in
              let dest_path = Filename.concat attachments_dir dest_filename in
              let () = 
                let ic = open_in_bin filepath in
                let oc = open_out_bin dest_path in
                try
                  let buf = Bytes.create 4096 in
                  let rec copy () =
                    let n = input ic buf 0 4096 in
                    if n > 0 then (output oc buf 0 n; copy ())
                  in
                  copy ();
                  close_in ic;
                  close_out oc
                with e -> close_in ic; close_out oc; raise e
              in
              let* _result = Storage.Queries.create_attachment p ~filename ~filepath:dest_filename ~size_bytes ~entity_type ~entity_id () in
              let* attachments = Storage.Queries.list_attachments_for_entity p ~entity_type ~entity_id in
              let status = Some { text = "Attached: " ^ filename; level = `Success; expires_at = None } in
              loop { model with input_mode = Normal; input_buffer = ""; current_attachments = attachments; status }
          | _ ->
            loop { model with input_mode = Normal; input_buffer = ""; status = Some { text = "Cannot attach here"; level = `Warning; expires_at = None } })
       | `Escape, Command ->
         loop { model with input_mode = Normal; input_buffer = ""; status = None }
       | `ASCII c, Command ->
         loop { model with input_buffer = model.input_buffer ^ String.make 1 c }
       | `Backspace, Command ->
         let len = String.length model.input_buffer in
         let new_buf = if len > 0 then String.sub model.input_buffer 0 (len - 1) else "" in
         loop { model with input_buffer = new_buf }
       | `ASCII 'd', Normal ->
         let* new_model = delete_and_update model in
         loop new_model
       | `ASCII 'x', Normal ->
         (* Toggle task status and save to DB *)
         (match model.view, db_pool with
          | (TaskList | Inbox | Dashboard), Some p ->
            let tasks_list = match model.view with
              | Inbox -> List.filter (fun (t : Domain.Types.task) -> t.status = Domain.Types.Inbox) model.tasks
              | _ -> List.filter (fun (t : Domain.Types.task) -> t.parent_id = None) model.tasks
            in
            (match List.nth_opt tasks_list model.selected_index with
             | Some task ->
               let new_status = if task.status = Domain.Types.Done then Domain.Types.Todo else Domain.Types.Done in
               let* _result = Storage.Queries.update_task_status p ~id:task.id ~status:new_status in
               let new_model = Update.update model ToggleTaskStatus in
               loop new_model
             | None -> loop model)
          | TaskDetail task_id, Some p ->
            (* Handle subtask toggling in task detail view *)
            let subtasks = List.filter (fun (t : Domain.Types.task) -> t.parent_id = Some task_id) model.tasks in
            let target_id = 
              if subtasks <> [] then
                (* Toggle selected subtask *)
                match List.nth_opt subtasks model.subtask_index with
                | Some subtask -> Some subtask.Domain.Types.id
                | None -> None
              else
                (* No subtasks - toggle this task's own status *)
                Some task_id
            in
            (match target_id with
             | Some id ->
               (match List.find_opt (fun (t : Domain.Types.task) -> t.id = id) model.tasks with
                | Some task ->
                  let new_status = if task.status = Domain.Types.Done then Domain.Types.Todo else Domain.Types.Done in
                  let* _result = Storage.Queries.update_task_status p ~id ~status:new_status in
                  let new_model = Update.update model (ToggleSubtaskStatus id) in
                  loop new_model
                | None -> loop model)
             | None -> loop model)
          | _ -> loop model)
       | `ASCII 'A', Normal ->
         (* Add attachment - prompt for file path *)
         (match model.view with
          | TaskDetail _ | NoteDetail _ ->
            (* Enter command mode to get file path *)
            let status = Some { text = "Enter file path (or drag file): "; level = `Info; expires_at = None } in
            loop { model with input_mode = Command; input_buffer = ""; status }
          | _ ->
            let status = Some { text = "Attachments only work in Task/Note detail views"; level = `Warning; expires_at = None } in
            loop { model with status })
       | `ASCII 'r', Normal ->
         (* Restore from archive *)
         (match model.view, db_pool with
          | Archive, Some p ->
            let all_archived = 
              List.map (fun t -> `Task t) model.archived_tasks @
              List.map (fun n -> `Note n) model.archived_notes @
              List.map (fun e -> `Event e) model.archived_events
            in
            (match List.nth_opt all_archived model.selected_index with
             | Some (`Task task) ->
               let* _result = Storage.Queries.restore_task p ~id:task.Domain.Types.id in
               let* tasks = Storage.Queries.list_tasks p in
               let* archived_tasks = Storage.Queries.list_deleted_tasks p in
               let status = Some { text = "Task restored"; level = `Success; expires_at = None } in
               loop { model with tasks; archived_tasks; status; selected_index = max 0 (model.selected_index - 1) }
             | Some (`Note note) ->
               let* _result = Storage.Queries.restore_note p ~id:note.Domain.Types.id in
               let* notes = Storage.Queries.list_notes p in
               let* archived_notes = Storage.Queries.list_deleted_notes p in
               let status = Some { text = "Note restored"; level = `Success; expires_at = None } in
               loop { model with notes; archived_notes; status; selected_index = max 0 (model.selected_index - 1) }
             | Some (`Event event) ->
               let* _result = Storage.Queries.restore_event p ~id:event.Domain.Types.id in
               let* events = Storage.Queries.list_events p in
               let* archived_events = Storage.Queries.list_deleted_events p in
               let status = Some { text = "Event restored"; level = `Success; expires_at = None } in
               loop { model with events; archived_events; status; selected_index = max 0 (model.selected_index - 1) }
             | None -> loop model)
          | _ -> 
            let status = Some { text = "Restore only works in Archive view (press 9)"; level = `Warning; expires_at = None } in
            loop { model with status })
       | `ASCII 'o', Normal ->
         (* Open attachment with xdg-open *)
         (match model.view with
          | TaskDetail _ | NoteDetail _ when model.current_attachments <> [] ->
            (match List.nth_opt model.current_attachments model.attachment_index with
             | Some att ->
               let attachments_dir = Filename.concat (Sys.getenv "HOME") ".local/share/parenvault/attachments" in
               let full_path = Filename.concat attachments_dir att.Domain.Types.filepath in
               let _ = Unix.create_process "xdg-open" [|"xdg-open"; full_path|] Unix.stdin Unix.stdout Unix.stderr in
               let status = Some { text = "Opening: " ^ att.filename; level = `Info; expires_at = None } in
               loop { model with status }
             | None -> loop model)
          | _ -> loop model)
       | `Enter, Normal ->
         (* Handle Enter key - load attachments when opening detail views *)
         (match model.view, db_pool with
          | LinkPicker (source_type, source_id), Some p ->
            (* Create link to selected item *)
            let items = 
              List.mapi (fun i (n : Domain.Types.note) -> (i, "note", n.id)) model.notes @
              List.mapi (fun i (pr : Domain.Types.project) -> (i + List.length model.notes, "project", pr.id)) model.projects @
              List.mapi (fun i (c : Domain.Types.contact) -> (i + List.length model.notes + List.length model.projects, "contact", c.id)) model.contacts @
              (if source_type <> "task" then 
                List.mapi (fun i (t : Domain.Types.task) -> (i + List.length model.notes + List.length model.projects + List.length model.contacts, "task", t.id)) model.tasks
              else [])
            in
            Printf.eprintf "LinkPicker: selected_index=%d, items=%d, notes=%d, projects=%d, contacts=%d\n%!" 
              model.selected_index (List.length items) (List.length model.notes) (List.length model.projects) (List.length model.contacts);
            (match List.find_opt (fun (i, _, _) -> i = model.selected_index) items with
             | Some (_, target_type, target_id) ->
               let* link_id = Storage.Queries.create_link p ~source_type ~source_id ~target_type ~target_id ~link_type:"related" in
               let status_text = match link_id with
                 | Some id -> Printf.sprintf "Linked to %s (id: %s)" target_type (String.sub id 0 8)
                 | None -> Printf.sprintf "Failed to link to %s" target_type
               in
               let level = if link_id = None then `Error else `Success in
               let status = Some { text = status_text; level; expires_at = None } in
               let new_view = match model.previous_views with v :: _ -> v | [] -> Dashboard in
               let prev = match model.previous_views with _ :: rest -> rest | [] -> [] in
               loop { model with view = new_view; previous_views = prev; status }
             | None -> 
               Printf.eprintf "No item found at index %d\n%!" model.selected_index;
               loop model)
          | (TaskList | Inbox | Dashboard | NoteList | Calendar | Projects), Some p ->
            let new_model = Update.update model OpenSelected in
            (* Load attachments/links for the new view *)
            (match new_model.view with
             | TaskDetail id ->
               let* attachments = Storage.Queries.list_attachments_for_entity p ~entity_type:"task" ~entity_id:id in
               let* links = Storage.Queries.list_links_for_entity p ~entity_type:"task" ~entity_id:id in
               (* Start with attachment_index=-1 if there are subtasks, so navigation starts from subtasks *)
               let subtasks = List.filter (fun (t : Domain.Types.task) -> t.parent_id = Some id) new_model.tasks in
               let att_idx = if subtasks <> [] then -1 else 0 in
               loop { new_model with current_attachments = attachments; attachment_index = att_idx; current_links = links; link_index = 0 }
             | NoteDetail id ->
               let* attachments = Storage.Queries.list_attachments_for_entity p ~entity_type:"note" ~entity_id:id in
               let* links = Storage.Queries.list_links_for_entity p ~entity_type:"note" ~entity_id:id in
               loop { new_model with current_attachments = attachments; attachment_index = 0; current_links = links; link_index = 0 }
             | ProjectDetail id ->
               let* links = Storage.Queries.list_links_for_entity p ~entity_type:"project" ~entity_id:id in
               loop { new_model with current_links = links; link_index = 0 }
             | _ -> loop new_model)
          | TaskDetail id, Some p ->
            (* Open linked item from task detail *)
            if model.current_links <> [] && model.link_index >= 0 && model.link_index < List.length model.current_links then
              (match List.nth_opt model.current_links model.link_index with
               | Some link ->
                 let (other_type, other_id) = 
                   if link.Domain.Types.target_id = id then (link.source_type, link.source_id)
                   else (link.target_type, link.target_id)
                 in
                 (match other_type with
                  | "task" ->
                    let* attachments = Storage.Queries.list_attachments_for_entity p ~entity_type:"task" ~entity_id:other_id in
                    let* links = Storage.Queries.list_links_for_entity p ~entity_type:"task" ~entity_id:other_id in
                    let subtasks = List.filter (fun (t : Domain.Types.task) -> t.parent_id = Some other_id) model.tasks in
                    let att_idx = if subtasks <> [] then -1 else 0 in
                    loop { model with view = TaskDetail other_id; previous_views = model.view :: model.previous_views; current_attachments = attachments; attachment_index = att_idx; current_links = links; link_index = 0 }
                  | "note" ->
                    let* attachments = Storage.Queries.list_attachments_for_entity p ~entity_type:"note" ~entity_id:other_id in
                    let* links = Storage.Queries.list_links_for_entity p ~entity_type:"note" ~entity_id:other_id in
                    loop { model with view = NoteDetail other_id; previous_views = model.view :: model.previous_views; current_attachments = attachments; attachment_index = 0; current_links = links; link_index = 0 }
                  | "project" ->
                    let* links = Storage.Queries.list_links_for_entity p ~entity_type:"project" ~entity_id:other_id in
                    loop { model with view = ProjectDetail other_id; previous_views = model.view :: model.previous_views; current_links = links; link_index = 0 }
                  | "event" ->
                    loop { model with view = EventDetail other_id; previous_views = model.view :: model.previous_views }
                  | "contact" ->
                    loop { model with view = ContactDetail other_id; previous_views = model.view :: model.previous_views }
                  | _ -> loop model)
               | None -> loop model)
            else
              loop model
          | ProjectDetail id, Some p ->
            (* Open linked item from project detail *)
            (match List.nth_opt model.current_links model.link_index with
             | Some link ->
               let (other_type, other_id) = 
                 if link.Domain.Types.target_id = id then (link.source_type, link.source_id)
                 else (link.target_type, link.target_id)
               in
               (match other_type with
                | "task" ->
                  let* attachments = Storage.Queries.list_attachments_for_entity p ~entity_type:"task" ~entity_id:other_id in
                  let* links = Storage.Queries.list_links_for_entity p ~entity_type:"task" ~entity_id:other_id in
                  let subtasks = List.filter (fun (t : Domain.Types.task) -> t.parent_id = Some other_id) model.tasks in
                  let att_idx = if subtasks <> [] then -1 else 0 in
                  loop { model with view = TaskDetail other_id; previous_views = model.view :: model.previous_views; current_attachments = attachments; attachment_index = att_idx; current_links = links; link_index = 0 }
                | "note" ->
                  let* attachments = Storage.Queries.list_attachments_for_entity p ~entity_type:"note" ~entity_id:other_id in
                  let* links = Storage.Queries.list_links_for_entity p ~entity_type:"note" ~entity_id:other_id in
                  loop { model with view = NoteDetail other_id; previous_views = model.view :: model.previous_views; current_attachments = attachments; attachment_index = 0; current_links = links; link_index = 0 }
                | "project" ->
                  let* links = Storage.Queries.list_links_for_entity p ~entity_type:"project" ~entity_id:other_id in
                  loop { model with view = ProjectDetail other_id; previous_views = model.view :: model.previous_views; current_links = links; link_index = 0 }
                | "event" ->
                  loop { model with view = EventDetail other_id; previous_views = model.view :: model.previous_views }
                | "contact" ->
                  loop { model with view = ContactDetail other_id; previous_views = model.view :: model.previous_views }
                | _ -> loop model)
             | None -> loop model)
          | _ ->
            let new_model = Update.update model OpenSelected in
            loop new_model)
       | _ ->
         (match handle_key model key with
          | `Quit -> 
            let* () = Term.release term in
            Lwt.return ()
          | `Continue new_model -> loop new_model))
    | Some (`Resize (w, h)) ->
      loop (Update.update model (Resize (w, h)))
    | Some (`Mouse _) -> loop model
    | Some (`Paste _) -> loop model
    | None -> 
      let* () = Term.release term in
      Lwt.return ()
  in
  let initial_model = { (Model.init ~device_id) with sync_online; width = init_w; height = init_h; tasks; notes; events; projects; contacts; archived_tasks; archived_notes; archived_events } in
  loop initial_model
