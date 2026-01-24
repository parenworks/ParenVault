(** TUI view rendering *)

module DT = Domain.Types
open Model

(** ANSI color codes *)
module Color = struct
  let reset = "\027[0m"
  let bold = "\027[1m"
  let dim = "\027[2m"
  let italic = "\027[3m"
  let underline = "\027[4m"
  
  let black = "\027[30m"
  let red = "\027[31m"
  let green = "\027[32m"
  let yellow = "\027[33m"
  let blue = "\027[34m"
  let magenta = "\027[35m"
  let cyan = "\027[36m"
  let white = "\027[37m"
  
  let bg_black = "\027[40m"
  let bg_red = "\027[41m"
  let bg_green = "\027[42m"
  let bg_yellow = "\027[43m"
  let bg_blue = "\027[44m"
  let bg_magenta = "\027[45m"
  let bg_cyan = "\027[46m"
  let bg_white = "\027[47m"
end

(** Format a priority badge *)
let priority_badge = function
  | DT.P0 -> Color.bg_red ^ Color.white ^ " P0 " ^ Color.reset
  | DT.P1 -> Color.bg_yellow ^ Color.black ^ " P1 " ^ Color.reset
  | DT.P2 -> Color.bg_blue ^ Color.white ^ " P2 " ^ Color.reset
  | DT.P3 -> Color.dim ^ " P3 " ^ Color.reset

(** Format task status *)
let status_icon = function
  | DT.Inbox -> "📥"
  | DT.Todo -> "○"
  | DT.InProgress -> "◐"
  | DT.Waiting -> "⏸"
  | DT.Done -> "✓"
  | DT.Cancelled -> "✗"

(** Format a task line *)
let render_task ~selected (task : DT.task) =
  let prefix = if selected then Color.bg_blue ^ Color.white ^ " > " else "   " in
  let status = status_icon task.DT.status in
  let priority = priority_badge task.DT.priority in
  let due = match task.due_date with
    | Some d -> 
      let date_str = Ptime.to_date d.time |> fun (y, m, day) ->
        Printf.sprintf "%04d-%02d-%02d" y m day
      in
      Color.dim ^ " 📅 " ^ date_str ^ Color.reset
    | None -> ""
  in
  let tags = match task.tags with
    | [] -> ""
    | ts -> Color.cyan ^ " #" ^ String.concat " #" ts ^ Color.reset
  in
  Printf.sprintf "%s%s %s %s%s%s%s" 
    prefix status priority task.title due tags Color.reset

(** Format a note line *)
let render_note ~selected (note : DT.note) =
  let prefix = if selected then Color.bg_blue ^ Color.white ^ " > " else "   " in
  let tags = match note.DT.tags with
    | [] -> ""
    | ts -> Color.cyan ^ " #" ^ String.concat " #" ts ^ Color.reset
  in
  Printf.sprintf "%s📝 %s%s%s" prefix note.DT.title tags Color.reset

(** Format an event line *)
let render_event ~selected (event : DT.event) =
  let prefix = if selected then Color.bg_blue ^ Color.white ^ " > " else "   " in
  let time = 
    let (y, m, d) = Ptime.to_date event.DT.start_time.DT.time in
    Printf.sprintf "%04d-%02d-%02d" y m d
  in
  Printf.sprintf "%s📅 %s %s%s" prefix time event.title Color.reset

(** Render header bar *)
let render_header model =
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
  in
  let sync_status = 
    if model.sync_online then
      Color.green ^ "●" ^ Color.reset ^ " Online"
    else
      Color.red ^ "●" ^ Color.reset ^ " Offline"
  in
  let pending = 
    if model.sync_pending > 0 then
      Printf.sprintf " (%d pending)" model.sync_pending
    else ""
  in
  Printf.sprintf "%s%s ParenVault %s│ %s %s%s%s\n%s"
    Color.bg_blue Color.white Color.reset
    view_name sync_status pending Color.reset
    (String.make model.width '-')

(** Render footer/status bar *)
let render_footer model =
  let mode = match model.input_mode with
    | Normal -> "NORMAL"
    | Insert -> "INSERT"
    | Command -> "COMMAND"
  in
  let help = match model.view with
    | TaskList | Dashboard -> "j/k:nav  Enter:open  n:new  d:delete  s:sync  q:quit"
    | NoteList -> "j/k:nav  Enter:open  n:new  d:delete  q:quit"
    | TaskDetail _ | NoteDetail _ -> "e:edit  Esc:back  d:delete"
    | TaskEdit _ | NoteEdit _ -> "Ctrl+S:save  Esc:cancel"
    | _ -> "q:quit  ?:help"
  in
  let status_msg = match model.status with
    | Some s -> 
      let color = match s.level with
        | `Info -> Color.blue
        | `Success -> Color.green
        | `Warning -> Color.yellow
        | `Error -> Color.red
      in
      color ^ s.text ^ Color.reset
    | None -> ""
  in
  Printf.sprintf "%s\n%s[%s]%s %s  %s"
    (String.make model.width '-')
    Color.bold mode Color.reset help status_msg

(** Render task list view *)
let render_task_list model tasks =
  let visible_height = model.height - 6 in  (* Header + footer *)
  let start_idx = model.scroll_offset in
  let end_idx = min (start_idx + visible_height) (List.length tasks) in
  let visible_tasks = 
    tasks 
    |> List.filteri (fun i _ -> i >= start_idx && i < end_idx)
  in
  let lines = List.mapi (fun i task ->
    let actual_idx = start_idx + i in
    render_task ~selected:(actual_idx = model.selected_index) task
  ) visible_tasks in
  String.concat "\n" lines

(** Render note list view *)
let render_note_list model notes =
  let visible_height = model.height - 6 in
  let start_idx = model.scroll_offset in
  let end_idx = min (start_idx + visible_height) (List.length notes) in
  let visible_notes = 
    notes 
    |> List.filteri (fun i _ -> i >= start_idx && i < end_idx)
  in
  let lines = List.mapi (fun i note ->
    let actual_idx = start_idx + i in
    render_note ~selected:(actual_idx = model.selected_index) note
  ) visible_notes in
  String.concat "\n" lines

(** Render dashboard *)
let render_dashboard model =
  let inbox_count = List.filter (fun (t : DT.task) -> t.DT.status = DT.Inbox) model.tasks |> List.length in
  let today_count = 
    let today = Ptime_clock.now () in
    List.filter (fun (t : DT.task) -> 
      match t.DT.due_date with
      | Some d -> Ptime.to_date d.DT.time = Ptime.to_date today
      | None -> false
    ) model.tasks |> List.length
  in
  let in_progress = List.filter (fun (t : DT.task) -> t.DT.status = DT.InProgress) model.tasks |> List.length in
  let overdue = 
    let now = Ptime_clock.now () in
    List.filter (fun (t : DT.task) ->
      match t.DT.due_date with
      | Some d -> Ptime.is_earlier d.DT.time ~than:now && t.DT.status <> DT.Done && t.DT.status <> DT.Cancelled
      | None -> false
    ) model.tasks |> List.length
  in
  
  Printf.sprintf {|
%s┌─────────────────────────────────────────────────────────────┐%s
%s│                     📋 Quick Stats                          │%s
%s├─────────────────────────────────────────────────────────────┤%s
%s│  📥 Inbox:        %-4d    │    🔄 In Progress:  %-4d        │%s
%s│  📅 Due Today:    %-4d    │    ⚠️  Overdue:      %-4d        │%s
%s│  📝 Notes:        %-4d    │    📁 Projects:     %-4d        │%s
%s└─────────────────────────────────────────────────────────────┘%s

%sRecent Tasks:%s
%s
%sUpcoming Events:%s
%s
|}
    Color.cyan Color.reset
    Color.cyan Color.reset
    Color.cyan Color.reset
    Color.cyan inbox_count in_progress Color.reset
    Color.cyan today_count overdue Color.reset
    Color.cyan (List.length model.notes) (List.length model.projects) Color.reset
    Color.cyan Color.reset
    Color.bold Color.reset
    (model.tasks |> List.filteri (fun i _ -> i < 5) 
     |> List.map (render_task ~selected:false) |> String.concat "\n")
    Color.bold Color.reset
    (model.events |> List.filteri (fun i _ -> i < 3)
     |> List.map (render_event ~selected:false) |> String.concat "\n")

(** Main render function *)
let render model =
  let header = render_header model in
  let content = match model.view with
    | Dashboard -> render_dashboard model
    | TaskList -> render_task_list model model.tasks
    | Inbox -> 
      let inbox_tasks = List.filter (fun (t : DT.task) -> t.DT.status = DT.Inbox) model.tasks in
      render_task_list model inbox_tasks
    | NoteList -> render_note_list model model.notes
    | TaskDetail id ->
      (match List.find_opt (fun (t : DT.task) -> t.DT.id = id) model.tasks with
       | Some task -> 
         Printf.sprintf "\n%s%s%s\n\nStatus: %s\nPriority: %s\n\n%s"
           Color.bold task.DT.title Color.reset
           (DT.task_status_to_string task.DT.status)
           (DT.priority_to_string task.DT.priority)
           (Option.value ~default:"No description" task.DT.description)
       | None -> "Task not found")
    | NoteDetail id ->
      (match List.find_opt (fun (n : DT.note) -> n.DT.id = id) model.notes with
       | Some note ->
         Printf.sprintf "\n%s%s%s\n\n%s"
           Color.bold note.DT.title Color.reset
           note.DT.content
       | None -> "Note not found")
    | Calendar ->
      model.events 
      |> List.mapi (fun i e -> render_event ~selected:(i = model.selected_index) e)
      |> String.concat "\n"
    | Projects ->
      model.projects
      |> List.mapi (fun i (p : DT.project) -> 
        let prefix = if i = model.selected_index then " > " else "   " in
        Printf.sprintf "%s📁 %s" prefix p.DT.name)
      |> String.concat "\n"
    | _ -> "View not implemented"
  in
  let footer = render_footer model in
  Printf.sprintf "%s\n%s\n%s" header content footer
