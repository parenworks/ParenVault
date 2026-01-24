(** TUI application model - Elm architecture *)

open Domain.Types

(** Current view/screen *)
type view =
  | Dashboard
  | TaskList
  | TaskDetail of uuid
  | TaskEdit of uuid option  (** None = new task *)
  | NoteList
  | NoteDetail of uuid
  | NoteEdit of uuid option
  | Calendar
  | EventDetail of uuid
  | EventEdit of uuid option  (** None = new event *)
  | ContactList
  | ContactDetail of uuid
  | ContactEdit of uuid option  (** None = new contact *)
  | Projects
  | ProjectDetail of uuid
  | ProjectEdit of uuid option  (** None = new project *)
  | Inbox
  | Archive
  | Search of string
  | LinkPicker of string * uuid  (** source_type, source_id - picking target to link *)

(** Input mode *)
type input_mode =
  | Normal
  | Insert
  | Command

(** Status bar message *)
type status_message = {
  text: string;
  level: [`Info | `Success | `Warning | `Error];
  expires_at: Ptime.t option;
}

(** Form field for multi-field editing *)
type form_field = {
  name: string;
  value: string;
  field_type: [`Text | `MultiLine | `Date | `Select of string list | `MultiSelect of string list];
}

(** Form state for editing entities *)
type form_state = {
  fields: form_field list;
  focused_field: int;  (** Index of currently focused field *)
  entity_id: uuid option;  (** None for new, Some for editing existing *)
}

(** Application state *)
type model = {
  (* Navigation *)
  view: view;
  previous_views: view list;  (** For back navigation *)
  
  (* Data *)
  tasks: task list;
  notes: note list;
  events: event list;
  projects: project list;
  contacts: contact list;
  
  (* Archived/deleted items *)
  archived_tasks: task list;
  archived_notes: note list;
  archived_events: event list;
  
  (* Attachments for current detail view *)
  current_attachments: Domain.Types.attachment list;
  attachment_index: int;
  
  (* Links for current detail view *)
  current_links: Domain.Types.link list;
  link_index: int;
  
  (* UI state *)
  selected_index: int;
  scroll_offset: int;
  input_mode: input_mode;
  input_buffer: string;
  form: form_state option;  (** Active form for editing *)
  
  (* Sync status *)
  sync_online: bool;
  sync_pending: int;
  last_sync: Ptime.t option;
  
  (* Status *)
  status: status_message option;
  
  (* Search *)
  search_query: string;
  search_results: [`Task of task | `Note of note | `Event of event | `Contact of contact] list;
  
  (* Subtask selection in task detail view *)
  subtask_index: int;
  
  (* Config *)
  device_id: string;
  
  (* Terminal size *)
  width: int;
  height: int;
}

(** Initial model *)
let init ~device_id =
  {
    view = Dashboard;
    previous_views = [];
    tasks = [];
    notes = [];
    events = [];
    projects = [];
    contacts = [];
    archived_tasks = [];
    archived_notes = [];
    archived_events = [];
    current_attachments = [];
    attachment_index = 0;
    current_links = [];
    link_index = 0;
    selected_index = 0;
    scroll_offset = 0;
    input_mode = Normal;
    input_buffer = "";
    form = None;
    sync_online = false;
    sync_pending = 0;
    last_sync = None;
    status = None;
    search_query = "";
    search_results = [];
    subtask_index = 0;
    device_id;
    width = 80;
    height = 24;
  }

(** Messages/events that update the model *)
type msg =
  (* Navigation *)
  | Navigate of view
  | GoBack
  | Quit
  
  (* Selection *)
  | SelectNext
  | SelectPrev
  | SelectFirst
  | SelectLast
  | PageDown
  | PageUp
  
  (* Actions *)
  | OpenSelected
  | CreateNew
  | QuickCapture
  | DailyNote
  | AddSubtask
  | AddAttachment of string  (* filepath *)
  | DeleteAttachment
  | OpenAttachment
  | EditSelected
  | DeleteSelected
  | RestoreSelected  (* Restore from archive *)
  | ToggleTaskStatus
  | ToggleSubtaskStatus of uuid  (* Toggle subtask by ID *)
  | SetTaskPriority of priority
  
  (* Input *)
  | EnterInsertMode
  | ExitInsertMode
  | UpdateInput of string
  | SubmitInput
  | CancelInput
  
  (* Form navigation *)
  | NextField
  | PrevField
  | UpdateFieldValue of string
  
  (* Data loading *)
  | TasksLoaded of task list
  | NotesLoaded of note list
  | EventsLoaded of event list
  | ProjectsLoaded of project list
  | LoadError of string
  
  (* Sync *)
  | SyncStatusChanged of bool * int
  | SyncCompleted of Ptime.t
  | TriggerSync
  
  (* Status *)
  | ShowStatus of status_message
  | ClearStatus
  
  (* Search *)
  | StartSearch
  | UpdateSearch of string
  | ExecuteSearch
  
  (* Terminal *)
  | Resize of int * int
  
  (* Tick for animations/status expiry *)
  | Tick

(** Get items for current view *)
let current_items model =
  match model.view with
  | TaskList | Dashboard -> List.length model.tasks
  | NoteList -> List.length model.notes
  | Calendar -> List.length model.events
  | Projects -> List.length model.projects
  | ContactList -> List.length model.contacts
  | Inbox -> 
    List.filter (fun (t : Domain.Types.task) -> t.status = Domain.Types.Inbox) model.tasks |> List.length
  | Archive ->
    List.length model.archived_tasks + List.length model.archived_notes + List.length model.archived_events
  | Search _ -> List.length model.search_results
  | _ -> 0

(** Clamp selection to valid range *)
let clamp_selection model =
  let max_idx = max 0 (current_items model - 1) in
  { model with selected_index = max 0 (min model.selected_index max_idx) }
