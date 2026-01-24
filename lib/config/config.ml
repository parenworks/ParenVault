(** Application configuration *)

(** Database configuration *)
type db_config = {
  local_path: string;           (** Path to local SQLite database *)
  remote_host: string option;   (** PostgreSQL host (via Tailscale) *)
  remote_port: int;             (** PostgreSQL port *)
  remote_name: string option;   (** Database name *)
  remote_user: string option;   (** Database user *)
  remote_password: string option; (** Database password *)
}

(** aerc/mail configuration *)
type mail_config = {
  maildir_path: string;         (** Path to maildir *)
  inbox_folder: string;         (** Inbox folder name *)
  action_folder: string;        (** Folder for emails to convert to tasks *)
  processed_folder: string;     (** Where processed emails go *)
  drafts_folder: string;        (** Drafts folder *)
  outbox_folder: string;        (** Outbox for sending *)
}

(** Sync configuration *)
type sync_config = {
  auto_sync: bool;              (** Enable automatic sync *)
  sync_interval_secs: int;      (** Seconds between sync attempts *)
  conflict_strategy: [`LocalWins | `RemoteWins | `Manual];
}

(** UI configuration *)
type ui_config = {
  theme: string;                (** Color theme name *)
  date_format: string;          (** Date display format *)
  show_completed: bool;         (** Show completed tasks *)
  default_view: string;         (** Default view on startup *)
}

(** Full application configuration *)
type t = {
  device_id: string;            (** Unique device identifier *)
  device_name: string;          (** Human-readable device name *)
  db: db_config;
  mail: mail_config;
  sync: sync_config;
  ui: ui_config;
}

(** Default configuration *)
let default () =
  let home = Sys.getenv_opt "HOME" |> Option.value ~default:"~" in
  let data_dir = Filename.concat home ".local/share/parenvault" in
  {
    device_id = "";  (* Generated on first run *)
    device_name = Unix.gethostname ();
    db = {
      local_path = Filename.concat data_dir "parenvault.db";
      remote_host = None;
      remote_port = 5432;
      remote_name = Some "parenvault";
      remote_user = None;
      remote_password = None;
    };
    mail = {
      maildir_path = Filename.concat home "Mail";
      inbox_folder = "INBOX";
      action_folder = "ParenVault";
      processed_folder = "ParenVault/Processed";
      drafts_folder = "Drafts";
      outbox_folder = "Outbox";
    };
    sync = {
      auto_sync = true;
      sync_interval_secs = 300;  (* 5 minutes *)
      conflict_strategy = `RemoteWins;
    };
    ui = {
      theme = "default";
      date_format = "%Y-%m-%d";
      show_completed = false;
      default_view = "dashboard";
    };
  }

(** Config file path *)
let config_path () =
  let home = Sys.getenv_opt "HOME" |> Option.value ~default:"~" in
  Filename.concat home ".config/parenvault/config.toml"

(** Generate PostgreSQL connection URI *)
let postgres_uri config =
  match config.db.remote_host, config.db.remote_user, config.db.remote_name with
  | Some host, Some user, Some name ->
    let password = Option.value ~default:"" config.db.remote_password in
    Some (Printf.sprintf "postgresql://%s:%s@%s:%d/%s"
      user password host config.db.remote_port name)
  | _ -> None

(** Parse config from TOML file *)
let load path =
  if not (Sys.file_exists path) then
    Ok (default ())
  else
    try
      let toml = Toml.Parser.from_filename path in
      match toml with
      | `Ok tbl ->
        let get_string section key default =
          match Toml.Types.Table.find_opt (Toml.Min.key section) tbl with
          | Some (Toml.Types.TTable t) ->
            (match Toml.Types.Table.find_opt (Toml.Min.key key) t with
             | Some (Toml.Types.TString s) -> s
             | _ -> default)
          | _ -> default
        in
        let get_string_opt section key =
          match Toml.Types.Table.find_opt (Toml.Min.key section) tbl with
          | Some (Toml.Types.TTable t) ->
            (match Toml.Types.Table.find_opt (Toml.Min.key key) t with
             | Some (Toml.Types.TString s) -> Some s
             | _ -> None)
          | _ -> None
        in
        let get_int section key default =
          match Toml.Types.Table.find_opt (Toml.Min.key section) tbl with
          | Some (Toml.Types.TTable t) ->
            (match Toml.Types.Table.find_opt (Toml.Min.key key) t with
             | Some (Toml.Types.TInt i) -> i
             | _ -> default)
          | _ -> default
        in
        let get_bool section key default =
          match Toml.Types.Table.find_opt (Toml.Min.key section) tbl with
          | Some (Toml.Types.TTable t) ->
            (match Toml.Types.Table.find_opt (Toml.Min.key key) t with
             | Some (Toml.Types.TBool b) -> b
             | _ -> default)
          | _ -> default
        in
        let def = default () in
        (* Check environment variable for password first, then config file *)
        let remote_password = 
          match Sys.getenv_opt "PARENVAULT_DB_PASSWORD" with
          | Some pw -> Some pw
          | None -> get_string_opt "database" "remote_password"
        in
        Ok {
          device_id = get_string "device" "id" def.device_id;
          device_name = get_string "device" "name" def.device_name;
          db = {
            local_path = get_string "database" "local_path" def.db.local_path;
            remote_host = get_string_opt "database" "remote_host";
            remote_port = get_int "database" "remote_port" def.db.remote_port;
            remote_name = get_string_opt "database" "remote_name";
            remote_user = get_string_opt "database" "remote_user";
            remote_password;
          };
          mail = {
            maildir_path = get_string "mail" "maildir_path" def.mail.maildir_path;
            inbox_folder = get_string "mail" "inbox_folder" def.mail.inbox_folder;
            action_folder = get_string "mail" "action_folder" def.mail.action_folder;
            processed_folder = get_string "mail" "processed_folder" def.mail.processed_folder;
            drafts_folder = get_string "mail" "drafts_folder" def.mail.drafts_folder;
            outbox_folder = get_string "mail" "outbox_folder" def.mail.outbox_folder;
          };
          sync = {
            auto_sync = get_bool "sync" "auto_sync" def.sync.auto_sync;
            sync_interval_secs = get_int "sync" "interval_secs" def.sync.sync_interval_secs;
            conflict_strategy = def.sync.conflict_strategy;  (* TODO: parse *)
          };
          ui = {
            theme = get_string "ui" "theme" def.ui.theme;
            date_format = get_string "ui" "date_format" def.ui.date_format;
            show_completed = get_bool "ui" "show_completed" def.ui.show_completed;
            default_view = get_string "ui" "default_view" def.ui.default_view;
          };
        }
      | `Error (msg, _) -> Error msg
    with e -> Error (Printexc.to_string e)

(** Save config to TOML file *)
let save config path =
  let content = Printf.sprintf {|# ParenVault Configuration

[device]
id = "%s"
name = "%s"

[database]
local_path = "%s"
%s
remote_port = %d
%s
%s
%s

[mail]
maildir_path = "%s"
inbox_folder = "%s"
action_folder = "%s"
processed_folder = "%s"
drafts_folder = "%s"
outbox_folder = "%s"

[sync]
auto_sync = %b
interval_secs = %d

[ui]
theme = "%s"
date_format = "%s"
show_completed = %b
default_view = "%s"
|}
    config.device_id
    config.device_name
    config.db.local_path
    (match config.db.remote_host with Some h -> Printf.sprintf "remote_host = \"%s\"" h | None -> "# remote_host = \"\"")
    config.db.remote_port
    (match config.db.remote_name with Some n -> Printf.sprintf "remote_name = \"%s\"" n | None -> "# remote_name = \"\"")
    (match config.db.remote_user with Some u -> Printf.sprintf "remote_user = \"%s\"" u | None -> "# remote_user = \"\"")
    (match config.db.remote_password with Some _ -> "# remote_password = \"***\"  # Set via environment variable" | None -> "# remote_password = \"\"")
    config.mail.maildir_path
    config.mail.inbox_folder
    config.mail.action_folder
    config.mail.processed_folder
    config.mail.drafts_folder
    config.mail.outbox_folder
    config.sync.auto_sync
    config.sync.sync_interval_secs
    config.ui.theme
    config.ui.date_format
    config.ui.show_completed
    config.ui.default_view
  in
  (* Ensure directory exists *)
  let dir = Filename.dirname path in
  if not (Sys.file_exists dir) then
    Unix.mkdir dir 0o755;
  let oc = open_out path in
  output_string oc content;
  close_out oc
