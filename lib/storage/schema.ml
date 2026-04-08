(** Database schema definitions and migrations *)

open Lwt.Syntax

(** Schema version *)
let current_version = 1

(** SQLite schema creation statements *)
let sqlite_schema = {|
-- Schema version tracking
CREATE TABLE IF NOT EXISTS schema_version (
  version INTEGER PRIMARY KEY,
  applied_at TEXT NOT NULL DEFAULT (datetime('now'))
);

-- Device identity for sync
CREATE TABLE IF NOT EXISTS device (
  id TEXT PRIMARY KEY,
  name TEXT NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);

-- Projects/Areas
CREATE TABLE IF NOT EXISTS projects (
  id TEXT PRIMARY KEY,
  name TEXT NOT NULL,
  description TEXT,
  status TEXT NOT NULL DEFAULT 'active',
  tags TEXT NOT NULL DEFAULT '[]',
  parent_id TEXT REFERENCES projects(id),
  created_at TEXT NOT NULL,
  -- Sync metadata
  sync_local_id TEXT NOT NULL,
  sync_version INTEGER NOT NULL DEFAULT 1,
  sync_modified_at TEXT NOT NULL,
  sync_synced_at TEXT,
  sync_deleted INTEGER NOT NULL DEFAULT 0
);

-- Tasks
CREATE TABLE IF NOT EXISTS tasks (
  id TEXT PRIMARY KEY,
  title TEXT NOT NULL,
  description TEXT,
  status TEXT NOT NULL DEFAULT 'inbox',
  priority TEXT NOT NULL DEFAULT 'p2',
  tags TEXT NOT NULL DEFAULT '[]',
  due_date TEXT,
  scheduled_date TEXT,
  completed_at TEXT,
  recurrence TEXT,
  parent_id TEXT REFERENCES tasks(id),
  project_id TEXT REFERENCES projects(id),
  block_start TEXT,
  block_end TEXT,
  created_at TEXT NOT NULL,
  -- Sync metadata
  sync_local_id TEXT NOT NULL,
  sync_version INTEGER NOT NULL DEFAULT 1,
  sync_modified_at TEXT NOT NULL,
  sync_synced_at TEXT,
  sync_deleted INTEGER NOT NULL DEFAULT 0
);

-- Notes
CREATE TABLE IF NOT EXISTS notes (
  id TEXT PRIMARY KEY,
  title TEXT NOT NULL,
  content TEXT NOT NULL DEFAULT '',
  tags TEXT NOT NULL DEFAULT '[]',
  project_id TEXT REFERENCES projects(id),
  created_at TEXT NOT NULL,
  -- Sync metadata
  sync_local_id TEXT NOT NULL,
  sync_version INTEGER NOT NULL DEFAULT 1,
  sync_modified_at TEXT NOT NULL,
  sync_synced_at TEXT,
  sync_deleted INTEGER NOT NULL DEFAULT 0
);

-- Events
CREATE TABLE IF NOT EXISTS events (
  id TEXT PRIMARY KEY,
  title TEXT NOT NULL,
  description TEXT,
  start_time TEXT NOT NULL,
  end_time TEXT,
  location TEXT,
  tags TEXT NOT NULL DEFAULT '[]',
  recurrence TEXT,
  created_at TEXT NOT NULL,
  -- Sync metadata
  sync_local_id TEXT NOT NULL,
  sync_version INTEGER NOT NULL DEFAULT 1,
  sync_modified_at TEXT NOT NULL,
  sync_synced_at TEXT,
  sync_deleted INTEGER NOT NULL DEFAULT 0
);

-- Companies/Organizations
CREATE TABLE IF NOT EXISTS companies (
  id TEXT PRIMARY KEY,
  name TEXT NOT NULL,
  website TEXT,
  industry TEXT,
  address TEXT,
  phone TEXT,
  email TEXT,
  notes TEXT,
  tags TEXT NOT NULL DEFAULT '[]',
  created_at TEXT NOT NULL,
  -- Sync metadata
  sync_local_id TEXT NOT NULL,
  sync_version INTEGER NOT NULL DEFAULT 1,
  sync_modified_at TEXT NOT NULL,
  sync_synced_at TEXT,
  sync_deleted INTEGER NOT NULL DEFAULT 0
);

-- Deals/Opportunities
CREATE TABLE IF NOT EXISTS deals (
  id TEXT PRIMARY KEY,
  name TEXT NOT NULL,
  company_id TEXT,
  contact_id TEXT,
  stage TEXT NOT NULL DEFAULT 'lead',
  value REAL,
  currency TEXT NOT NULL DEFAULT 'GBP',
  expected_close_date TEXT,
  notes TEXT,
  tags TEXT NOT NULL DEFAULT '[]',
  created_at TEXT NOT NULL,
  -- Sync metadata
  sync_local_id TEXT NOT NULL,
  sync_version INTEGER NOT NULL DEFAULT 1,
  sync_modified_at TEXT NOT NULL,
  sync_synced_at TEXT,
  sync_deleted INTEGER NOT NULL DEFAULT 0
);

-- Activities/Interactions
CREATE TABLE IF NOT EXISTS activities (
  id TEXT PRIMARY KEY,
  kind TEXT NOT NULL DEFAULT 'other',
  subject TEXT NOT NULL,
  description TEXT,
  contact_id TEXT,
  company_id TEXT,
  deal_id TEXT,
  activity_date TEXT NOT NULL,
  duration_minutes INTEGER,
  tags TEXT NOT NULL DEFAULT '[]',
  created_at TEXT NOT NULL,
  -- Sync metadata
  sync_local_id TEXT NOT NULL,
  sync_version INTEGER NOT NULL DEFAULT 1,
  sync_modified_at TEXT NOT NULL,
  sync_synced_at TEXT,
  sync_deleted INTEGER NOT NULL DEFAULT 0
);

-- Contacts
CREATE TABLE IF NOT EXISTS contacts (
  id TEXT PRIMARY KEY,
  name TEXT NOT NULL,
  email TEXT,
  notes TEXT,
  tags TEXT NOT NULL DEFAULT '[]',
  created_at TEXT NOT NULL,
  -- Sync metadata
  sync_local_id TEXT NOT NULL,
  sync_version INTEGER NOT NULL DEFAULT 1,
  sync_modified_at TEXT NOT NULL,
  sync_synced_at TEXT,
  sync_deleted INTEGER NOT NULL DEFAULT 0
);

-- Time entries (billable hours tracking)
CREATE TABLE IF NOT EXISTS time_entries (
  id TEXT PRIMARY KEY,
  description TEXT NOT NULL,
  date TEXT NOT NULL,
  start_time TEXT,
  end_time TEXT,
  duration_minutes INTEGER NOT NULL,
  billable INTEGER NOT NULL DEFAULT 1,
  rate REAL,
  currency TEXT NOT NULL DEFAULT 'USD',
  project_id TEXT REFERENCES projects(id),
  task_id TEXT REFERENCES tasks(id),
  deal_id TEXT REFERENCES deals(id),
  company_id TEXT REFERENCES companies(id),
  contact_id TEXT REFERENCES contacts(id),
  tags TEXT NOT NULL DEFAULT '[]',
  created_at TEXT NOT NULL,
  -- Sync metadata
  sync_local_id TEXT NOT NULL,
  sync_version INTEGER NOT NULL DEFAULT 1,
  sync_modified_at TEXT NOT NULL,
  sync_synced_at TEXT,
  sync_deleted INTEGER NOT NULL DEFAULT 0
);

-- Email references (aerc integration)
CREATE TABLE IF NOT EXISTS email_refs (
  id TEXT PRIMARY KEY,
  message_id TEXT NOT NULL UNIQUE,
  subject TEXT NOT NULL,
  from_addr TEXT NOT NULL,
  to_addrs TEXT NOT NULL DEFAULT '[]',
  date TEXT NOT NULL,
  maildir_path TEXT,
  linked_task_id TEXT REFERENCES tasks(id),
  linked_note_id TEXT REFERENCES notes(id),
  created_at TEXT NOT NULL,
  -- Sync metadata
  sync_local_id TEXT NOT NULL,
  sync_version INTEGER NOT NULL DEFAULT 1,
  sync_modified_at TEXT NOT NULL,
  sync_synced_at TEXT,
  sync_deleted INTEGER NOT NULL DEFAULT 0
);

-- Links between entities
CREATE TABLE IF NOT EXISTS links (
  id TEXT PRIMARY KEY,
  source_type TEXT NOT NULL,
  source_id TEXT NOT NULL,
  target_type TEXT NOT NULL,
  target_id TEXT NOT NULL,
  link_type TEXT NOT NULL,
  created_at TEXT NOT NULL,
  -- Sync metadata
  sync_local_id TEXT NOT NULL,
  sync_version INTEGER NOT NULL DEFAULT 1,
  sync_modified_at TEXT NOT NULL,
  sync_synced_at TEXT,
  sync_deleted INTEGER NOT NULL DEFAULT 0,
  UNIQUE(source_type, source_id, target_type, target_id, link_type)
);

-- Attachments
CREATE TABLE IF NOT EXISTS attachments (
  id TEXT PRIMARY KEY,
  filename TEXT NOT NULL,
  filepath TEXT NOT NULL,
  mime_type TEXT,
  size_bytes INTEGER NOT NULL,
  entity_type TEXT NOT NULL,
  entity_id TEXT NOT NULL,
  created_at TEXT NOT NULL,
  -- Sync metadata
  sync_local_id TEXT NOT NULL,
  sync_version INTEGER NOT NULL DEFAULT 1,
  sync_modified_at TEXT NOT NULL,
  sync_synced_at TEXT,
  sync_deleted INTEGER NOT NULL DEFAULT 0
);

-- Sync queue for pending changes
CREATE TABLE IF NOT EXISTS sync_queue (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  entity_type TEXT NOT NULL,
  entity_id TEXT NOT NULL,
  operation TEXT NOT NULL,  -- 'insert', 'update', 'delete'
  queued_at TEXT NOT NULL DEFAULT (datetime('now')),
  attempts INTEGER NOT NULL DEFAULT 0,
  last_error TEXT,
  UNIQUE(entity_type, entity_id)
);

-- Indexes for common queries
CREATE INDEX IF NOT EXISTS idx_tasks_status ON tasks(status) WHERE sync_deleted = 0;
CREATE INDEX IF NOT EXISTS idx_tasks_due_date ON tasks(due_date) WHERE sync_deleted = 0;
CREATE INDEX IF NOT EXISTS idx_tasks_project ON tasks(project_id) WHERE sync_deleted = 0;
CREATE INDEX IF NOT EXISTS idx_notes_project ON notes(project_id) WHERE sync_deleted = 0;
CREATE INDEX IF NOT EXISTS idx_events_start ON events(start_time) WHERE sync_deleted = 0;
CREATE INDEX IF NOT EXISTS idx_email_refs_message_id ON email_refs(message_id);
CREATE INDEX IF NOT EXISTS idx_sync_queue_pending ON sync_queue(queued_at);
CREATE INDEX IF NOT EXISTS idx_companies_name ON companies(name) WHERE sync_deleted = 0;
CREATE INDEX IF NOT EXISTS idx_deals_stage ON deals(stage) WHERE sync_deleted = 0;
CREATE INDEX IF NOT EXISTS idx_deals_company ON deals(company_id) WHERE sync_deleted = 0;
CREATE INDEX IF NOT EXISTS idx_activities_kind ON activities(kind) WHERE sync_deleted = 0;
CREATE INDEX IF NOT EXISTS idx_activities_contact ON activities(contact_id) WHERE sync_deleted = 0;
CREATE INDEX IF NOT EXISTS idx_activities_company ON activities(company_id) WHERE sync_deleted = 0;
CREATE INDEX IF NOT EXISTS idx_activities_deal ON activities(deal_id) WHERE sync_deleted = 0;
CREATE INDEX IF NOT EXISTS idx_activities_date ON activities(activity_date) WHERE sync_deleted = 0;
CREATE INDEX IF NOT EXISTS idx_time_entries_date ON time_entries(date) WHERE sync_deleted = 0;
CREATE INDEX IF NOT EXISTS idx_time_entries_project ON time_entries(project_id) WHERE sync_deleted = 0;
CREATE INDEX IF NOT EXISTS idx_time_entries_company ON time_entries(company_id) WHERE sync_deleted = 0;
CREATE INDEX IF NOT EXISTS idx_time_entries_billable ON time_entries(billable) WHERE sync_deleted = 0;
|}

(** PostgreSQL schema (similar but with PostgreSQL-specific types) *)
let postgres_schema = {|
-- Schema version tracking
CREATE TABLE IF NOT EXISTS schema_version (
  version INTEGER PRIMARY KEY,
  applied_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Projects/Areas
CREATE TABLE IF NOT EXISTS projects (
  id TEXT PRIMARY KEY,
  name TEXT NOT NULL,
  description TEXT,
  status TEXT NOT NULL DEFAULT 'active',
  tags JSONB NOT NULL DEFAULT '[]',
  parent_id TEXT REFERENCES projects(id),
  created_at TIMESTAMPTZ NOT NULL,
  -- Sync metadata
  sync_local_id TEXT NOT NULL,
  sync_version INTEGER NOT NULL DEFAULT 1,
  sync_modified_at TIMESTAMPTZ NOT NULL,
  sync_synced_at TIMESTAMPTZ,
  sync_deleted BOOLEAN NOT NULL DEFAULT FALSE
);

-- Tasks
CREATE TABLE IF NOT EXISTS tasks (
  id TEXT PRIMARY KEY,
  title TEXT NOT NULL,
  description TEXT,
  status TEXT NOT NULL DEFAULT 'inbox',
  priority TEXT NOT NULL DEFAULT 'p2',
  tags JSONB NOT NULL DEFAULT '[]',
  due_date TIMESTAMPTZ,
  scheduled_date TIMESTAMPTZ,
  completed_at TIMESTAMPTZ,
  recurrence TEXT,
  parent_id TEXT REFERENCES tasks(id),
  project_id TEXT REFERENCES projects(id),
  block_start TIMESTAMPTZ,
  block_end TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL,
  -- Sync metadata
  sync_local_id TEXT NOT NULL,
  sync_version INTEGER NOT NULL DEFAULT 1,
  sync_modified_at TIMESTAMPTZ NOT NULL,
  sync_synced_at TIMESTAMPTZ,
  sync_deleted BOOLEAN NOT NULL DEFAULT FALSE
);

-- Notes
CREATE TABLE IF NOT EXISTS notes (
  id TEXT PRIMARY KEY,
  title TEXT NOT NULL,
  content TEXT NOT NULL DEFAULT '',
  tags JSONB NOT NULL DEFAULT '[]',
  project_id TEXT REFERENCES projects(id),
  created_at TIMESTAMPTZ NOT NULL,
  -- Sync metadata
  sync_local_id TEXT NOT NULL,
  sync_version INTEGER NOT NULL DEFAULT 1,
  sync_modified_at TIMESTAMPTZ NOT NULL,
  sync_synced_at TIMESTAMPTZ,
  sync_deleted BOOLEAN NOT NULL DEFAULT FALSE
);

-- Events
CREATE TABLE IF NOT EXISTS events (
  id TEXT PRIMARY KEY,
  title TEXT NOT NULL,
  description TEXT,
  start_time TIMESTAMPTZ NOT NULL,
  end_time TIMESTAMPTZ,
  location TEXT,
  tags JSONB NOT NULL DEFAULT '[]',
  recurrence TEXT,
  created_at TIMESTAMPTZ NOT NULL,
  -- Sync metadata
  sync_local_id TEXT NOT NULL,
  sync_version INTEGER NOT NULL DEFAULT 1,
  sync_modified_at TIMESTAMPTZ NOT NULL,
  sync_synced_at TIMESTAMPTZ,
  sync_deleted BOOLEAN NOT NULL DEFAULT FALSE
);

-- Companies/Organizations
CREATE TABLE IF NOT EXISTS companies (
  id TEXT PRIMARY KEY,
  name TEXT NOT NULL,
  website TEXT,
  industry TEXT,
  address TEXT,
  phone TEXT,
  email TEXT,
  notes TEXT,
  tags JSONB NOT NULL DEFAULT '[]',
  created_at TIMESTAMPTZ NOT NULL,
  -- Sync metadata
  sync_local_id TEXT NOT NULL,
  sync_version INTEGER NOT NULL DEFAULT 1,
  sync_modified_at TIMESTAMPTZ NOT NULL,
  sync_synced_at TIMESTAMPTZ,
  sync_deleted BOOLEAN NOT NULL DEFAULT FALSE
);

-- Deals/Opportunities
CREATE TABLE IF NOT EXISTS deals (
  id TEXT PRIMARY KEY,
  name TEXT NOT NULL,
  company_id TEXT,
  contact_id TEXT,
  stage TEXT NOT NULL DEFAULT 'lead',
  value DOUBLE PRECISION,
  currency TEXT NOT NULL DEFAULT 'GBP',
  expected_close_date TIMESTAMPTZ,
  notes TEXT,
  tags JSONB NOT NULL DEFAULT '[]',
  created_at TIMESTAMPTZ NOT NULL,
  -- Sync metadata
  sync_local_id TEXT NOT NULL,
  sync_version INTEGER NOT NULL DEFAULT 1,
  sync_modified_at TIMESTAMPTZ NOT NULL,
  sync_synced_at TIMESTAMPTZ,
  sync_deleted BOOLEAN NOT NULL DEFAULT FALSE
);

-- Activities/Interactions
CREATE TABLE IF NOT EXISTS activities (
  id TEXT PRIMARY KEY,
  kind TEXT NOT NULL DEFAULT 'other',
  subject TEXT NOT NULL,
  description TEXT,
  contact_id TEXT,
  company_id TEXT,
  deal_id TEXT,
  activity_date TIMESTAMPTZ NOT NULL,
  duration_minutes INTEGER,
  tags JSONB NOT NULL DEFAULT '[]',
  created_at TIMESTAMPTZ NOT NULL,
  -- Sync metadata
  sync_local_id TEXT NOT NULL,
  sync_version INTEGER NOT NULL DEFAULT 1,
  sync_modified_at TIMESTAMPTZ NOT NULL,
  sync_synced_at TIMESTAMPTZ,
  sync_deleted BOOLEAN NOT NULL DEFAULT FALSE
);

-- Contacts
CREATE TABLE IF NOT EXISTS contacts (
  id TEXT PRIMARY KEY,
  name TEXT NOT NULL,
  email TEXT,
  notes TEXT,
  tags JSONB NOT NULL DEFAULT '[]',
  created_at TIMESTAMPTZ NOT NULL,
  -- Sync metadata
  sync_local_id TEXT NOT NULL,
  sync_version INTEGER NOT NULL DEFAULT 1,
  sync_modified_at TIMESTAMPTZ NOT NULL,
  sync_synced_at TIMESTAMPTZ,
  sync_deleted BOOLEAN NOT NULL DEFAULT FALSE
);

-- Time entries (billable hours tracking)
CREATE TABLE IF NOT EXISTS time_entries (
  id TEXT PRIMARY KEY,
  description TEXT NOT NULL,
  date TIMESTAMPTZ NOT NULL,
  start_time TIMESTAMPTZ,
  end_time TIMESTAMPTZ,
  duration_minutes INTEGER NOT NULL,
  billable BOOLEAN NOT NULL DEFAULT TRUE,
  rate DOUBLE PRECISION,
  currency TEXT NOT NULL DEFAULT 'USD',
  project_id TEXT REFERENCES projects(id),
  task_id TEXT REFERENCES tasks(id),
  deal_id TEXT REFERENCES deals(id),
  company_id TEXT REFERENCES companies(id),
  contact_id TEXT REFERENCES contacts(id),
  tags JSONB NOT NULL DEFAULT '[]',
  created_at TIMESTAMPTZ NOT NULL,
  -- Sync metadata
  sync_local_id TEXT NOT NULL,
  sync_version INTEGER NOT NULL DEFAULT 1,
  sync_modified_at TIMESTAMPTZ NOT NULL,
  sync_synced_at TIMESTAMPTZ,
  sync_deleted BOOLEAN NOT NULL DEFAULT FALSE
);

-- Email references (aerc integration)
CREATE TABLE IF NOT EXISTS email_refs (
  id TEXT PRIMARY KEY,
  message_id TEXT NOT NULL UNIQUE,
  subject TEXT NOT NULL,
  from_addr TEXT NOT NULL,
  to_addrs JSONB NOT NULL DEFAULT '[]',
  date TIMESTAMPTZ NOT NULL,
  maildir_path TEXT,
  linked_task_id TEXT REFERENCES tasks(id),
  linked_note_id TEXT REFERENCES notes(id),
  created_at TIMESTAMPTZ NOT NULL,
  -- Sync metadata
  sync_local_id TEXT NOT NULL,
  sync_version INTEGER NOT NULL DEFAULT 1,
  sync_modified_at TIMESTAMPTZ NOT NULL,
  sync_synced_at TIMESTAMPTZ,
  sync_deleted BOOLEAN NOT NULL DEFAULT FALSE
);

-- Links between entities
CREATE TABLE IF NOT EXISTS links (
  id TEXT PRIMARY KEY,
  source_type TEXT NOT NULL,
  source_id TEXT NOT NULL,
  target_type TEXT NOT NULL,
  target_id TEXT NOT NULL,
  link_type TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL,
  -- Sync metadata
  sync_local_id TEXT NOT NULL,
  sync_version INTEGER NOT NULL DEFAULT 1,
  sync_modified_at TIMESTAMPTZ NOT NULL,
  sync_synced_at TIMESTAMPTZ,
  sync_deleted BOOLEAN NOT NULL DEFAULT FALSE,
  UNIQUE(source_type, source_id, target_type, target_id, link_type)
);

-- Attachments
CREATE TABLE IF NOT EXISTS attachments (
  id TEXT PRIMARY KEY,
  filename TEXT NOT NULL,
  filepath TEXT NOT NULL,
  mime_type TEXT,
  size_bytes BIGINT NOT NULL,
  entity_type TEXT NOT NULL,
  entity_id TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL,
  -- Sync metadata
  sync_local_id TEXT NOT NULL,
  sync_version INTEGER NOT NULL DEFAULT 1,
  sync_modified_at TIMESTAMPTZ NOT NULL,
  sync_synced_at TIMESTAMPTZ,
  sync_deleted BOOLEAN NOT NULL DEFAULT FALSE
);

-- Indexes
CREATE INDEX IF NOT EXISTS idx_tasks_status ON tasks(status) WHERE NOT sync_deleted;
CREATE INDEX IF NOT EXISTS idx_tasks_due_date ON tasks(due_date) WHERE NOT sync_deleted;
CREATE INDEX IF NOT EXISTS idx_tasks_project ON tasks(project_id) WHERE NOT sync_deleted;
CREATE INDEX IF NOT EXISTS idx_notes_project ON notes(project_id) WHERE NOT sync_deleted;
CREATE INDEX IF NOT EXISTS idx_events_start ON events(start_time) WHERE NOT sync_deleted;
CREATE INDEX IF NOT EXISTS idx_email_refs_message_id ON email_refs(message_id);
CREATE INDEX IF NOT EXISTS idx_companies_name ON companies(name) WHERE NOT sync_deleted;
CREATE INDEX IF NOT EXISTS idx_deals_stage ON deals(stage) WHERE NOT sync_deleted;
CREATE INDEX IF NOT EXISTS idx_deals_company ON deals(company_id) WHERE NOT sync_deleted;
CREATE INDEX IF NOT EXISTS idx_activities_kind ON activities(kind) WHERE NOT sync_deleted;
CREATE INDEX IF NOT EXISTS idx_activities_contact ON activities(contact_id) WHERE NOT sync_deleted;
CREATE INDEX IF NOT EXISTS idx_activities_company ON activities(company_id) WHERE NOT sync_deleted;
CREATE INDEX IF NOT EXISTS idx_activities_deal ON activities(deal_id) WHERE NOT sync_deleted;
CREATE INDEX IF NOT EXISTS idx_activities_date ON activities(activity_date) WHERE NOT sync_deleted;
CREATE INDEX IF NOT EXISTS idx_time_entries_date ON time_entries(date) WHERE NOT sync_deleted;
CREATE INDEX IF NOT EXISTS idx_time_entries_project ON time_entries(project_id) WHERE NOT sync_deleted;
CREATE INDEX IF NOT EXISTS idx_time_entries_company ON time_entries(company_id) WHERE NOT sync_deleted;
CREATE INDEX IF NOT EXISTS idx_time_entries_billable ON time_entries(billable) WHERE NOT sync_deleted;
|}

(** Migration SQL for adding time blocking columns to existing databases *)
let sqlite_migrations = {|
ALTER TABLE tasks ADD COLUMN block_start TEXT;
ALTER TABLE tasks ADD COLUMN block_end TEXT;
CREATE TABLE IF NOT EXISTS time_entries (
  id TEXT PRIMARY KEY,
  description TEXT NOT NULL,
  date TEXT NOT NULL,
  start_time TEXT,
  end_time TEXT,
  duration_minutes INTEGER NOT NULL,
  billable INTEGER NOT NULL DEFAULT 1,
  rate REAL,
  currency TEXT NOT NULL DEFAULT 'USD',
  project_id TEXT REFERENCES projects(id),
  task_id TEXT REFERENCES tasks(id),
  deal_id TEXT REFERENCES deals(id),
  company_id TEXT REFERENCES companies(id),
  contact_id TEXT REFERENCES contacts(id),
  tags TEXT NOT NULL DEFAULT '[]',
  created_at TEXT NOT NULL,
  sync_local_id TEXT NOT NULL,
  sync_version INTEGER NOT NULL DEFAULT 1,
  sync_modified_at TEXT NOT NULL,
  sync_synced_at TEXT,
  sync_deleted INTEGER NOT NULL DEFAULT 0
);
CREATE INDEX IF NOT EXISTS idx_time_entries_date ON time_entries(date) WHERE sync_deleted = 0;
CREATE INDEX IF NOT EXISTS idx_time_entries_project ON time_entries(project_id) WHERE sync_deleted = 0;
CREATE INDEX IF NOT EXISTS idx_time_entries_company ON time_entries(company_id) WHERE sync_deleted = 0;
CREATE INDEX IF NOT EXISTS idx_time_entries_billable ON time_entries(billable) WHERE sync_deleted = 0
|}

let postgres_migrations = {|
ALTER TABLE tasks ADD COLUMN IF NOT EXISTS block_start TIMESTAMPTZ;
ALTER TABLE tasks ADD COLUMN IF NOT EXISTS block_end TIMESTAMPTZ;
CREATE TABLE IF NOT EXISTS time_entries (
  id TEXT PRIMARY KEY,
  description TEXT NOT NULL,
  date TIMESTAMPTZ NOT NULL,
  start_time TIMESTAMPTZ,
  end_time TIMESTAMPTZ,
  duration_minutes INTEGER NOT NULL,
  billable BOOLEAN NOT NULL DEFAULT TRUE,
  rate DOUBLE PRECISION,
  currency TEXT NOT NULL DEFAULT 'USD',
  project_id TEXT REFERENCES projects(id),
  task_id TEXT REFERENCES tasks(id),
  deal_id TEXT REFERENCES deals(id),
  company_id TEXT REFERENCES companies(id),
  contact_id TEXT REFERENCES contacts(id),
  tags JSONB NOT NULL DEFAULT '[]',
  created_at TIMESTAMPTZ NOT NULL,
  sync_local_id TEXT NOT NULL,
  sync_version INTEGER NOT NULL DEFAULT 1,
  sync_modified_at TIMESTAMPTZ NOT NULL,
  sync_synced_at TIMESTAMPTZ,
  sync_deleted BOOLEAN NOT NULL DEFAULT FALSE
);
CREATE INDEX IF NOT EXISTS idx_time_entries_date ON time_entries(date) WHERE NOT sync_deleted;
CREATE INDEX IF NOT EXISTS idx_time_entries_project ON time_entries(project_id) WHERE NOT sync_deleted;
CREATE INDEX IF NOT EXISTS idx_time_entries_company ON time_entries(company_id) WHERE NOT sync_deleted;
CREATE INDEX IF NOT EXISTS idx_time_entries_billable ON time_entries(billable) WHERE NOT sync_deleted
|}

(** Run schema migration on a connection *)
let migrate_sqlite pool =
  let exec_sql sql =
    Caqti_lwt_unix.Pool.use (fun (module C : Caqti_lwt.CONNECTION) ->
      (* Split by semicolons and execute each statement *)
      let statements = String.split_on_char ';' sql in
      let rec exec_all = function
        | [] -> Lwt.return (Ok ())
        | stmt :: rest ->
          let stmt = String.trim stmt in
          if stmt = "" then exec_all rest
          else
            let query = Caqti_request.Infix.(Caqti_type.unit ->. Caqti_type.unit) stmt in
            let* result = C.exec query () in
            match result with
            | Ok () -> exec_all rest
            | Error e -> Lwt.return (Error e)
      in
      exec_all statements
    ) pool
  in
  let exec_migration sql =
    Caqti_lwt_unix.Pool.use (fun (module C : Caqti_lwt.CONNECTION) ->
      let statements = String.split_on_char ';' sql in
      let rec exec_all = function
        | [] -> Lwt.return (Ok ())
        | stmt :: rest ->
          let stmt = String.trim stmt in
          if stmt = "" then exec_all rest
          else
            let query = Caqti_request.Infix.(Caqti_type.unit ->. Caqti_type.unit) stmt in
            let* result = C.exec query () in
            match result with
            | Ok () -> exec_all rest
            | Error _ -> exec_all rest  (* Ignore errors for migrations - column may already exist *)
      in
      exec_all statements
    ) pool
  in
  let* _ = exec_sql sqlite_schema in
  exec_migration sqlite_migrations

let migrate_postgres pool =
  let exec_sql sql =
    Caqti_lwt_unix.Pool.use (fun (module C : Caqti_lwt.CONNECTION) ->
      let statements = String.split_on_char ';' sql in
      let rec exec_all = function
        | [] -> Lwt.return (Ok ())
        | stmt :: rest ->
          let stmt = String.trim stmt in
          if stmt = "" then exec_all rest
          else
            let query = Caqti_request.Infix.(Caqti_type.unit ->. Caqti_type.unit) stmt in
            let* result = C.exec query () in
            match result with
            | Ok () -> exec_all rest
            | Error e -> Lwt.return (Error e)
      in
      exec_all statements
    ) pool
  in
  let exec_migration sql =
    Caqti_lwt_unix.Pool.use (fun (module C : Caqti_lwt.CONNECTION) ->
      let statements = String.split_on_char ';' sql in
      let rec exec_all = function
        | [] -> Lwt.return (Ok ())
        | stmt :: rest ->
          let stmt = String.trim stmt in
          if stmt = "" then exec_all rest
          else
            let query = Caqti_request.Infix.(Caqti_type.unit ->. Caqti_type.unit) stmt in
            let* result = C.exec query () in
            match result with
            | Ok () -> exec_all rest
            | Error _ -> exec_all rest  (* Ignore errors - column may already exist *)
      in
      exec_all statements
    ) pool
  in
  let* _ = exec_sql postgres_schema in
  exec_migration postgres_migrations
