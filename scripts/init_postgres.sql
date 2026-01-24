-- ParenVault PostgreSQL Schema
-- Run: psql -h gt-nas -U root -d parenvault -f init_postgres.sql

CREATE TABLE IF NOT EXISTS schema_version (
    version INTEGER PRIMARY KEY,
    applied_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS devices (
    id TEXT PRIMARY KEY,
    name TEXT NOT NULL,
    last_seen TIMESTAMPTZ DEFAULT NOW(),
    created_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS projects (
    id TEXT PRIMARY KEY,
    name TEXT NOT NULL,
    description TEXT,
    status TEXT DEFAULT 'active',
    tags TEXT DEFAULT '[]',
    parent_id TEXT REFERENCES projects(id),
    created_at TIMESTAMPTZ DEFAULT NOW(),
    sync_local_id TEXT NOT NULL,
    sync_version INTEGER DEFAULT 1,
    sync_modified_at TIMESTAMPTZ DEFAULT NOW(),
    sync_synced_at TIMESTAMPTZ,
    sync_deleted BOOLEAN DEFAULT FALSE
);

CREATE TABLE IF NOT EXISTS tasks (
    id TEXT PRIMARY KEY,
    title TEXT NOT NULL,
    description TEXT,
    status TEXT DEFAULT 'inbox',
    priority TEXT DEFAULT 'p2',
    tags TEXT DEFAULT '[]',
    due_date TIMESTAMPTZ,
    scheduled_date TIMESTAMPTZ,
    completed_at TIMESTAMPTZ,
    parent_id TEXT REFERENCES tasks(id),
    project_id TEXT REFERENCES projects(id),
    created_at TIMESTAMPTZ DEFAULT NOW(),
    sync_local_id TEXT NOT NULL,
    sync_version INTEGER DEFAULT 1,
    sync_modified_at TIMESTAMPTZ DEFAULT NOW(),
    sync_synced_at TIMESTAMPTZ,
    sync_deleted BOOLEAN DEFAULT FALSE
);

CREATE TABLE IF NOT EXISTS notes (
    id TEXT PRIMARY KEY,
    title TEXT NOT NULL,
    content TEXT DEFAULT '',
    tags TEXT DEFAULT '[]',
    project_id TEXT REFERENCES projects(id),
    created_at TIMESTAMPTZ DEFAULT NOW(),
    sync_local_id TEXT NOT NULL,
    sync_version INTEGER DEFAULT 1,
    sync_modified_at TIMESTAMPTZ DEFAULT NOW(),
    sync_synced_at TIMESTAMPTZ,
    sync_deleted BOOLEAN DEFAULT FALSE
);

CREATE TABLE IF NOT EXISTS events (
    id TEXT PRIMARY KEY,
    title TEXT NOT NULL,
    description TEXT,
    start_time TIMESTAMPTZ NOT NULL,
    end_time TIMESTAMPTZ,
    location TEXT,
    tags TEXT DEFAULT '[]',
    recurrence TEXT,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    sync_local_id TEXT NOT NULL,
    sync_version INTEGER DEFAULT 1,
    sync_modified_at TIMESTAMPTZ DEFAULT NOW(),
    sync_synced_at TIMESTAMPTZ,
    sync_deleted BOOLEAN DEFAULT FALSE
);

CREATE TABLE IF NOT EXISTS contacts (
    id TEXT PRIMARY KEY,
    name TEXT NOT NULL,
    email TEXT,
    notes TEXT,
    tags TEXT DEFAULT '[]',
    created_at TIMESTAMPTZ DEFAULT NOW(),
    sync_local_id TEXT NOT NULL,
    sync_version INTEGER DEFAULT 1,
    sync_modified_at TIMESTAMPTZ DEFAULT NOW(),
    sync_synced_at TIMESTAMPTZ,
    sync_deleted BOOLEAN DEFAULT FALSE
);

CREATE TABLE IF NOT EXISTS email_refs (
    id TEXT PRIMARY KEY,
    message_id TEXT NOT NULL UNIQUE,
    subject TEXT NOT NULL,
    from_addr TEXT NOT NULL,
    to_addrs TEXT DEFAULT '[]',
    date TIMESTAMPTZ,
    maildir_path TEXT,
    linked_task_id TEXT REFERENCES tasks(id),
    linked_note_id TEXT REFERENCES notes(id),
    created_at TIMESTAMPTZ DEFAULT NOW(),
    sync_local_id TEXT NOT NULL,
    sync_version INTEGER DEFAULT 1,
    sync_modified_at TIMESTAMPTZ DEFAULT NOW(),
    sync_synced_at TIMESTAMPTZ,
    sync_deleted BOOLEAN DEFAULT FALSE
);

CREATE TABLE IF NOT EXISTS links (
    id TEXT PRIMARY KEY,
    source_type TEXT NOT NULL,
    source_id TEXT NOT NULL,
    target_type TEXT NOT NULL,
    target_id TEXT NOT NULL,
    link_type TEXT NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    sync_local_id TEXT NOT NULL,
    sync_version INTEGER DEFAULT 1,
    sync_modified_at TIMESTAMPTZ DEFAULT NOW(),
    sync_synced_at TIMESTAMPTZ,
    sync_deleted BOOLEAN DEFAULT FALSE
);

CREATE TABLE IF NOT EXISTS sync_queue (
    id SERIAL PRIMARY KEY,
    entity_type TEXT NOT NULL,
    entity_id TEXT NOT NULL,
    operation TEXT NOT NULL,
    device_id TEXT NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    UNIQUE(entity_type, entity_id, device_id)
);

CREATE INDEX IF NOT EXISTS idx_tasks_status ON tasks(status) WHERE NOT sync_deleted;
CREATE INDEX IF NOT EXISTS idx_tasks_due_date ON tasks(due_date) WHERE NOT sync_deleted;
CREATE INDEX IF NOT EXISTS idx_tasks_project ON tasks(project_id) WHERE NOT sync_deleted;
CREATE INDEX IF NOT EXISTS idx_notes_project ON notes(project_id) WHERE NOT sync_deleted;
CREATE INDEX IF NOT EXISTS idx_events_start ON events(start_time) WHERE NOT sync_deleted;

INSERT INTO schema_version (version) VALUES (1) ON CONFLICT DO NOTHING;
