# ParenVault User Guide

A comprehensive guide to using ParenVault, your offline-first Personal Knowledge Management system.

---

## Quick Start

```bash
# Run the application
./pv

# Or with dune
dune exec parenvault -- tui
```

---

## Keyboard Shortcuts

### Global Navigation

| Key | Action |
| --- | ------ |
| `1` | Dashboard |
| `2` | Tasks |
| `3` | Notes |
| `4` | Calendar |
| `5` | Projects |
| `6` | Contacts |
| `0` | Inbox |
| `9` | Archive |
| `w` | Weekly Review |
| `f` | Filter Tasks |
| `/` | Search |
| `q` | Quit |
| `Esc` | Go back / Cancel |

### List Navigation

| Key | Action |
| --- | ------ |
| `j` / `↓` | Move down |
| `k` / `↑` | Move up |
| `g` | Go to first item |
| `G` | Go to last item |
| `Enter` | Open selected item |

### Actions

| Key | Action |
| --- | ------ |
| `n` | Create new item |
| `e` | Edit selected item |
| `d` | Delete (soft delete) |
| `c` | Quick capture to Inbox |
| `D` | Daily Note (create/open today's note) |
| `L` | Link to another entity |
| `t` | Open template picker |
| `E` | Export (list view → picker, detail view → export item) |

### Task-Specific

| Key | Action |
| --- | ------ |
| `x` | Toggle task done/todo |
| `a` | Add subtask (in task detail) |
| `A` | Add attachment (in task/note detail) |
| `o` | Open attachment |

### Archive View (`9`)

| Key | Action |
| --- | ------ |
| `r` | Restore selected item |
| `d` | Permanently delete |

### Form Editing

| Key | Action |
| --- | ------ |
| `Tab` / `↓` | Next field |
| `↑` | Previous field |
| `←` / `→` | Change priority / toggle options |
| `Enter` | Save |
| `Esc` | Cancel |

### Recurring Tasks/Events

When editing recurrence, use these keys to toggle days:

| Key | Day |
| --- | --- |
| `M` | Monday |
| `T` | Tuesday |
| `W` | Wednesday |
| `R` | Thursday |
| `F` | Friday |
| `S` | Saturday |
| `U` | Sunday |

---

## Features

### Tasks
- **Inbox**: Quick capture items that need processing
- **Status**: Inbox → Todo → In Progress → Waiting → Done
- **Priority**: P0 (Critical) → P1 (High) → P2 (Medium) → P3 (Low)
- **Due Date**: Deadline for the task (red if overdue, yellow if today)
- **Scheduled Date**: When you plan to work on it
- **Subtasks**: Nested checklists within a task
- **Progress**: Shows [done/total - %] for tasks with subtasks
- **Auto-complete**: Parent task marks done when all subtasks complete

### Notes
- Markdown content
- Tags for organization
- Link to projects
- Attachments support

### Daily Notes
Press `D` from any view to create or open today's daily note.
- Auto-titled with today's date (e.g., "24-JAN-2026")
- Auto-tagged with "daily"
- Great for journaling, standup notes, etc.

### Calendar/Events
- Start time and optional end time
- Location field
- Recurring events (daily, weekly with specific days, monthly, yearly)
- Times displayed in local timezone (UTC+3)

### Search
Press `/` to search across all entities:
- Tasks
- Notes
- Events
- Contacts

Results show with icons: 📋 Task, 📝 Note, 📅 Event, 👤 Contact

### Archive
Press `9` to view deleted items:
- See all soft-deleted tasks, notes, and events
- Press `r` to restore an item
- Press `d` to permanently delete

### Attachments

In Task or Note detail view:

- Press `A` to attach a file (enter full path)
- Press `j/k` to navigate between attachments
- Press `o` to open the selected attachment
- Files are copied to `~/.local/share/parenvault/attachments/`

### Linking

Connect related entities together:

- Press `L` from any detail view to open the link picker
- Select a task, note, project, or contact to link
- Press `Enter` to open a linked item
- Press `d` to delete a link (when link is selected)
- Links are bidirectional - visible from both ends

### Time Blocking

Assign specific time slots to tasks:

- Edit a task and set "Block Start" and "Block End" fields
- Format: `YYYY-MM-DD HH:MM` (e.g., `2026-01-24 09:00`)
- Time blocks display in task detail with ⏰ icon

### Weekly Review

Press `w` to open the Weekly Review:

- **Overdue**: Tasks past their due date (red)
- **Due This Week**: Upcoming deadlines (yellow)
- **Events This Week**: Scheduled events (cyan)
- **Inbox**: Items needing processing (magenta)
- **Completed**: Tasks finished this week (green)
- Navigate with `j/k`, press `Enter` to open any item

### Filters/Views

Press `f` to filter tasks:

- **Priority**: P0, P1, P2, P3
- **Status**: Overdue, Due Today, Due This Week, Inbox
- **Project**: Filter by project
- **Tag**: All existing tags are listed for selection
- Navigate with `j/k`, press `Enter` to apply filter
- Press `Esc` to go back to filter picker or exit

### Templates

Press `t` to open the template picker:

- From Notes view → Note templates
- From other views → Task templates
- Navigate with `j/k`, press `Enter` to create

**Note Templates:**

- Blank Note, Meeting Notes, Daily Standup, 1:1 Meeting, Project Plan, Retrospective

**Task Templates:**

- Blank Task, Bug Report, Feature Request, Code Review

### aerc Email Integration

**Import email as task/note (in aerc):**

Add to `~/.config/aerc/binds.conf`:
```
pt = :pipe -m parenvault import-email<Enter>
pn = :pipe -m parenvault import-email -n<Enter>
```

- `pt` in aerc → pipes email to ParenVault, creates task (tagged "email")
- `pn` in aerc → pipes email to ParenVault, creates note (tagged "email")

**Send task/note via email:**
```bash
parenvault send <task-id-prefix> recipient@example.com
```

Opens aerc compose with task/note content pre-filled.

### Export

Press `E` to export data:

**From list views** → Opens export picker:

- Tasks → Markdown, JSON, CSV
- Notes → Markdown, JSON
- Events → Markdown, JSON
- All Data → JSON

**From detail views** → Exports single item:

- Task detail → `~/parenvault-export/task-{title}.md`
- Note detail → `~/parenvault-export/note-{title}.md`
- Event detail → `~/parenvault-export/event-{title}.md`

---

## Data Storage

### Directories
| Path | Purpose |
|------|---------|
| `~/.config/parenvault/config.toml` | Configuration file |
| `~/.local/share/parenvault/parenvault.db` | Local SQLite database |
| `~/.local/share/parenvault/attachments/` | Attached files |

### Sync
- **Local**: SQLite database for offline operation
- **Remote**: PostgreSQL for multi-device sync
- **Auto-sync**: On startup, bidirectional sync between local and remote
- **Schema auto-creation**: Tables created automatically on new machines

### Sync Status Indicators
| Icon | Meaning |
|------|---------|
| `●` (gray) | Local only, not synced |
| `⟳` (yellow) | Pending sync |
| `✓` (green) | Synced with remote |

---

## Installation on New Machine

### Prerequisites
- OCaml 5.1+
- opam with required packages
- Tailscale (for remote sync)

### Setup Steps

1. **Create directories**:
```bash
mkdir -p ~/.config/parenvault
mkdir -p ~/.local/share/parenvault/attachments
```

2. **Copy or create config file** (`~/.config/parenvault/config.toml`):
```toml
[device]
id = "your-unique-device-id"
name = "laptop-name"

[database]
local_path = "/home/yourusername/.local/share/parenvault/parenvault.db"
remote_host = "your-postgres-host"
remote_port = 5432
remote_name = "parenvault"
remote_user = "root"

[sync]
auto_sync = true
interval_secs = 300

[ui]
theme = "default"
date_format = "%Y-%m-%d"
show_completed = false
default_view = "dashboard"
```

3. **Set PostgreSQL password** (in your shell or the `pv` script):
```bash
export PARENVAULT_DB_PASSWORD="your-password"
```

4. **Build and run**:
```bash
dune build
./pv
```

The app will automatically:
- Create the local SQLite database with all tables
- Connect to PostgreSQL and create tables if needed
- Sync all data from remote to local

---

## Tips

- Use `c` for quick capture - dump thoughts into Inbox, process later
- Use `D` daily for journaling or standup notes
- Subtasks are great for breaking down complex tasks
- The Archive lets you recover accidentally deleted items
- Attachments are copied locally, so they work offline

---

## Troubleshooting

### Can't connect to PostgreSQL
- Check Tailscale is running: `tailscale status`
- Verify host is reachable: `ping your-host`
- Check password is set: `echo $PARENVAULT_DB_PASSWORD`

### Database errors
- Tables are auto-created on startup
- If issues persist, check `~/.local/share/parenvault/parenvault.db` exists

### Attachments not showing
- Verify file was copied to `~/.local/share/parenvault/attachments/`
- Check the attachment table in the database
