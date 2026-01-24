# ParenVault

A personal knowledge management (PKM) TUI application written in OCaml with offline-first sync and aerc integration.

## Features

- **Offline-first**: Works without network, syncs when available
- **Multi-device sync**: PostgreSQL backend over Tailscale
- **TUI interface**: Vim-style keybindings, fast and keyboard-driven
- **aerc integration**: Create tasks from emails, send task updates
- **Markdown notes**: Simple, portable note format with task extensions

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                         TUI (minttea)                       │
├─────────────────────────────────────────────────────────────┤
│  Domain Layer (types, entities, business logic)             │
├──────────────────────┬──────────────────────────────────────┤
│  Local Storage       │  Sync Engine                         │
│  (SQLite/caqti)      │  (conflict resolution)               │
├──────────────────────┴──────────────────────────────────────┤
│  Remote Storage (PostgreSQL via Tailscale)                  │
└─────────────────────────────────────────────────────────────┘
```

## Installation

### Prerequisites

- OCaml 5.1+
- opam
- PostgreSQL (for remote sync)
- Tailscale (for secure remote access)

### Build

```bash
# Clone the repository
cd ParenVault

# Install dependencies
opam install . --deps-only

# Build
dune build

# Install
dune install
```

## Usage

### Initialize

```bash
# Create config and local database
parenvault init
```

This creates:
- `~/.config/parenvault/config.toml` - Configuration file
- `~/.local/share/parenvault/parenvault.db` - Local SQLite database

### TUI Mode

```bash
# Start the TUI
parenvault tui
```

#### Keybindings

| Key | Action |
|-----|--------|
| `j/k` or `↑/↓` | Navigate up/down |
| `Enter` | Open selected item |
| `n` | Create new item |
| `e` | Edit selected item |
| `d` | Delete selected |
| `x` | Toggle task status |
| `c` | Quick capture to Inbox |
| `D` | Daily Note |
| `L` | Link entities |
| `t` | Templates |
| `E` | Export |
| `w` | Weekly Review |
| `f` | Filter Tasks |
| `1-6` | Switch views (Dashboard, Tasks, Notes, Calendar, Projects, Contacts) |
| `0` | Inbox view |
| `9` | Archive view |
| `Esc` | Go back |
| `q` | Quit |

### CLI Commands

```bash
# Add a task
parenvault add "Review contract" -p high -t legal,urgent

# List tasks
parenvault list -s inbox

# Manual sync
parenvault sync
```

## Configuration

Edit `~/.config/parenvault/config.toml`:

```toml
[device]
name = "my-laptop"

[database]
local_path = "~/.local/share/parenvault/parenvault.db"
remote_host = "server.tailnet.ts.net"
remote_port = 5432
remote_name = "parenvault"
remote_user = "parenvault"

[mail]
maildir_path = "~/Mail"
action_folder = "ParenVault"

[sync]
auto_sync = true
interval_secs = 300
```

## aerc Integration

### Email to Task

1. Create a maildir folder called `ParenVault` in your mail account
2. In aerc, move emails you want to convert to tasks to this folder
3. ParenVault watches this folder and creates tasks from emails
4. Processed emails are moved to `ParenVault/Processed`

### Task Updates via Email

ParenVault can send task status updates as emails:
- Task assignments
- Status changes
- Completion notifications

Configure your outbox folder and ParenVault will create draft emails.

## PostgreSQL Setup (Remote Sync)

```sql
CREATE DATABASE parenvault;
CREATE USER parenvault WITH PASSWORD 'your-password';
GRANT ALL PRIVILEGES ON DATABASE parenvault TO parenvault;
```

Run migrations:
```bash
parenvault migrate
```

## Task Format in Markdown

```markdown
- [ ] Task title @due(2026-01-25) @priority(high) @tag(work)
- [x] Completed task @priority(low)
```

## Project Structure

```
ParenVault/
├── bin/                 # CLI entry point
│   └── main.ml
├── lib/
│   ├── domain/          # Core types and entities
│   ├── storage/         # Database layer (SQLite + PostgreSQL)
│   ├── sync/            # Offline-first sync engine
│   ├── tui/             # Terminal UI (minttea)
│   ├── aerc/            # Email integration
│   ├── markdown/        # Markdown parsing
│   └── config/          # Configuration handling
├── dune-project
└── config.example.toml
```

## License

Proprietary
