# ParenVault Feature Roadmap

A checklist of planned features for the ParenVault PKM application.

---

## High Priority (Core PKM)

- [x] **Search** - Full-text search across all entities (tasks, notes, events, contacts) - Press `/` to search
- [x] **Recurring Events/Tasks** - Multi-select weekdays with M T W R F S U keys to toggle days
- [x] **Due Date Reminders** - Visual indicators for overdue/upcoming items (red=overdue, yellow=today)
- [x] **Quick Capture** - Fast entry mode to dump thoughts into Inbox - Press `c` to capture
- [x] **Task Scheduling** - Due Date (deadline) + Scheduled Date ("do on" date)
- [x] **Linking** - Connect tasks to projects, notes to contacts, etc. (L key to link, Enter to open, d to delete)

---

## Medium Priority (Workflow)

- [x] **Filters/Views** - Filter by tag, priority, date range, project (f key)
- [x] **Weekly Review** - Summary of completed tasks, upcoming deadlines (w key)
- [x] **Templates** - Predefined structures for common note/task types (t key)
- [x] **Archive** - Hide completed items but keep them searchable
- [x] **Subtasks/Checklists** - Nested tasks within a parent task
- [x] **Time Blocking** - Assign time slots to tasks (Block Start/End fields in task form)

---

## Nice to Have (Power User)

- [ ] **Keyboard Macros** - Custom shortcuts for common actions
- [x] **Export** - Markdown, JSON, CSV export (E key)
- [x] **Sync Status** - Indicators: ●=local, ⟳=pending, ✓=synced
- [ ] **Undo/Redo** - Revert recent changes
- [x] **Attachments** - Link files to notes/tasks
- [x] **Daily Notes** - Press `D` to open/create today's note (auto-tagged "daily")

---

## Workflow Considerations

Moving from pen and paper:

- **Quick Capture** is essential - dump ideas fast, organize later
- **Weekly Review** helps build the habit of processing your inbox
- **Due dates with visual indicators** replace the "glance at your list" habit

---

## Current Sprint - COMPLETE! 🎉

All initial sprint features are done:

1. [x] **Search** - Full-text search (`/` key)
2. [x] **Recurring Events/Tasks** - Multi-select weekdays (M T W R F S U keys)
3. [x] **Due Date Indicators** - Visual cues in task lists (red=overdue, yellow=today)
4. [x] **Quick Capture** - Fast inbox entry (`c` key)

---

## Completed Features

- [x] Dashboard with today's tasks/events (DD-MMM-YYYY format)
- [x] Task CRUD with tags, notes, priority
- [x] Note CRUD with content and tags
- [x] Event CRUD with location, notes, date/time (UTC+3 timezone support)
- [x] Contact CRUD with email, phone, notes
- [x] Project CRUD with status
- [x] Multi-field form editing with Tab navigation
- [x] Keyboard navigation (hjkl, number keys for views)
- [x] Delete functionality
- [x] Toggle task done/todo with 'x'
- [x] ESC to go back / Dashboard fallback
- [x] Run script (`./pv`) for easy startup
- [x] Search across all entities (`/` key)
- [x] Quick Capture to Inbox (`c` key)
- [x] Due date indicators (red=overdue, yellow=today)
- [x] Event time editing with local timezone (UTC+3)
- [x] Task Scheduling with Due Date and Scheduled Date
- [x] Recurring tasks/events with multi-select weekdays (Mon-Sun toggle)
- [x] Daily Notes (`D` key) - auto-creates note with today's date and "daily" tag
- [x] Sync Status indicators (●=local, ⟳=pending, ✓=synced)
- [x] Priority field arrow key navigation (←/→ to change)
- [x] PostgreSQL remote sync working with soft deletes
- [x] Subtasks/Checklists (`a` key to add subtask, j/k to navigate, x to toggle)
- [x] Subtask progress display [done/total %] in task list and detail views
- [x] Auto-complete parent task when all subtasks are done
- [x] Archive view (`9` key) - view deleted tasks, notes, events
- [x] Restore from archive (`r` key)
- [x] Permanent delete from archive (`d` key)
- [x] Attachments (`A` key to attach file, `o` to open, `j/k` to navigate)
- [x] Auto schema creation on startup (tables created automatically on new machines)
- [x] Bidirectional sync on startup (local ↔ remote PostgreSQL)
- [x] Linking entities (`L` key to link, Enter to open linked item, `d` to delete link)
- [x] Time Blocking (Block Start/End datetime fields in task form, displayed in task detail)
- [x] Weekly Review (`w` key - shows overdue, due this week, events, inbox, completed)
- [x] Filters/Views (`f` key - filter by tag, priority, project, status)
- [x] Templates (`t` key - built-in templates for notes and tasks)
- [x] Export (`E` key - export lists to MD/JSON/CSV, or single item from detail view)
- [x] Centered UI (content centered horizontally and vertically)
