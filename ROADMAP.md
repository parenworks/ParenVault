# ParenVault Feature Roadmap

A checklist of planned features for the ParenVault PKM application.

---

## High Priority (Core PKM)

- [x] **Search** - Full-text search across all entities (tasks, notes, events, contacts) - Press `/` to search
- [x] **Recurring Events/Tasks** - Multi-select weekdays with M T W R F S U keys to toggle days
- [x] **Due Date Reminders** - Visual indicators for overdue/upcoming items (red=overdue, yellow=today)
- [x] **Quick Capture** - Fast entry mode to dump thoughts into Inbox - Press `c` to capture
- [x] **Task Scheduling** - Due Date (deadline) + Scheduled Date ("do on" date)
- [ ] **Linking** - Connect tasks to projects, notes to contacts, etc.

---

## Medium Priority (Workflow)

- [ ] **Filters/Views** - Filter by tag, priority, date range, project
- [ ] **Weekly Review** - Summary of completed tasks, upcoming deadlines
- [ ] **Templates** - Predefined structures for common note/task types
- [ ] **Archive** - Hide completed items but keep them searchable
- [ ] **Subtasks/Checklists** - Nested tasks within a parent task
- [ ] **Time Blocking** - Assign time slots to tasks

---

## Nice to Have (Power User)

- [ ] **Keyboard Macros** - Custom shortcuts for common actions
- [ ] **Export** - Markdown, JSON, CSV export
- [x] **Sync Status** - Indicators: ●=local, ⟳=pending, ✓=synced
- [ ] **Undo/Redo** - Revert recent changes
- [ ] **Attachments** - Link files to notes/tasks
- [x] **Daily Notes** - Press `D` to open/create today's note (auto-tagged "daily")

---

## Workflow Considerations

Since you're moving from pen and paper:

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
