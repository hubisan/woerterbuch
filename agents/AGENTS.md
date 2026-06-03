# AI Agent Instructions

Version: 0.4.0

AI agents must read this file before changing this repository.

This file defines the general repository workflow for AI agents.

Repository-specific instructions are stored separately in [./repository.org](./repository.org).

Important files and directories:

- [./repository.org](./repository.org): Repository-specific instructions.
- [./ai-notes.org](./ai-notes.org): Cross-task context, notes, blockers, and decisions. Keep task-local details inside task files.
- [../tasks/todo.org](../tasks/todo.org): Active task index and statuses.
- [../tasks/](../tasks/): Active task-specific files.
- [../tasks/archive/](../tasks/archive/): Archived tasks/todos. Exclude from active AI context unless explicitly asked.
- [../tasks/template.org](../tasks/template.org): Template for task-specific files.
- [../../CHANGELOG.org](../../CHANGELOG.org): Approved/completed notable changes.

Before making changes, read and follow:

1. [AGENTS.md](./AGENTS.md)
2. [./repository.org](./repository.org)
3. [../tasks/todo.org](../tasks/todo.org)

| Status   | Owner   | Meaning                                      |
| -------- | ------- | -------------------------------------------- |
| TODO     | User    | Not ready. Do not work on it.                |
| NEXT     | User    | Ready for AI. Work only on these tasks.      |
| WAIT     | User/AI | Blocked. Document reason in the task file.   |
| REVIEW   | AI      | Done by AI, waiting for user review.         |
| CONTINUE | User    | Continue using the user's review comments.   |
| DONE     | User/AI | Approved and completed. Changelog if needed. |
| CANCEL   | User    | Abandoned. Do not work on it.                |

## Language

Match user's language for chat. Use English for code, comments, docs, commits, and files unless instructed otherwise.

## Org-mode syntax

[../tasks/todo.org](../tasks/todo.org), [./ai-notes.org](./ai-notes.org), [./repository.org](./repository.org), and [../../CHANGELOG.org](../../CHANGELOG.org) use Org-mode, not Markdown.

Common AI errors to avoid:

- Bold: `*bold*`, not `**bold**`
- Inline code, variables, commands, filenames, symbols: `~name~`, not `` `name` ``
- Use bold for short inline emphasis, not as a heading substitute.
- Prefer headings/subheadings for structure.
- Do not insert manual line breaks in normal prose just for visual wrapping.

## Workflow

1. User marks a task in [../tasks/todo.org](../tasks/todo.org) as `NEXT`.
2. AI only works on `NEXT` or `CONTINUE` tasks unless explicitly instructed.
3. For each AI task, AI creates/uses a task file in [../tasks/](../tasks/) based on [../tasks/template.org](../tasks/template.org): `../tasks/YYYY-MM-DD--slug.org`. Remove sections in the template if not relevant. Add other sections if it makes sense. Add the task start date at the top at `#+TASK_STARTED:`, like `[2026-06-03 Mi]`.
4. AI links the task in [../tasks/todo.org](../tasks/todo.org) to its task file. Example: `[[./2026-05-24--anki-export-fix.org]]` (relative to the todo file).
5. On `main`, create a focused branch, e.g. `feature/task-name` or `fix/task-name`; otherwise continue on the current branch and report suspicious branch/repo state.
6. AI reads and follows [./repository.org](./repository.org) for repository-specific instructions.
7. AI implements the requested task.
8. If blocked, AI sets the task to `WAIT`, records the reason in the task file, notifies the user, and stops until the task is set back to `NEXT` or `CONTINUE`.
9. AI updates docs/links and runs relevant checks.
10. AI documents results, checks, blockers, questions, and follow-ups in the task file.
11. AI sets finished tasks to `REVIEW`.
12. User reviews `REVIEW` tasks. If more work is needed, user sets the task to `CONTINUE`.
13. After approval by the user:
    1. AI sets the task to `DONE`.
    2. AI updates [../../CHANGELOG.org](../../CHANGELOG.org) for notable changes.
    3. AI adds the completion date at the top of the task file at `#+TASK_COMPLETED:`, like `[2026-06-03 Mi]`.
    4. AI may create [../tasks/archive/](../tasks/archive/) if needed, moves the task file there, and updates the link in [../tasks/todo.org](../tasks/todo.org) to point to the archive subdirectory.
    5. AI must not remove or archive the task entry in [../tasks/todo.org](../tasks/todo.org) unless explicitly instructed.
14. AI commits only when explicitly asked.
15. Commits must be focused and have clear messages.
16. If asked to squash, AI reviews the branch diff, updates [../../CHANGELOG.org](../../CHANGELOG.org) if needed, and creates one clear commit.
17. User merges or requests merge into `main`.

## Commit rule

AI may prepare commits, but may only create, amend, squash, or rewrite commits when explicitly asked.

## Implementation rules

- Make small, focused changes.
- Respect existing style and architecture.
- Read and follow [./repository.org](./repository.org) before changing code, tests, documentation, or project configuration.
- Run relevant checks when possible.
- Record results, failed/skipped checks, blockers, and uncertainty in the task file.
- Avoid unrelated changes, unnecessary refactoring, and whole-file formatting.

## Protected changes

Do not change without explicit instruction: secrets, tokens, `.env` files, production config, deployment credentials, large refactorings, dependency updates, or whole-file formatting.

## When unsure

Do not invent assumptions. Record task-specific uncertainty in the task file. Record only repository-wide uncertainty in [./ai-notes.org](./ai-notes.org). Notify the user and ask only if needed.

## Repository Maintenance (User Only)

AGENTS.md and the task template are managed via a Git subtree into a local vendor cache (`.project/vendor/`) and then copied to their respective locations.

**Critical:** AI agents must not update or synchronize these shared workflow files. This is a manual user task.

If the repository needs to be synchronized, remind the user to run one of the following command sequences:

**For initial setup in a new project:**

```bash
git subtree add --prefix=.project/vendor https://github.com/hubisan/ai-agents-config.git main --squash
cp -f .project/vendor/agents/AGENTS.md .project/agents/
cp -f .project/vendor/tasks/template.org .project/tasks/
```

**For subsequent updates:**

```bash
git subtree pull --prefix=.project/vendor https://github.com/hubisan/ai-agents-config.git main --squash
cp -f .project/vendor/agents/AGENTS.md .project/agents/
cp -f .project/vendor/tasks/template.org .project/tasks/
```
