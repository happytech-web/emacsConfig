# Workflow

## Summary

This skill maps the reference Obsidian workflow into an Emacs `org-roam` workflow.

Reference shape:

- fetch candidates
- review candidates
- create notes for only the highest-priority papers

Our `org-roam` shape:

- write recommendation results into `RoamNotes/daily/YYYY-MM-DD.org`
- create formal paper notes in `RoamNotes/papers/`
- refresh `paper-index.org` and `research-dashboard.org`
- keep all created or updated files discoverable by `org-roam-find-node`

## Selector Profile

For `recommend` and `transfer`, prefer reading:

- `RoamNotes/projects/dailypaper-selector-profile.org`

Treat this file as the first source of truth for:

- current project description
- current research directions
- include keywords
- boost keywords
- exclude keywords
- preferred sources
- scoring weights for zero-token filtering

`config.json` still defines defaults and static topic registries, but the selector profile should drive the day-to-day filtering preference when present.

## Buckets

Use these bucket meanings consistently:

- `urgent_follow`
  Current-project strong match. These should usually create formal paper notes.
- `direction_core`
  Strongly aligned with the main research direction. These should usually create formal paper notes.
- `classics`
  Classic papers for structured learning. Only auto-create notes in classic-learning mode.
- `transferable_ideas`
  Not directly in the main direction but plausibly transferable. Keep in daily by default.
- `skip`
  Keep only a short reason in daily. Do not create formal notes.

## Org outputs

### Daily file

Write recommendation results into the current daily file under a managed block:

- `# DAILYPAPER-BEGIN`
- `# DAILYPAPER-END`

Within the block, keep sections in this order:

1. `Urgent Follow`
2. `Direction Core`
3. `Classics`
4. `Transferable Ideas`
5. `Skip`

### Paper notes

Create paper notes only for the buckets that require long-term retention:

- `urgent_follow`
- `direction_core`
- `classics` only in classic-learning mode

Each note should preserve:

- source URL
- arXiv ID when available
- recommendation bucket
- related area
- related project
- reason for recommendation

## Write Path

Do not write the final `.org` files directly from Python.

Instead:

- render content in Python
- hand off file creation or update to batch Emacs
- let Emacs ensure `ID`, save the buffer, and refresh the `org-roam` database

This is required so index pages and generated notes remain searchable from `org-roam-find-node`.

## Behavior expectations

- Prefer daily as the first landing page for all recommendation runs.
- Avoid turning every recommended paper into a long-term note.
- Use the generated org files as the durable artifact, not the chat response.
