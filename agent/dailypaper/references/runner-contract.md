# Runner Contract

## Main entrypoint

Use the package entrypoint:

```bash
python -m agent.dailypaper ...
```

Do not bypass the top-level runner unless you are debugging a specific phase.

## Commands

### Recommend

```bash
python -m agent.dailypaper recommend --days N [--area AREA] [--project PROFILE] [--project-text TEXT]
```

Use for:

- daily recommendations
- recent-paper windows
- project-driven recommendation requests

Before filtering, this mode should prefer reading:

- `~/RoamNotes/projects/dailypaper-selector-profile.org`

Use that org note as the day-to-day source for project description, current directions, include/boost/exclude keywords, preferred sources, and ranking weights.

This command does not finalize org output. It prepares:

- fetched candidates
- filtered candidates
- a compact review packet for agent review

### Classic

```bash
python -m agent.dailypaper classic --topic TOPIC
```

Use for:

- “推荐这个方向的经典论文”
- “我想系统学某个方向”

This mode produces a review packet from configured classic-topic entries.

### Transfer

```bash
python -m agent.dailypaper transfer --topic TOPIC [--days N]
```

Use for:

- cross-domain method discovery
- “哪些别的领域方法可能迁移到我这里”

Like `recommend`, this stops after the review packet stage.
Like `recommend`, it should read the selector profile first when available.

### Finalize

```bash
python -m agent.dailypaper finalize --input /path/to/reviewed.json
```

Use after the agent has:

- reviewed filtered candidates
- assigned scores and buckets
- chosen the deep-read subset
- filled deep-read summaries for the selected papers

Finalize must route final org creation or updates through Emacs org-roam APIs, not raw Python file writes.

### Refresh indexes

```bash
python -m agent.dailypaper refresh-indexes
```

Use when:

- the user asks to rebuild index pages
- you changed or created many paper notes outside the normal workflow

This command should also refresh index files through Emacs so the generated nodes keep valid `ID` metadata and are present in the org-roam DB.

## Config rules

Read `config.json` when you need to know:

- configured research areas
- configured project profiles
- classic topics
- transfer domains
- note creation policy

Read `~/RoamNotes/projects/dailypaper-selector-profile.org` when you need the user's current project description and filtering preference.

Prefer config-backed names over inventing new slugs when possible.
