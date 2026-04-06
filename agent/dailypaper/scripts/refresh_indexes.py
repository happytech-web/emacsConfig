"""Refresh org-roam paper index pages."""

from __future__ import annotations

import argparse
from pathlib import Path

from agent.dailypaper.scripts.common import expand_roam_path, load_config
from agent.dailypaper.scripts.emacs_bridge import write_roam_files


def sorted_org_files(path: Path) -> list[Path]:
    if not path.exists():
        return []
    files = []
    for candidate in path.glob("*.org"):
        if candidate.name.startswith(".#") or candidate.name.startswith("#"):
            continue
        if not candidate.exists() or not candidate.is_file():
            continue
        files.append(candidate)
    return sorted(files, reverse=True)


def read_title(path: Path) -> str:
    for line in path.read_text(encoding="utf-8", errors="replace").splitlines():
        if line.startswith("#+title:"):
            return line.split(":", 1)[1].strip()
    return path.stem


def file_link(root: Path, path: Path, label: str) -> str:
    rel = path.relative_to(root)
    return f"[[file:{rel.as_posix()}][{label}]]"


def render_dashboard(config: dict) -> dict:
    root = Path(config["paths"]["roam_root"]).expanduser()
    daily_dir = root / config["paths"]["daily_dir"]
    papers_dir = root / config["paths"]["papers_dir"]
    dashboard_path = root / config["paths"]["research_dashboard"]

    recent_dailies = sorted_org_files(daily_dir)[:10]
    recent_papers = sorted_org_files(papers_dir)[:20]

    lines = [
        "#+title: Research Dashboard",
        "#+filetags: :dashboard:",
        "",
        "* Areas",
        "- Add or link research area notes here.",
        "",
        "* Active Projects",
        "- Add or link active project notes here.",
        "",
        "* Urgent Follow",
    ]
    if recent_papers:
        lines.extend([f"- {file_link(root, path, read_title(path))}" for path in recent_papers[:8]])
    else:
        lines.append("- None")
    lines.extend([
        "",
        "* Recent Recommendation Days",
    ])
    if recent_dailies:
        lines.extend([f"- {file_link(root, path, read_title(path))}" for path in recent_dailies])
    else:
        lines.append("- None")
    return {
        "path": str(dashboard_path),
        "title": "Research Dashboard",
        "content": "\n".join(lines) + "\n",
    }


def render_paper_index(config: dict) -> dict:
    root = Path(config["paths"]["roam_root"]).expanduser()
    papers_dir = root / config["paths"]["papers_dir"]
    index_path = root / config["paths"]["paper_index"]
    paper_files = sorted_org_files(papers_dir)

    lines = [
        "#+title: Paper Index",
        "#+filetags: :paper:index:",
        "",
        "* Recent Papers",
    ]
    if paper_files:
        lines.extend([f"- {file_link(root, path, read_title(path))}" for path in paper_files])
    else:
        lines.append("- None")
    lines.extend([
        "",
        "* Transferable Methods",
        "- Add method notes here.",
        "",
        "* Reading Notes",
        "- Agenda-driven reading queue lives in org-agenda command `P`."
    ])
    return {
        "path": str(index_path),
        "title": "Paper Index",
        "content": "\n".join(lines) + "\n",
    }


def main() -> int:
    parser = argparse.ArgumentParser(description="Refresh paper indexes")
    parser.add_argument("--config", default=None)
    args = parser.parse_args()

    config = load_config(args.config)
    write_roam_files([
        render_dashboard(config),
        render_paper_index(config),
    ])
    print("Refreshed research-dashboard.org and paper-index.org")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
