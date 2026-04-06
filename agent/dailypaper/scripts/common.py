"""Shared utilities for the dailypaper workflow."""

from __future__ import annotations

import json
import os
import re
from dataclasses import dataclass
from datetime import date, datetime, timedelta
from pathlib import Path
from typing import Any


ROOT = Path(__file__).resolve().parents[1]
STATE_DIR = ROOT / "state"
TEMPLATE_DIR = ROOT / "templates"


def load_json(path: Path) -> Any:
    with path.open("r", encoding="utf-8") as handle:
        return json.load(handle)


def save_json(path: Path, payload: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8") as handle:
        json.dump(payload, handle, ensure_ascii=False, indent=2)


def load_config(config_path: str | None = None) -> dict[str, Any]:
    path = Path(config_path).expanduser() if config_path else ROOT / "config.json"
    return load_json(path)


def expand_roam_path(config: dict[str, Any], *parts: str) -> Path:
    base = Path(config["paths"]["roam_root"]).expanduser()
    for part in parts:
        base = base / part
    return base


def today_iso() -> str:
    return date.today().isoformat()


def parse_iso_day(value: str | None) -> date:
    return datetime.strptime(value or today_iso(), "%Y-%m-%d").date()


def daterange_backwards(end_day: date, days: int) -> list[date]:
    return [end_day - timedelta(days=offset) for offset in range(days)]


def slugify(text: str) -> str:
    lowered = text.lower().strip()
    lowered = re.sub(r"[^a-z0-9]+", "-", lowered)
    return lowered.strip("-") or "note"


def extract_arxiv_id(url: str) -> str:
    match = re.search(r"arxiv\.org/(?:abs|pdf|html)/(\d{4}\.\d{4,5}(?:v\d+)?)", url or "")
    return match.group(1) if match else ""


def compact(text: str) -> str:
    return re.sub(r"\s+", " ", (text or "").strip())


def read_template(name: str) -> str:
    return (TEMPLATE_DIR / name).read_text(encoding="utf-8")


def read_org_profile(path: Path) -> dict[str, Any]:
    if not path.exists():
        return {}

    sections: dict[str, list[str]] = {}
    current: str | None = None
    for raw_line in path.read_text(encoding="utf-8", errors="replace").splitlines():
        line = raw_line.rstrip()
        heading = re.match(r"^\*+\s+(.*)$", line)
        if heading:
            current = heading.group(1).strip().lower()
            sections.setdefault(current, [])
            continue
        if current is not None:
            sections[current].append(line)

    def section_lines(name: str) -> list[str]:
        return [line.strip() for line in sections.get(name.lower(), [])]

    def bullet_items(name: str) -> list[str]:
        items = []
        for line in section_lines(name):
            match = re.match(r"^-\s+(.*)$", line)
            if match and match.group(1).strip():
                items.append(match.group(1).strip())
        return items

    def kv_items(name: str) -> dict[str, str]:
        pairs: dict[str, str] = {}
        for item in bullet_items(name):
            if ":" not in item:
                continue
            key, value = item.split(":", 1)
            pairs[key.strip()] = value.strip()
        return pairs

    def paragraph(name: str) -> str:
        parts = []
        for line in section_lines(name):
            if not line.strip() or line.lstrip().startswith("#"):
                continue
            if re.match(r"^[-:]", line.strip()):
                continue
            parts.append(line.strip())
        return compact(" ".join(parts))

    profile = {
        "project_description": paragraph("project description"),
        "current_directions": bullet_items("current directions"),
        "include_keywords": bullet_items("include keywords"),
        "boost_keywords": bullet_items("boost keywords"),
        "exclude_keywords": bullet_items("exclude keywords"),
        "preferred_sources": bullet_items("preferred sources"),
        "scoring_weights": {},
    }
    for key, value in kv_items("scoring weights").items():
        try:
            profile["scoring_weights"][key] = int(value)
        except ValueError:
            continue
    return profile


def build_org_link(from_file: Path, target_file: Path, title: str) -> str:
    relative = Path(os.path.relpath(target_file, start=from_file.parent))
    return f"[[file:{relative.as_posix()}][{title}]]"


@dataclass
class WorkflowContext:
    mode: str
    date: str
    days: int
    topic: str = ""
    area: str = ""
    project: str = ""
    project_text: str = ""
