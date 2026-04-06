"""Write agent-reviewed recommendation results into org-roam files."""

from __future__ import annotations

import argparse
import uuid
from datetime import datetime
from pathlib import Path

from agent.dailypaper.scripts.common import build_org_link, expand_roam_path, load_config, load_json, read_template, save_json, slugify
from agent.dailypaper.scripts.emacs_bridge import write_roam_files


MARKER_BEGIN = "# DAILYPAPER-BEGIN"
MARKER_END = "# DAILYPAPER-END"


def load_history(history_path: Path) -> list[dict]:
    if not history_path.exists():
        return []
    return load_json(history_path)


def save_history(history_path: Path, papers: list[dict], run_date: str) -> None:
    existing = load_history(history_path)
    by_id = {item["key"]: item for item in existing if item.get("key")}
    for paper in papers:
        key = paper.get("arxiv_id") or paper.get("url")
        if not key:
            continue
        by_id[key] = {
            "key": key,
            "title": paper.get("title", ""),
            "bucket": paper.get("bucket", ""),
            "date": run_date,
        }
    rows = sorted(by_id.values(), key=lambda item: (item["date"], item["title"]), reverse=True)
    save_json(history_path, rows[:200])


def reviewed_field(paper: dict, field: str, default=None):
    review = paper.get("review", {})
    return review.get(field, default)


def deep_read_field(paper: dict, field: str, default=None):
    review = paper.get("review", {})
    deep_read = review.get("deep_read", {})
    return deep_read.get(field, default)


def format_bulleted_list(items, empty="- None") -> str:
    if not items:
        return empty
    return "\n".join(f"- {item}" for item in items)


def format_formula_list(items) -> str:
    if not items:
        return "- 无关键公式或暂未补充。"
    rendered = []
    for item in items:
        if isinstance(item, dict):
            name = item.get("name", "公式")
            latex = item.get("latex", "")
            meaning = item.get("meaning", "")
            symbols = item.get("symbols", [])
            block = [f"- {name}"]
            if latex:
                block.append("  #+begin_export latex")
                block.append(f"  {latex}")
                block.append("  #+end_export")
            if meaning:
                block.append(f"  - 含义: {meaning}")
            if symbols:
                block.append(f"  - 符号: {'; '.join(symbols)}")
            rendered.append("\n".join(block))
        else:
            rendered.append(f"- {item}")
    return "\n".join(rendered)


def format_related_work(items) -> str:
    if not items:
        return "- 暂未补充。"
    rendered = []
    for item in items:
        if isinstance(item, dict):
            title = item.get("title", "Unknown")
            relation = item.get("relation", "")
            note = item.get("note", "")
            line = f"- {title}"
            if relation:
                line += f" | 关系: {relation}"
            if note:
                line += f" | 说明: {note}"
            rendered.append(line)
        else:
            rendered.append(f"- {item}")
    return "\n".join(rendered)


def format_bullets(daily_file: Path, papers: list[dict], note_links: dict[str, Path]) -> dict[str, str]:
    buckets = {
        "urgent_follow": [],
        "direction_core": [],
        "classics": [],
        "transferable_ideas": [],
        "skip": [],
    }
    for paper in papers:
        bucket = reviewed_field(paper, "bucket", "skip") or "skip"
        note_path = note_links.get(paper.get("title", ""))
        if note_path:
            title = build_org_link(daily_file, note_path, paper["title"])
        else:
            title = f"[[{paper.get('url', '')}][{paper['title']}]]" if paper.get("url") else paper["title"]
        line = f"- {title}\n  - {reviewed_field(paper, 'reason', '')}"
        score = reviewed_field(paper, "score", None)
        if score is not None:
            line += f"\n  - Review score: {score}"
        deep_read_summary = deep_read_field(paper, "一句话总结", "")
        if deep_read_summary:
            line += f"\n  - Deep read: {deep_read_summary}"
        if paper.get("figure_url"):
            line += f"\n  - Figure: {paper['figure_url']}"
        buckets[bucket].append(line)
    return {key: "\n".join(value) if value else "- None" for key, value in buckets.items()}


def replace_block(original: str, replacement: str) -> str:
    if MARKER_BEGIN in original and MARKER_END in original:
        prefix = original.split(MARKER_BEGIN, 1)[0].rstrip()
        suffix = original.split(MARKER_END, 1)[1].lstrip()
        body = f"{MARKER_BEGIN}\n{replacement.rstrip()}\n{MARKER_END}"
        return f"{prefix}\n\n{body}\n\n{suffix}".rstrip() + "\n"
    base = original.rstrip()
    body = f"{MARKER_BEGIN}\n{replacement.rstrip()}\n{MARKER_END}"
    return (base + "\n\n" + body + "\n").lstrip()


def existing_note_for(papers_dir: Path, paper: dict) -> Path | None:
    if not papers_dir.exists():
        return None
    arxiv_id = paper.get("arxiv_id", "")
    for path in papers_dir.glob("*.org"):
        text = path.read_text(encoding="utf-8", errors="replace")
        if arxiv_id and f":ARXIV_ID: {arxiv_id}" in text:
            return path
        if paper.get("url") and f":SOURCE_URL: {paper['url']}" in text:
            return path
        if f"#+title: {paper['title']}" in text:
            return path
    return None


def render_paper_note(template: str, path: Path, paper: dict, mode: str, run_days: int) -> dict:
    figure_block = f"- [[{paper['figure_url']}][Representative figure]]" if paper.get("figure_url") else "- No figure extracted yet."
    content = template.format(
        uuid=str(uuid.uuid4()),
        title=paper["title"],
        url=paper.get("url", ""),
        arxiv_id=paper.get("arxiv_id", ""),
        doi=paper.get("doi", ""),
        pdf_path=paper.get("pdf_url", ""),
        bib_key="",
        time_range=f"past-{run_days}-days" if run_days else "",
        bucket=reviewed_field(paper, "bucket", ""),
        related_area=paper.get("related_area", ""),
        related_project=paper.get("related_project", ""),
        classic_of=paper.get("classic_of", "") if mode == "classic-learning" else "",
        transfer_hint=paper.get("transfer_hint", ""),
        summary=deep_read_field(paper, "一句话总结", "").strip() or paper.get("summary", "").strip() or "Pending deep read.",
        reason=reviewed_field(paper, "reason", ""),
        one_line_summary=deep_read_field(paper, "一句话总结", "").strip() or "待补充。",
        core_contributions=format_bulleted_list(deep_read_field(paper, "核心贡献", []), "- 待补充。"),
        problem_statement=deep_read_field(paper, "要解决的问题", "") or "待补充。",
        method_limits=deep_read_field(paper, "现有方法局限", "") or "待补充。",
        motivation=deep_read_field(paper, "本文动机", "") or "待补充。",
        framework=deep_read_field(paper, "整体框架", "") or "待补充。",
        core_modules=format_bulleted_list(deep_read_field(paper, "核心模块", []), "- 待补充。"),
        key_formulas=format_formula_list(deep_read_field(paper, "关键公式", [])),
        experiments=deep_read_field(paper, "实验结果", "") or "待补充。",
        strengths=format_bulleted_list((deep_read_field(paper, "批判性思考", {}) or {}).get("优点", []), "- 待补充。"),
        limitations=format_bulleted_list((deep_read_field(paper, "批判性思考", {}) or {}).get("局限性", []), "- 待补充。"),
        improvements=format_bulleted_list((deep_read_field(paper, "批判性思考", {}) or {}).get("潜在改进", []), "- 待补充。"),
        related_work=format_related_work(deep_read_field(paper, "相关工作", [])),
        follow_up=format_bulleted_list(deep_read_field(paper, "后续阅读", []), "- 待补充。"),
        figure_block=figure_block,
    )
    return {"path": str(path), "title": paper["title"], "content": content}


def write_paper_note(template: str, papers_dir: Path, paper: dict, mode: str, run_days: int) -> tuple[Path, dict | None]:
    existing = existing_note_for(papers_dir, paper)
    if existing:
        return existing, None

    papers_dir.mkdir(parents=True, exist_ok=True)
    timestamp = datetime.now().strftime("%Y%m%d%H%M%S")
    path = papers_dir / f"{timestamp}-{slugify(paper['title'])}.org"
    return path, render_paper_note(template, path, paper, mode, run_days)


def render_daily(config: dict, ranked: dict, note_links: dict[str, Path]) -> tuple[Path, dict]:
    run_date = ranked["date"] or datetime.now().strftime("%Y-%m-%d")
    daily_file = expand_roam_path(config, config["paths"]["daily_dir"], f"{run_date}.org")
    daily_file.parent.mkdir(parents=True, exist_ok=True)
    if daily_file.exists():
        existing = daily_file.read_text(encoding="utf-8")
    else:
        existing = f"#+title: {run_date}\n"
    bullets = format_bullets(daily_file, ranked["papers"], note_links)
    block = read_template("daily_recommendation.org").format(
        title=f"{run_date} Paper Recommendations",
        urgent_follow=bullets["urgent_follow"],
        direction_core=bullets["direction_core"],
        classics=bullets["classics"],
        transferable_ideas=bullets["transferable_ideas"],
        skip=bullets["skip"],
    )
    updated = replace_block(existing, block)
    return daily_file, {"path": str(daily_file), "title": run_date, "content": updated}


def main() -> int:
    parser = argparse.ArgumentParser(description="Write org outputs")
    parser.add_argument("--input", required=True)
    parser.add_argument("--config", default=None)
    parser.add_argument("--history", default=None)
    args = parser.parse_args()

    config = load_config(args.config)
    ranked = load_json(Path(args.input))
    history_path = Path(args.history) if args.history else Path(__file__).resolve().parents[1] / "state" / "history.json"
    paper_template = read_template("paper_note.org")
    papers_dir = expand_roam_path(config, config["paths"]["papers_dir"])

    note_links = {}
    pending_writes = []

    for paper in ranked["papers"]:
        should_write = bool(reviewed_field(paper, "should_deep_read", False))
        if should_write:
            note_path, pending = write_paper_note(paper_template, papers_dir, paper, ranked["mode"], ranked.get("days", 0))
            note_links[paper["title"]] = note_path
            if pending:
                pending_writes.append(pending)

    daily_file, daily_write = render_daily(config, ranked, note_links)
    pending_writes.append(daily_write)
    write_roam_files(pending_writes)
    save_history(history_path, ranked["papers"], ranked.get("date") or datetime.now().strftime("%Y-%m-%d"))

    print(f"Wrote daily recommendations to {daily_file}")
    for title, path in note_links.items():
        print(f"Wrote paper note: {title} -> {path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
