"""Fetch candidate papers from HuggingFace Daily and arXiv."""

from __future__ import annotations

import argparse
import json
import sys
import urllib.parse
import urllib.request
import xml.etree.ElementTree as ET
from datetime import timedelta
from pathlib import Path

from agent.dailypaper.scripts.common import compact, daterange_backwards, extract_arxiv_id, load_config, parse_iso_day, save_json


ATOM_NS = {
    "atom": "http://www.w3.org/2005/Atom",
    "arxiv": "http://arxiv.org/schemas/atom",
}


def fetch_json(url: str) -> list[dict]:
    request = urllib.request.Request(url, headers={"User-Agent": "dailypaper/1.0"})
    with urllib.request.urlopen(request, timeout=20) as response:
        return json.loads(response.read().decode("utf-8"))


def fetch_text(url: str) -> str:
    request = urllib.request.Request(url, headers={"User-Agent": "dailypaper/1.0"})
    with urllib.request.urlopen(request, timeout=20) as response:
        return response.read().decode("utf-8", errors="replace")


def fetch_hf_daily(target_day, limit: int) -> list[dict]:
    url = f"https://huggingface.co/api/daily_papers?date={target_day.isoformat()}&limit={limit}"
    rows = fetch_json(url)
    results = []
    for row in rows:
        paper = row.get("paper", {})
        paper_id = paper.get("id", "")
        if not paper_id:
            continue
        results.append({
            "title": paper.get("title", "").strip(),
            "summary": compact(paper.get("summary", "")),
            "authors": [author.get("name", "") for author in paper.get("authors", []) if author.get("name")],
            "url": f"https://arxiv.org/abs/{paper_id}",
            "pdf_url": f"https://arxiv.org/pdf/{paper_id}.pdf",
            "arxiv_id": paper_id,
            "figure_url": paper.get("thumbnail", ""),
            "published_at": paper.get("publishedAt", "")[:10],
            "source": "hf-daily",
            "source_score": row.get("num_upvotes", 0) or 0,
        })
    return results


def fetch_hf_trending(limit: int) -> list[dict]:
    url = f"https://huggingface.co/api/daily_papers?sort=trending&limit={limit}"
    rows = fetch_json(url)
    results = []
    for row in rows:
        paper = row.get("paper", {})
        paper_id = paper.get("id", "")
        if not paper_id:
            continue
        results.append({
            "title": paper.get("title", "").strip(),
            "summary": compact(paper.get("summary", "")),
            "authors": [author.get("name", "") for author in paper.get("authors", []) if author.get("name")],
            "url": f"https://arxiv.org/abs/{paper_id}",
            "pdf_url": f"https://arxiv.org/pdf/{paper_id}.pdf",
            "arxiv_id": paper_id,
            "figure_url": paper.get("thumbnail", ""),
            "published_at": paper.get("publishedAt", "")[:10],
            "source": "hf-trending",
            "source_score": row.get("num_upvotes", 0) or 0,
        })
    return results


def fetch_arxiv(categories: list[str], max_results: int, start_day, end_day) -> list[dict]:
    query = " OR ".join(f"cat:{category}" for category in categories)
    params = urllib.parse.urlencode({
        "search_query": query,
        "sortBy": "submittedDate",
        "sortOrder": "descending",
        "start": 0,
        "max_results": max_results,
    })
    xml_text = fetch_text(f"https://export.arxiv.org/api/query?{params}")
    root = ET.fromstring(xml_text)
    rows = []
    for entry in root.findall("atom:entry", ATOM_NS):
        published = (entry.findtext("atom:published", default="", namespaces=ATOM_NS) or "")[:10]
        if published:
            day = parse_iso_day(published)
            if day < start_day or day > end_day:
                continue
        url = entry.findtext("atom:id", default="", namespaces=ATOM_NS)
        title = compact(entry.findtext("atom:title", default="", namespaces=ATOM_NS))
        summary = compact(entry.findtext("atom:summary", default="", namespaces=ATOM_NS))
        authors = [author.findtext("atom:name", default="", namespaces=ATOM_NS)
                   for author in entry.findall("atom:author", ATOM_NS)]
        arxiv_id = extract_arxiv_id(url)
        rows.append({
            "title": title,
            "summary": summary,
            "authors": [author for author in authors if author],
            "url": url,
            "pdf_url": f"https://arxiv.org/pdf/{arxiv_id}.pdf" if arxiv_id else "",
            "arxiv_id": arxiv_id,
            "figure_url": "",
            "published_at": published,
            "source": "arxiv",
            "source_score": 0,
        })
    return rows


def dedup(papers: list[dict]) -> list[dict]:
    by_id = {}
    for paper in papers:
        key = paper.get("arxiv_id") or paper.get("url")
        if not key:
            continue
        existing = by_id.get(key)
        if existing is None or paper.get("source_score", 0) > existing.get("source_score", 0):
            by_id[key] = paper
    return list(by_id.values())


def main() -> int:
    parser = argparse.ArgumentParser(description="Fetch paper candidates")
    parser.add_argument("--days", type=int, default=1)
    parser.add_argument("--date", default=None)
    parser.add_argument("--config", default=None)
    parser.add_argument("--output", default=None)
    args = parser.parse_args()

    config = load_config(args.config)
    end_day = parse_iso_day(args.date)
    start_day = end_day - timedelta(days=max(args.days - 1, 0))
    source_config = config["data_sources"]

    try:
        papers = []
        for day in daterange_backwards(end_day, args.days):
            papers.extend(fetch_hf_daily(day, source_config["hf_daily_limit"]))
        papers.extend(fetch_hf_trending(source_config["hf_trending_limit"]))
        papers.extend(fetch_arxiv(source_config["arxiv_categories"], source_config["arxiv_max_results"], start_day, end_day))
        merged = dedup(papers)
        payload = {
            "date": end_day.isoformat(),
            "days": args.days,
            "papers": merged,
        }
    except Exception as exc:  # pragma: no cover - runtime failure path
        print(f"fetch_candidates failed: {exc}", file=sys.stderr)
        return 1

    if args.output:
        save_json(Path(args.output), payload)
    else:
        print(json.dumps(payload, ensure_ascii=False, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
