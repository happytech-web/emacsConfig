"""Zero-token filtering before agent review."""

from __future__ import annotations

import argparse
import json
from pathlib import Path

from agent.dailypaper.scripts.common import compact, expand_roam_path, load_config, load_json, read_org_profile, save_json


def flatten(values):
    return [value.lower() for value in values if value]


def keyword_hits(text: str, keywords: list[str]) -> list[str]:
    haystack = text.lower()
    return [keyword for keyword in keywords if keyword.lower() in haystack]


def load_selector_profile(config: dict) -> dict:
    profile_relpath = config.get("paths", {}).get("selector_profile", "")
    if not profile_relpath:
        return {}
    profile_path = expand_roam_path(config, profile_relpath)
    return read_org_profile(profile_path)


def build_context(config: dict, area: str, project: str, project_text: str, topic: str) -> dict:
    areas = {item["slug"]: item for item in config.get("research_areas", [])}
    projects = config.get("project_profiles", {})
    transfers = {item["slug"]: item for item in config.get("transfer_domains", [])}
    area_profile = areas.get(area) or next(iter(areas.values()), {"slug": "", "keywords": []})
    project_profile = projects.get(project, {"name": "", "description": "", "keywords": []}) if project else {"name": "", "description": "", "keywords": []}
    transfer_profile = transfers.get(topic, {"slug": topic, "keywords": [topic], "target_hint": ""}) if topic else {"slug": "", "keywords": [], "target_hint": ""}

    if project_text:
        project_profile = {
            "name": project_profile.get("name", "Ad-hoc Project"),
            "description": project_text,
            "keywords": project_profile.get("keywords", []) + [token.strip() for token in project_text.split() if len(token.strip()) > 3],
        }

    selector = load_selector_profile(config)
    if selector.get("project_description"):
        project_profile["description"] = selector["project_description"]
    if selector.get("current_directions"):
        area_profile = {
            **area_profile,
            "keywords": area_profile.get("keywords", []) + selector["current_directions"],
        }
    if selector.get("include_keywords"):
        project_profile["keywords"] = project_profile.get("keywords", []) + selector["include_keywords"]

    return {
        "area": area_profile,
        "project": project_profile,
        "transfer": transfer_profile,
        "selector": selector,
        "negative": flatten(config.get("negative_keywords", [])) + flatten(selector.get("exclude_keywords", [])),
    }


def filter_one(paper: dict, context: dict) -> dict:
    text = compact(f"{paper.get('title', '')} {paper.get('summary', '')}")
    area_hits = keyword_hits(text, flatten(context["area"].get("keywords", [])))
    project_hits = keyword_hits(text, flatten(context["project"].get("keywords", [])))
    transfer_hits = keyword_hits(text, flatten(context["transfer"].get("keywords", [])))
    boost_hits = keyword_hits(text, flatten(context["selector"].get("boost_keywords", [])))
    negative_hits = keyword_hits(text, context["negative"])
    weights = context["selector"].get("scoring_weights", {})
    preferred_sources = set(context["selector"].get("preferred_sources", []))

    source_bonus = 0
    if paper.get("source") == "hf-trending":
        source_bonus += weights.get("hf_trending_bonus", 2)
    if paper.get("source") in preferred_sources:
        source_bonus += weights.get("preferred_source_bonus", 2)
    if (paper.get("source_score") or 0) >= weights.get("source_score_threshold", 20):
        source_bonus += weights.get("source_score_bonus", 1)

    filter_score = (
        len(project_hits) * weights.get("project", 4)
        + len(area_hits) * weights.get("area", 3)
        + len(transfer_hits) * weights.get("transfer", 2)
        + len(boost_hits) * weights.get("boost", 5)
        + source_bonus
        - len(negative_hits) * weights.get("negative_penalty", 100)
    )
    keep = not negative_hits and filter_score >= 0 and (project_hits or area_hits or transfer_hits or source_bonus >= 2)

    result = dict(paper)
    result.update({
        "filter_score": filter_score,
        "filter_keep": keep,
        "filter_matches": {
            "project": project_hits[:4],
            "area": area_hits[:4],
            "transfer": transfer_hits[:4],
            "boost": boost_hits[:4],
            "negative": negative_hits[:4],
        },
    })
    return result


def main() -> int:
    parser = argparse.ArgumentParser(description="Zero-token filter candidates")
    parser.add_argument("--input", required=True)
    parser.add_argument("--output", required=False)
    parser.add_argument("--config", default=None)
    parser.add_argument("--area", default="robot-learning")
    parser.add_argument("--project", default="")
    parser.add_argument("--project-text", default="")
    parser.add_argument("--topic", default="")
    args = parser.parse_args()

    config = load_config(args.config)
    payload = load_json(Path(args.input))
    context = build_context(config, args.area, args.project, args.project_text, args.topic)
    filtered = [filter_one(paper, context) for paper in payload["papers"]]
    filtered = [paper for paper in filtered if paper["filter_keep"]]
    filtered.sort(key=lambda item: (-item["filter_score"], -(item.get("source_score") or 0), item.get("title", "")))
    selector = context.get("selector", {})
    top_n = selector.get("scoring_weights", {}).get("top_n", config.get("filtering", {}).get("top_n", 24))
    output = {
        "date": payload.get("date", ""),
        "days": payload.get("days", 0),
        "selector_profile": selector,
        "papers": filtered[:top_n],
    }

    if args.output:
        save_json(Path(args.output), output)
    else:
        print(json.dumps(output, ensure_ascii=False, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
