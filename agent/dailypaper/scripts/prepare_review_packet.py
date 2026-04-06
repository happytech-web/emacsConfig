"""Prepare compact packets for agent review."""

from __future__ import annotations

import argparse
import json
from pathlib import Path

from agent.dailypaper.scripts.common import extract_arxiv_id, load_config, load_json, save_json, today_iso


def build_classic_packet(config: dict, topic: str) -> dict:
    rows = []
    for item in config.get("classic_topics", {}).get(topic, []):
        rows.append({
            "title": item.get("title", ""),
            "url": item.get("url", ""),
            "arxiv_id": extract_arxiv_id(item.get("url", "")),
            "authors": item.get("authors", []),
            "published_at": item.get("published_at", ""),
            "summary": item.get("summary", ""),
            "figure_url": item.get("figure_url", ""),
            "source": "classic-topic",
            "seed_reason": item.get("reason", ""),
            "review": {
                "score": None,
                "bucket": None,
                "reason": "",
                "should_deep_read": None,
                "deep_read": {
                    "一句话总结": "",
                    "核心贡献": [],
                    "要解决的问题": "",
                    "现有方法局限": "",
                    "本文动机": "",
                    "整体框架": "",
                    "核心模块": [],
                    "关键公式": [],
                    "实验结果": "",
                    "批判性思考": {
                        "优点": [],
                        "局限性": [],
                        "潜在改进": []
                    },
                    "相关工作": [],
                    "后续阅读": []
                }
            }
        })
    return {
        "date": today_iso(),
        "days": 0,
        "mode": "classic-learning",
        "topic": topic,
        "papers": rows,
    }


def build_packet(payload: dict, mode: str, topic: str) -> dict:
    rows = []
    for paper in payload["papers"]:
        rows.append({
            "title": paper.get("title", ""),
            "url": paper.get("url", ""),
            "arxiv_id": paper.get("arxiv_id", ""),
            "authors": paper.get("authors", []),
            "published_at": paper.get("published_at", ""),
            "summary": paper.get("summary", ""),
            "figure_url": paper.get("figure_url", ""),
            "source": paper.get("source", ""),
            "source_score": paper.get("source_score", 0),
            "filter_score": paper.get("filter_score", 0),
            "filter_matches": paper.get("filter_matches", {}),
            "review": {
                "score": None,
                "bucket": None,
                "reason": "",
                "should_deep_read": None,
                "deep_read": {
                    "一句话总结": "",
                    "核心贡献": [],
                    "要解决的问题": "",
                    "现有方法局限": "",
                    "本文动机": "",
                    "整体框架": "",
                    "核心模块": [],
                    "关键公式": [],
                    "实验结果": "",
                    "批判性思考": {
                        "优点": [],
                        "局限性": [],
                        "潜在改进": []
                    },
                    "相关工作": [],
                    "后续阅读": []
                }
            }
        })
    return {
        "date": payload.get("date", ""),
        "days": payload.get("days", 0),
        "mode": mode,
        "topic": topic,
        "papers": rows,
    }


def main() -> int:
    parser = argparse.ArgumentParser(description="Prepare review packet")
    parser.add_argument("--input", required=False)
    parser.add_argument("--output", required=False)
    parser.add_argument("--config", default=None)
    parser.add_argument("--mode", required=True)
    parser.add_argument("--topic", default="")
    args = parser.parse_args()

    config = load_config(args.config)
    if args.mode == "classic-learning":
        packet = build_classic_packet(config, args.topic)
    else:
        packet = build_packet(load_json(Path(args.input)), args.mode, args.topic)

    if args.output:
        save_json(Path(args.output), packet)
    else:
        print(json.dumps(packet, ensure_ascii=False, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
