"""Validate reviewed packets before finalize."""

from __future__ import annotations

import argparse
from pathlib import Path

from agent.dailypaper.scripts.common import load_json


REQUIRED_BUCKETS = {
    "urgent_follow",
    "direction_core",
    "classics",
    "transferable_ideas",
    "skip",
}


def _require(condition: bool, message: str, errors: list[str]) -> None:
    if not condition:
        errors.append(message)


def validate_paper(paper: dict, index: int, errors: list[str]) -> None:
    review = paper.get("review", {})
    prefix = f"paper[{index}] {paper.get('title', '<untitled>')}:"
    _require(review.get("score") is not None, f"{prefix} missing review.score", errors)
    _require(review.get("bucket") in REQUIRED_BUCKETS, f"{prefix} invalid review.bucket", errors)
    _require(bool((review.get("reason") or "").strip()), f"{prefix} missing review.reason", errors)
    _require(review.get("should_deep_read") in (True, False), f"{prefix} missing review.should_deep_read", errors)

    if review.get("should_deep_read") is True:
        deep_read = review.get("deep_read", {})
        _require(bool((deep_read.get("一句话总结") or "").strip()), f"{prefix} missing deep_read.一句话总结", errors)
        _require(len(deep_read.get("核心贡献", [])) >= 3, f"{prefix} need >=3 核心贡献", errors)
        _require(bool((deep_read.get("要解决的问题") or "").strip()), f"{prefix} missing deep_read.要解决的问题", errors)
        _require(bool((deep_read.get("现有方法局限") or "").strip()), f"{prefix} missing deep_read.现有方法局限", errors)
        _require(bool((deep_read.get("本文动机") or "").strip()), f"{prefix} missing deep_read.本文动机", errors)
        _require(bool((deep_read.get("整体框架") or "").strip()), f"{prefix} missing deep_read.整体框架", errors)
        _require(len(deep_read.get("核心模块", [])) >= 1, f"{prefix} need >=1 核心模块", errors)
        _require(bool((deep_read.get("实验结果") or "").strip()), f"{prefix} missing deep_read.实验结果", errors)
        critique = deep_read.get("批判性思考", {})
        _require(len(critique.get("优点", [])) >= 1, f"{prefix} missing 批判性思考.优点", errors)
        _require(len(critique.get("局限性", [])) >= 1, f"{prefix} missing 批判性思考.局限性", errors)
        _require(len(critique.get("潜在改进", [])) >= 1, f"{prefix} missing 批判性思考.潜在改进", errors)
        _require(len(deep_read.get("相关工作", [])) >= 1, f"{prefix} missing 深读相关工作", errors)
        _require(len(deep_read.get("后续阅读", [])) >= 1, f"{prefix} missing 深读后续阅读", errors)


def main() -> int:
    parser = argparse.ArgumentParser(description="Validate reviewed packet")
    parser.add_argument("--input", required=True)
    args = parser.parse_args()

    payload = load_json(Path(args.input))
    errors: list[str] = []
    papers = payload.get("papers", [])
    _require(isinstance(papers, list) and len(papers) > 0, "payload must contain non-empty papers list", errors)
    for index, paper in enumerate(papers):
        validate_paper(paper, index, errors)

    if errors:
        for error in errors:
            print(error)
        return 1

    print("reviewed packet validation passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
