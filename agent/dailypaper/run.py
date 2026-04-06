"""CLI entrypoint for the staged dailypaper workflow."""

from __future__ import annotations

import argparse
import subprocess
import sys
from pathlib import Path

from agent.dailypaper.scripts.common import ROOT, STATE_DIR, load_config, parse_iso_day


def run_step(command: list[str]) -> None:
    subprocess.run(command, check=True)


def latest_paths() -> dict[str, Path]:
    STATE_DIR.mkdir(parents=True, exist_ok=True)
    return {
        "fetched": STATE_DIR / "latest-fetched.json",
        "filtered": STATE_DIR / "latest-filtered.json",
        "review_packet": STATE_DIR / "latest-review-packet.json",
        "ranked": STATE_DIR / "latest-ranked.json",
    }


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="staged org-roam paper recommendation workflow")
    parser.add_argument("--config", default=None)
    subparsers = parser.add_subparsers(dest="command", required=True)

    recommend = subparsers.add_parser("recommend")
    recommend.add_argument("--days", type=int, default=1)
    recommend.add_argument("--date", default=parse_iso_day(None).isoformat())
    recommend.add_argument("--area", default="robot-learning")
    recommend.add_argument("--project", default="")
    recommend.add_argument("--project-text", default="")
    recommend.add_argument("--topic", default="")

    classic = subparsers.add_parser("classic")
    classic.add_argument("--topic", required=True)
    classic.add_argument("--area", default="")
    classic.add_argument("--project", default="")
    classic.add_argument("--date", default=parse_iso_day(None).isoformat())

    transfer = subparsers.add_parser("transfer")
    transfer.add_argument("--topic", required=True)
    transfer.add_argument("--days", type=int, default=3)
    transfer.add_argument("--area", default="robot-learning")
    transfer.add_argument("--project", default="")
    transfer.add_argument("--project-text", default="")
    transfer.add_argument("--date", default=parse_iso_day(None).isoformat())

    refresh = subparsers.add_parser("refresh-indexes")
    refresh.add_argument("--date", default=parse_iso_day(None).isoformat())
    finalize = subparsers.add_parser("finalize")
    finalize.add_argument("--input", required=True)
    return parser


def main() -> int:
    parser = build_parser()
    args = parser.parse_args()
    config_path = str(Path(args.config).expanduser()) if args.config else str(ROOT / "config.json")
    paths = latest_paths()

    if args.command == "refresh-indexes":
        run_step([
            sys.executable, "-m", "agent.dailypaper.scripts.refresh_indexes",
            "--config", config_path,
        ])
        return 0

    if args.command == "finalize":
        run_step([
            sys.executable, "-m", "agent.dailypaper.scripts.validate_reviewed_packet",
            "--input", args.input,
        ])
        run_step([
            sys.executable, "-m", "agent.dailypaper.scripts.write_org_outputs",
            "--input", args.input,
            "--config", config_path,
        ])
        config = load_config(args.config)
        if config["automation"].get("auto_refresh_indexes", True):
            run_step([
                sys.executable, "-m", "agent.dailypaper.scripts.refresh_indexes",
                "--config", config_path,
            ])
        return 0

    if args.command == "classic":
        run_step([
            sys.executable, "-m", "agent.dailypaper.scripts.prepare_review_packet",
            "--mode", "classic-learning",
            "--topic", args.topic,
            "--config", config_path,
            "--output", str(paths["review_packet"]),
        ])
        print(f"Prepared classic review packet at {paths['review_packet']}")
        return 0

    mode = "cross-domain-transfer" if args.command == "transfer" else ("project-support" if (args.project_text or args.project) else "area-explore")

    run_step([
        sys.executable, "-m", "agent.dailypaper.scripts.fetch_candidates",
        "--days", str(args.days),
        "--date", args.date,
        "--config", config_path,
        "--output", str(paths["fetched"]),
    ])
    run_step([
        sys.executable, "-m", "agent.dailypaper.scripts.filter_candidates",
        "--input", str(paths["fetched"]),
        "--area", args.area,
        "--project", args.project,
        "--project-text", args.project_text,
        "--topic", args.topic,
        "--config", config_path,
        "--output", str(paths["filtered"]),
    ])
    run_step([
        sys.executable, "-m", "agent.dailypaper.scripts.prepare_review_packet",
        "--input", str(paths["filtered"]),
        "--mode", mode,
        "--topic", args.topic,
        "--config", config_path,
        "--output", str(paths["review_packet"]),
    ])
    print(f"Prepared review packet at {paths['review_packet']}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
