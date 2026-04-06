"""Bridge org-roam file writes through batch Emacs."""

from __future__ import annotations

import subprocess

from agent.dailypaper.scripts.common import ROOT, STATE_DIR, save_json


EMACS_INIT = ROOT.parents[1] / "init.el"


def write_roam_files(files: list[dict]) -> None:
    payload_path = STATE_DIR / "latest-emacs-payload.json"
    save_json(payload_path, {"files": files})
    subprocess.run([
        "emacs",
        "--batch",
        "-l",
        str(EMACS_INIT),
        "--eval",
        f'(my/org-paper-batch-apply-payload "{payload_path}")',
    ], check=True)
