"""Load export tiers and ownership, including the generated declaration projection."""

from pathlib import Path
import tomllib


def load_document(path: Path, generated: Path | None = None) -> dict:
    document = tomllib.loads(path.read_text(encoding="utf-8"))
    if generated is None:
        return document
    derived = tomllib.loads(generated.read_text(encoding="utf-8"))
    for tier in ("non-declarable", "non-declarable-stdlib"):
        symbols = document.get(tier, [] if tier == "non-declarable-stdlib" else None)
        if isinstance(symbols, list):
            document[tier] = symbols + derived.get(tier, [])
    # Preserve malformed input for the existing schema validator to diagnose.
    ownership = document.get("ownership")
    if isinstance(ownership, dict) and isinstance(ownership.get("contracts"), list):
        ownership["contracts"].extend(derived.get("ownership", {}).get("contracts", []))
    return document
