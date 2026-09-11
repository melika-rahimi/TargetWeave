# Fixtures

Offline fixtures under `tests/testdata/` test **parser semantics and response shape**, not the current live genome coordinates.

Do not regenerate them because a live lookup returned a newer gene version or a shifted `start`/`end`.

## Ensembl EGFR overview fixture

- File: `tests/testdata/ensembl_overview_egfr.json`
- Source: Ensembl REST gene lookup payload for `ENSG00000146648` (`display_name` EGFR, `object_type` Gene)
- Retrieval date: not recorded at capture; payload is historical (`version` 22, `start` 55019017)
- What the fixture test guarantees: `parse_ensembl_gene()` / overview genomic mapping still read `id`, `seq_region_name`, `start`/`end`, `strand`, `biotype`, `display_name`
- What it does **not** guarantee: current Ensembl assembly coordinates or gene version

Observed live EGFR Ensembl lookup (2026-09-11): `version` 23, `start` 55018820, `seq_region_name` `7`. Keep the older fixture for parser coverage.

Refresh a fixture only when:

1. Live tests fail for a durable field/key change (not a timeout/5xx).
2. The parser meaning is reviewed.
3. Source and date are recorded in the commit message or this file.
