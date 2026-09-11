# Technical backlog

## RC live Ensembl (2026-09-11)

Live `TW_LIVE_API=1` cross-check for EGFR/KRAS/TP53 returned `partial_uniprot` while Ensembl REST answered HTTP 500/503 (and occasional timeouts). UniProt still returned accessions and Ensembl GeneId xrefs. TargetWeave did not force `matched`. Treat as transient source failure, not as a parser change, unless a later live run with Ensembl 200 still fails the join.

Gated `TW_LIVE_API=1` checks may still fail when sources are unreachable or when UniProt Ensembl xrefs / Ensembl lookup payloads differ from fixtures.

M11 parser hardening (does not hide failures):

- UniProt xref property keys are matched case-insensitively (`GeneId`).
- Ensembl genomic region accepts `seq_region_name`, `seqRegionName`, or nested `seq_region`.

Do not weaken live assertions. If a live run shows a durable shape change, refresh the fixture with source and date recorded.

## Snapshot / note ownership if a project is deleted

There is still no user-facing project **delete** (archive + restore). Intended semantics:

- Archive leaves `evidence_snapshots` and `research_notes` intact.
- If project deletion is added later, FK `ON DELETE CASCADE` from snapshots/notes → `projects` removes the research record with the project (owner-initiated).
- Live target deletion cascades **target notes** only after explicit confirmation when notes exist. Snapshot JSONB is frozen.
- `api_cache` remains a public technical cache and is never used as a snapshot store.

## Deferred product work

AlphaFold, PDF export, collaboration, sharing, email, admin dashboard, billing, public projects.
