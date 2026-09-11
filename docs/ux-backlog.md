# UX backlog

## Resolved in pre-release polish (2026-09-11)

- Landing product explainer (SVG/CSS, ~12s loop, reduced motion, keyboard source nodes, pause when the document is hidden)
- Landing sources list includes UniProt, Ensembl, Open Targets, Reactome, PubMed, and RCSB PDB
- Local retrieval states (no full-page busy overlay); EGFR retrieving does not disable KRAS/TP53
- Shared panel empty/loading/error language (`Not retrieved`, `Retrieving`, `No source result`, `Unavailable` / source temporarily unavailable)
- Target-row retrieving pill and local “Retrieving identity…” copy
- Snapshot vs live visual distinction (`This is not the live workspace`) and export label grouping (Research dossier HTML / Data package ZIP)
- Account/auth spacing, form widths, password toggle ARIA, and complete source list in product guidance
- Compact status pills, table overflow, 390px header wrap, visible keyboard focus on public nav

## A. Remaining before a later RC (only if live check fails)

- Confirm landing animation on Docker/browser RC host (desktop, 390px, reduced motion)
- Confirm EGFR lookup then immediate KRAS selection in Docker (async acceptance)
- Workspace tour / first-investigation guide on a narrow live session (popover-in-viewport)

## B. Safe to defer after first deployment

- Denser Compare matrix typography
- Literature article-card density tweaks beyond current compact table
- Account form field-level inline validation copy
- Additional empty-state illustrations (copy-only states are in place)
- `{shinytest2}` workspace tour browser automation (skipped when the package is not installed)

## C. Feature / out of scope (do not start)

- AlphaFold / predicted structures
- PDF export
- Sharing / collaboration
- Admin dashboard
- AI summaries
- New biomedical APIs or evidence types
- Project deletion
- Billing / notifications
- Deployment automation

Historical note: this file did not exist before the pre-release polish sweep. Technical source/outage notes remain in `docs/technical-backlog.md`.
