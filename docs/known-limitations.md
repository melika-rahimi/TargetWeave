# Known limitations

- Human-only v1. Organism is Homo sapiens.
- Results depend on public source availability and rate limits.
- Live API response shapes can drift; tests with `TW_LIVE_API=1` are the canary.
- PubMed corpus is a defined query, not a complete literature review. Abstracts are not stored.
- Reactome membership is not statistical enrichment.
- Experimental PDB only. No AlphaFold.
- No clinical recommendation.
- PDF export is deferred. HTML/ZIP dossiers are snapshot-only.
- Support requests are stored for later handling; admin tooling is incomplete.
- Project delete is deferred. Archive is not delete.
- No collaboration, sharing, public projects, email notifications, billing, or AI summaries.
