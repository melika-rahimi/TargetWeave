# Data stored by TargetWeave

This describes the implementation, not a legal policy.

## Private account data

- Email and sodium password hash
- Optional profile (display name, role, field, institution)
- Projects (title, research question, disease context, candidate targets, confirmed identities)
- Research notes
- Evidence snapshots
- Support request subject/message

Workspaces are private to the account. Profile fields do not change scientific retrieval.

## Public-source cache

`api_cache` stores external biomedical records and query results keyed by public identifiers. It can be rebuilt. It is not the research record.

## What is sent outbound

Account data is not sent to scientific APIs except the scientific identifiers and query context required for retrieval (for example UniProt accessions, Ensembl IDs, disease ontology IDs, NCBI tool/email as API identity).

Notes, support messages, and passwords are not sent to those APIs.
