# Operations

## Environment modes

- Development: `TW_ENV=development`, application schema, public/dev data.
- Test: testthat sets `TESTTHAT=true`, pool `search_path=tw_test`. Not inferred from UI query strings.
- Production: `TW_ENV=production`. Refuses `tw_test`. Sets `shiny.sanitize.errors`.

## Display timezone

Database timestamps stay UTC. Human-readable timestamps use `TW_DISPLAY_TZ` (DST-aware, with a zone abbreviation). The default is `UTC`. Do not infer the zone from the Docker or host timezone.

Deployments may set `TW_DISPLAY_TZ` intentionally, for example `Europe/Amsterdam`. This release does not detect the browser timezone or store a per-user timezone.

## Migrations

`ensure_schema()` creates tables/columns idempotently (`IF NOT EXISTS`).

Named migrations live in `schema_migrations` (`R/db/migrations.R`). Startup does not DELETE user scientific rows.

Destructive repairs (duplicate targets) remain explicit scripts such as `scripts/repair_duplicate_targets.R`.

## Logging

JSON lines on stderr via `tw_log`. Events include startup, shutdown, DB failure, external API failure, snapshot create, export failure.

Do not log passwords, API keys, note bodies, support bodies, WebEnv/query_key, or full payloads. Session token is truncated as operational correlation only.

## Health

`GET /healthz` → `{ok, app, db}`. Database `SELECT 1` only.

## Backups

Use PostgreSQL `pg_dump`. Do not build a backup system inside Shiny.

Prioritize `evidence_snapshots` and `research_notes`. Cache may be omitted.

Dump example (development database; adjust host/user):

```bash
pg_dump -Fc -h 127.0.0.1 -U targetweave -d targetweave \
  --exclude-table-data=api_cache \
  -f targetweave.dump
```

Restore into an empty temporary database (does not touch the running app DB):

```bash
createdb targetweave_restore
pg_restore -d targetweave_restore --no-owner targetweave.dump
psql -d targetweave_restore -c "SELECT count(*) FROM evidence_snapshots;"
psql -d targetweave_restore -c "SELECT count(*) FROM research_notes;"
psql -d targetweave_restore -c "SELECT count(*) FROM projects;"
dropdb targetweave_restore
```

## HTTPS

Serve production over HTTPS. Do not invent custom session cryptography. Shiny cookies follow the reverse-proxy TLS setup.

## Fixture refresh

Offline fixtures test parser **shape**, not live genomic coordinates. See `docs/fixtures.md`.

When `TW_LIVE_API=1` tests fail:

1. Confirm whether the live source was transient.
2. If the response shape changed, save a dated fixture under `tests/testdata/` and record source + date in the commit message or backlog.
3. Update the parser only if TargetWeave's interpretation was wrong or the source semantic changed.
4. Do not weaken assertions only to go green.

## Clean install

1. Fresh OS packages: `libpq-dev` `libsodium-dev`.
2. `Rscript -e 'source("scripts/install_packages.R")'`
3. PostgreSQL 14+, empty database, `.Renviron` from `.Renviron.example`.
4. `Rscript -e 'shiny::runApp(port=3838)'` then create a user and project.

## Deployment options compared

| Option | Postgres | Shiny process | Secrets | Domain | Cost / ops |
| --- | --- | --- | --- | --- | --- |
| shinyapps.io / Posit Connect | Connect supports it; shinyapps.io is weaker for private persistent Postgres | Managed | Yes | Connect: custom domain possible | Connect is the commercial fit; shinyapps.io is awkward for this DB model |
| Render / Railway / Fly.io container | Add a managed Postgres add-on | Run the Dockerfile | Env vars | Custom domain typical | Moderate; need memory for plots/export |
| Generic Docker on a VM | Self-managed or RDS-like | `rocker` image + reverse proxy | Env vars | Full control | Lowest lock-in, more ops |

**Recommendation:** Docker image + managed PostgreSQL behind HTTPS (Fly.io, Render, or a small VM). Posit Connect is the best commercial Shiny ops choice if an organization already pays for it. Do not use shinyapps.io as the primary production path because of private Postgres and export/temp-file needs.

Do not deploy until explicitly requested.

## Production image verification (not docker-compose)

`docker-compose.yml` starts **development PostgreSQL only**. It is not the production application configuration.

Use `sudo` if the daemon socket is not writable by your user. Do not put real secrets in the command history if you can avoid it; export placeholders first.

```bash
cd "/path/to/TargetWeave"

export TW_PG_PASSWORD='replace-with-local-test-password'
export TW_NCBI_EMAIL='maintainer@example.org'

# 1. Build the production image (renv.lock restore happens here)
sudo docker build -t targetweave:rc .

# 2. Start a throwaway Postgres for the image (not compose app config)
sudo docker network create tw-rc || true
sudo docker run -d --name tw-rc-pg --network tw-rc \
  -e POSTGRES_DB=targetweave \
  -e POSTGRES_USER=targetweave \
  -e POSTGRES_PASSWORD="$TW_PG_PASSWORD" \
  postgres:16-alpine
sudo docker exec tw-rc-pg pg_isready -U targetweave -d targetweave

# 3. Run TargetWeave from the image
sudo docker run -d --name tw-rc-app --network tw-rc -p 3838:3838 \
  -e TW_ENV=production \
  -e TW_DISPLAY_TZ=UTC \
  -e PGHOST=tw-rc-pg \
  -e PGPORT=5432 \
  -e PGDATABASE=targetweave \
  -e PGUSER=targetweave \
  -e PGPASSWORD="$TW_PG_PASSWORD" \
  -e PGSSLMODE=disable \
  -e NCBI_TOOL=TargetWeave \
  -e NCBI_EMAIL="$TW_NCBI_EMAIL" \
  targetweave:rc

# 4. Logs and health
sudo docker logs tw-rc-app
sudo docker exec tw-rc-app sh -c 'command -v curl >/dev/null && curl -sf http://127.0.0.1:3838/healthz || Rscript -e "con<-url(\"http://127.0.0.1:3838/healthz\"); print(readLines(con,warn=FALSE)); close(con)"'
# From the host:
curl -sf http://127.0.0.1:3838/healthz

# 5. Persistence check: create an account in the UI, then
sudo docker restart tw-rc-app
# Sign in again with the same credentials; the project must still exist.

# 6. Stop and remove the test containers (volume is container-local unless you added one)
sudo docker stop tw-rc-app tw-rc-pg
sudo docker rm tw-rc-app tw-rc-pg
sudo docker network rm tw-rc
# Optional: sudo docker rmi targetweave:rc
```

Expected observations after a successful local run:

| Step | Expected |
| --- | --- |
| Build | `renv::restore` completes; `check_renv_imports.R` prints Imports present |
| First start | Schema/migrations apply; Shiny listens on 3838 |
| Landing | Logo, CSS, JS load; no broken static 404 |
| Account | Create account, sign in, Account Center opens |
| Project | Create project, open workspace |
| Snapshot | Snapshot opens; HTML download writes a file; ZIP download writes a file |
| `/healthz` | JSON `{ok:true, app:..., db:true}` with no secrets |
| Restart | Same user can sign in; project/notes/snapshots still present |
| Sign out | Workspace/private project list gone until sign-in |
| XSS | Project title `<script>alert("x")</script>` and a hostile note render as text in UI and HTML dossier; no alert |

## Mol* iframe

Structures embed RCSB `3d-view` in an iframe and always offer **Open in RCSB PDB** above the viewer. Metadata remains if the iframe is blank or blocked.
