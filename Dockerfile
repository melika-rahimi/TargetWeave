# Installs runtime packages from renv.lock (not the host library).
# Build: docker build -t targetweave .
# Secrets must be supplied at runtime. Do not copy .Renviron.

FROM rocker/r-ver:4.3.3

RUN apt-get update && apt-get install -y --no-install-recommends \
    libpq-dev \
    libsodium-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    curl \
    pkg-config \
    libuv1-dev \
    zlib1g-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

RUN R -e "install.packages('renv', repos = 'https://cloud.r-project.org')"

COPY DESCRIPTION renv.lock scripts/check_renv_imports.R ./
ENV RENV_CONFIG_REPOS_OVERRIDE=https://cloud.r-project.org
# Restore pinned versions from renv.lock only. Do not copy a host library.
RUN R -e "renv::consent(provided = TRUE); renv::restore(lockfile = 'renv.lock', prompt = FALSE, library = .libPaths()[[1]])"
RUN Rscript check_renv_imports.R

COPY app.R ./
COPY R ./R
COPY www ./www

ENV TW_ENV=production
ENV TW_DISPLAY_TZ=UTC
ENV PORT=3838
EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('.', host='0.0.0.0', port=as.integer(Sys.getenv('PORT','3838')))"]
