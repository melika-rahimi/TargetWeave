with_host_tz <- function(tz, expr) {
  old <- Sys.getenv("TZ", unset = NA_character_)
  on.exit(
    {
      if (is.na(old) || !nzchar(old)) {
        Sys.unsetenv("TZ")
      } else {
        Sys.setenv(TZ = old)
      }
    },
    add = TRUE
  )
  Sys.setenv(TZ = tz)
  force(expr)
}

with_display_tz <- function(tz, expr) {
  old <- Sys.getenv("TW_DISPLAY_TZ", unset = NA_character_)
  on.exit(
    {
      if (is.na(old) || !nzchar(old)) {
        Sys.unsetenv("TW_DISPLAY_TZ")
      } else {
        Sys.setenv(TW_DISPLAY_TZ = old)
      }
    },
    add = TRUE
  )
  if (is.null(tz) || !nzchar(tz)) {
    Sys.unsetenv("TW_DISPLAY_TZ")
  } else {
    Sys.setenv(TW_DISPLAY_TZ = tz)
  }
  force(expr)
}

test_that("no configured timezone displays stored UTC", {
  captured <- as.POSIXct("2026-09-11 21:50:00", tz = "UTC")
  with_display_tz(NULL, {
    expect_equal(resolve_display_tz(), "UTC")
    expect_equal(format_user_timestamp(captured), "11 Sep 2026, 21:50 UTC")
    expect_equal(format_user_timestamp("2026-09-11T21:50:00Z"), "11 Sep 2026, 21:50 UTC")
    expect_equal(format_utc_iso(captured), "2026-09-11T21:50:00Z")
    expect_equal(resolve_display_tz(""), "UTC")
    expect_equal(resolve_display_tz("Not/AZone"), "UTC")
  })
})

test_that("Europe/Amsterdam config is DST-aware", {
  captured <- as.POSIXct("2026-09-11 21:50:00", tz = "UTC")
  summer <- as.POSIXct("2026-07-15 12:00:00", tz = "UTC")
  winter <- as.POSIXct("2026-01-15 12:00:00", tz = "UTC")
  with_display_tz("Europe/Amsterdam", {
    expect_equal(resolve_display_tz(), "Europe/Amsterdam")
    expect_equal(format_user_timestamp(captured), "11 Sep 2026, 23:50 CEST")
    expect_equal(format_user_timestamp(summer), "15 Jul 2026, 14:00 CEST")
    expect_equal(format_user_timestamp(winter), "15 Jan 2026, 13:00 CET")
    expect_equal(format_user_timestamp("2026-07-15T12:00:00Z"), "15 Jul 2026, 14:00 CEST")
    expect_equal(format_user_timestamp("2026-01-15T12:00:00Z"), "15 Jan 2026, 13:00 CET")
  })
  expect_equal(format_user_timestamp(captured, tz = "Europe/Amsterdam"), "11 Sep 2026, 23:50 CEST")
})

test_that("snapshot name, list, detail, and export captured times agree", {
  captured <- as.POSIXct("2026-09-11 21:50:00", tz = "UTC")
  generated <- as.POSIXct("2026-09-11 21:51:00", tz = "UTC")
  local_same <- as.POSIXct("2026-09-11 23:50:00", tz = "Europe/Amsterdam")
  with_display_tz(NULL, {
    shown <- format_user_timestamp(captured)
    expect_equal(shown, "11 Sep 2026, 21:50 UTC")
    expect_equal(default_snapshot_name(captured), paste("Snapshot \u00b7", shown))
    expect_equal(default_snapshot_name(local_same), default_snapshot_name(captured))
    expect_equal(export_format_when(captured), shown)
    expect_equal(export_format_when(generated), format_user_timestamp(generated))
    expect_equal(
      sprintf("Captured %s", format_user_timestamp(captured)),
      "Captured 11 Sep 2026, 21:50 UTC"
    )
  })
  with_display_tz("Europe/Amsterdam", {
    shown <- format_user_timestamp(captured)
    expect_equal(shown, "11 Sep 2026, 23:50 CEST")
    expect_equal(default_snapshot_name(captured), paste("Snapshot \u00b7", shown))
    expect_equal(default_snapshot_name(local_same), default_snapshot_name(captured))
    expect_equal(export_format_when(captured), shown)
    expect_equal(format_user_timestamp(generated), "11 Sep 2026, 23:51 CEST")
  })
})

test_that("local Sys.time instants and UTC POSIXct use the same formatter", {
  now <- Sys.time()
  utc <- as.POSIXct(as.numeric(now), origin = "1970-01-01", tz = "UTC")
  with_display_tz(NULL, {
    expect_equal(format_user_timestamp(now), format_user_timestamp(utc))
    expect_equal(default_snapshot_name(now), default_snapshot_name(utc))
    expect_equal(format_utc_iso(now), format_utc_iso(utc))
  })
  with_display_tz("Europe/Amsterdam", {
    expect_equal(format_user_timestamp(now), format_user_timestamp(utc))
    expect_equal(default_snapshot_name(now), default_snapshot_name(utc))
  })
  with_host_tz("America/New_York", {
    with_display_tz("Europe/Amsterdam", {
      expect_equal(format_user_timestamp(now), format_user_timestamp(utc))
    })
    with_display_tz(NULL, {
      expect_equal(format_user_timestamp(now), format_user_timestamp(utc))
    })
  })
})

test_that("auto snapshot names are rewritten from captured time", {
  expect_true(is_auto_snapshot_name("Snapshot \u00b7 12 Sep 2026, 00:16 CEST"))
  expect_true(is_auto_snapshot_name("Snapshot \u00b7 11 Sep 2026, 22:16 UTC"))
  expect_false(is_auto_snapshot_name("EGFR baseline"))
  expect_false(is_auto_snapshot_name("Snapshot \u00b7 EGFR first look"))
  captured <- as.POSIXct("2026-09-11 21:50:00", tz = "UTC")
  with_display_tz(NULL, {
    expect_equal(
      align_snapshot_name("Snapshot \u00b7 12 Sep 2026, 00:16 CEST", captured),
      "Snapshot \u00b7 11 Sep 2026, 21:50 UTC"
    )
    expect_equal(align_snapshot_name("Baseline review", captured), "Baseline review")
  })
  with_display_tz("Europe/Amsterdam", {
    expect_equal(
      align_snapshot_name("Snapshot \u00b7 11 Sep 2026, 21:50 UTC", captured),
      "Snapshot \u00b7 11 Sep 2026, 23:50 CEST"
    )
  })
})

test_that("snapshot and dossier display paths do not format Sys.time() directly", {
  files <- c(
    "R/process/process_snapshots.R",
    "R/modules/mod_research.R",
    "R/export/export_dossier.R",
    "R/export/export_manifest.R"
  )
  for (rel in files) {
    src <- paste(readLines(file.path(app_root(), rel), warn = FALSE), collapse = "\n")
    expect_false(
      grepl("format\\(Sys\\.time\\(", src),
      info = rel
    )
    expect_false(
      grepl("strftime\\(Sys\\.time\\(", src),
      info = rel
    )
  }
  snap_src <- paste(readLines(file.path(app_root(), "R/process/process_snapshots.R"), warn = FALSE), collapse = "\n")
  expect_match(snap_src, "format_user_timestamp\\(when\\)")
})

test_that("host timezone does not affect configured display time", {
  captured <- as.POSIXct("2026-09-11 21:50:00", tz = "UTC")
  with_display_tz(NULL, {
    with_host_tz("UTC", {
      expect_equal(format_user_timestamp(captured), "11 Sep 2026, 21:50 UTC")
    })
    with_host_tz("America/New_York", {
      expect_equal(format_user_timestamp(captured), "11 Sep 2026, 21:50 UTC")
      expect_equal(default_snapshot_name(captured), "Snapshot \u00b7 11 Sep 2026, 21:50 UTC")
    })
    with_host_tz("Europe/Amsterdam", {
      expect_equal(format_user_timestamp(captured), "11 Sep 2026, 21:50 UTC")
    })
  })
  with_display_tz("Europe/Amsterdam", {
    with_host_tz("UTC", {
      expect_equal(format_user_timestamp(captured), "11 Sep 2026, 23:50 CEST")
    })
    with_host_tz("America/New_York", {
      expect_equal(format_user_timestamp(captured), "11 Sep 2026, 23:50 CEST")
    })
  })
})
