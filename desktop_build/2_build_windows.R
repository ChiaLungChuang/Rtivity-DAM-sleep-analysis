# ============================================================
# STEP 2 — Build Rtivity Windows Installer
#
# Run this script on a WINDOWS machine.
#
# What it produces:
#   build/windows/Rtivity Setup.exe
#   → A self-contained installer. End users need NO R, NO Chrome,
#     NO technical knowledge — just double-click and run.
#
# Prerequisites (run once before this script):
#   1. Install R  https://cran.r-project.org
#   2. Install Node.js (LTS)  https://nodejs.org
#   3. Run 0_install_prerequisites.R to install electricShine
#
# Estimated build time: 10–20 minutes on first run
#   (downloads portable R + Chromium/Electron)
# ============================================================

library(electricShine)

# ── Paths ─────────────────────────────────────────────────
source_dir <- file.path(Sys.getenv("USERPROFILE"), "Desktop", "Rtivity_main")
build_dir  <- file.path(Sys.getenv("USERPROFILE"), "Desktop", "Rtivity_main", "desktop_build", "build", "windows")

# Allow override via environment variable if folder is elsewhere
if (Sys.getenv("RTIVITY_SOURCE") != "") source_dir <- Sys.getenv("RTIVITY_SOURCE")
if (Sys.getenv("RTIVITY_BUILD")  != "") build_dir  <- Sys.getenv("RTIVITY_BUILD")

if (!dir.exists(source_dir)) {
  stop(
    "\nCould not find the Rtivity source folder at:\n  ", source_dir,
    "\n\nIf your Rtivity_main folder is somewhere else, set the path:\n",
    "  Sys.setenv(RTIVITY_SOURCE = 'C:/path/to/Rtivity_main')\n",
    "and re-run this script."
  )
}

dir.create(build_dir, recursive = TRUE, showWarnings = FALSE)

# ── App metadata ───────────────────────────────────────────
app_name    <- "Rtivity"
app_version <- "1.3.0"
description <- "Automated analysis of animal activity and sleep patterns"

# ── R version to bundle (change if a newer CRAN binary is available) ──
r_version_to_bundle <- "4.4.3"

# ── Packages the app needs (auto-installed into the bundle) ───────────
app_packages <- c(
  "shiny", "shinyFiles", "shinyTime", "shinyWidgets", "behavr",
  "data.table", "ggplot2", "damr", "zeitgebr", "sleepr", "ggetho",
  "DT", "fs", "dplyr", "Hmisc", "shinyjs", "readr", "colourpicker",
  "lattice", "survival", "Formula", "stringr", "DescTools",
  "doBy", "zoo", "tm", "scales", "shinythemes",
  "openxlsx", "ActCR",
  "nonlinearTseries", "shinytitle", "tidyverse", "lubridate"
)

# ── Build ──────────────────────────────────────────────────
message("\n========================================")
message(" Building Rtivity Windows installer...")
message(" Source : ", source_dir)
message(" Output : ", build_dir)
message("========================================\n")

tryCatch({
  electricShine::electrify(
    app_name            = app_name,
    short_description   = description,
    semantic_version    = app_version,
    build_path          = build_dir,
    cran_like_url       = "https://cloud.r-project.org",
    r_version           = r_version_to_bundle,
    package_install_opts = list(type = "binary"),  # faster binary installs on Windows
    run_app_on_start    = TRUE,
    app_root_path       = source_dir
  )

  message("\n========================================")
  message(" Build complete!")
  message(" Installer: ", file.path(build_dir, paste0(app_name, " Setup.exe")))
  message("========================================")
  message("\n To distribute:")
  message("   Share 'Rtivity Setup.exe' with Windows users.")
  message("   They just double-click — no R or Chrome needed.\n")

}, error = function(e) {
  message("\n[ERROR] Build failed: ", e$message)
  message("\nCommon fixes:")
  message("  1. Make sure Node.js is installed: https://nodejs.org")
  message("  2. Re-run 0_install_prerequisites.R to reinstall electricShine")
  message("  3. Check that Rtivity_main exists at: ", source_dir)
  message("  4. Try updating electricShine:")
  message("       remotes::install_github('chasemc/electricShine', build = FALSE)")
})
