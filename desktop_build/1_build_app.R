# ============================================================
# STEP 1 — Build Rtivity.app for macOS
# Mirrors the Windows build: app bundle + launcher + Shiny files
# ============================================================

source_dir <- path.expand("~/Desktop/Rtivity_main")
build_dir  <- path.expand("~/Desktop/Rtivity_main/desktop_build/build")
app_dir    <- file.path(build_dir, "Rtivity.app")

# Clean and create structure
unlink(app_dir, recursive = TRUE)
dirs <- c(
  file.path(app_dir, "Contents/MacOS"),
  file.path(app_dir, "Contents/Resources/Shiny"),
  file.path(app_dir, "Contents/Resources")
)
for (d in dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# ── 1. Copy Shiny app files ────────────────────────────────
r_files <- list.files(source_dir, pattern = "\\.(R|xlsx|txt)$",
                      full.names = TRUE, ignore.case = TRUE)
file.copy(r_files,
          file.path(app_dir, "Contents/Resources/Shiny"),
          overwrite = TRUE)
message("Copied ", length(r_files), " app files")

# ── 2. Write launcher.R (mirrors runShinyApp.R on Windows) ─
writeLines(
  c(
    '# Rtivity launcher — run by the shell script',
    'options(repos = c(CRAN = "https://cloud.r-project.org"))',
    'args <- commandArgs(trailingOnly = TRUE)',
    'shiny_dir <- if (length(args) > 0) args[1] else "."',
    '',
    '# Install missing packages on first run',
    'pkgs <- c("shiny","shinyFiles","shinyTime","shinyWidgets","behavr",',
    '          "data.table","ggplot2","damr","zeitgebr","sleepr","ggetho",',
    '          "DT","fs","dplyr","Hmisc","shinyjs","readr","colourpicker",',
    '          "lattice","survival","Formula","stringr","DescTools",',
    '          "graphics","doBy","zoo","tm","base","scales","shinythemes",',
    '          "openxlsx","ActCR","tools",',
    '          "nonlinearTseries","shinytitle","tidyverse")',
    'missing <- pkgs[!pkgs %in% installed.packages()[,"Package"]]',
    'if (length(missing) > 0) {',
    '  message("Installing missing packages: ", paste(missing, collapse=", "))',
    '  install.packages(missing, dependencies = TRUE,',
    '                   repos = "https://cloud.r-project.org")',
    '}',
    '',
    'setwd(shiny_dir)',
    'options(shiny.port = 9999, shiny.launch.browser = FALSE)',
    'shiny::runApp(shiny_dir, port = 9999, launch.browser = FALSE)'
  ),
  file.path(app_dir, "Contents/Resources/launcher.R")
)

# ── 3. Write shell launcher (Contents/MacOS/Rtivity) ───────
launcher <- c(
  '#!/bin/bash',
  'DIR="$(cd "$(dirname "$0")" && pwd)"',
  'SHINY_DIR="$DIR/../Resources/Shiny"',
  'R_SCRIPT="$DIR/../Resources/launcher.R"',
  'LOG="$DIR/../Resources/rtivity.log"',
  '',
  '# Find R (checks common CRAN install locations)',
  'R_BIN=""',
  'for r_path in \\',
  '  "/opt/R/arm64/bin/R" \\',
  '  "/opt/R/x86_64/bin/R" \\',
  '  "/usr/local/bin/R" \\',
  '  "/Library/Frameworks/R.framework/Resources/bin/R"; do',
  '  if [ -f "$r_path" ]; then R_BIN="$r_path"; break; fi',
  'done',
  '',
  'if [ -z "$R_BIN" ]; then',
  '  osascript -e \'display dialog "R is not installed.\\n\\nPlease download R from:\\nhttps://cran.r-project.org\\n\\nThen re-open Rtivity." buttons {"OK"} default button "OK" with icon stop with title "Rtivity — R Not Found"\'',
  '  exit 1',
  'fi',
  '',
  '# Start Shiny in background, log output',
  '"$R_BIN" --vanilla -f "$R_SCRIPT" --args "$SHINY_DIR" > "$LOG" 2>&1 &',
  'R_PID=$!',
  '',
  '# Wait for Shiny to be ready (up to 60s)',
  'for i in $(seq 1 180); do',
  '  sleep 1',
  '  if curl -s http://127.0.0.1:9999 > /dev/null 2>&1; then break; fi',
  'done',
  '',
  '# Open in Chrome app mode (no browser chrome), fallback to default browser',
  'CHROME="/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"',
  'if [ -f "$CHROME" ]; then',
  '  "$CHROME" --app=http://127.0.0.1:9999 --window-size=1400,900',
  'else',
  '  open http://127.0.0.1:9999',
  'fi',
  '',
  '# When browser closes, kill R',
  'wait',
  'kill $R_PID 2>/dev/null'
)
launcher_path <- file.path(app_dir, "Contents/MacOS/Rtivity")
writeLines(launcher, launcher_path)
Sys.chmod(launcher_path, mode = "0755")

# ── 4. Write Info.plist ────────────────────────────────────
writeLines(
  c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"',
    '  "http://www.apple.com/DTDs/PropertyList-1.0.dtd">',
    '<plist version="1.0">',
    '<dict>',
    '  <key>CFBundleName</key>             <string>Rtivity</string>',
    '  <key>CFBundleDisplayName</key>      <string>Rtivity</string>',
    '  <key>CFBundleIdentifier</key>       <string>com.rtivity.app</string>',
    '  <key>CFBundleVersion</key>          <string>1.2.0</string>',
    '  <key>CFBundleShortVersionString</key><string>1.2.0</string>',
    '  <key>CFBundleExecutable</key>       <string>Rtivity</string>',
    '  <key>CFBundlePackageType</key>      <string>APPL</string>',
    '  <key>CFBundleSignature</key>        <string>????</string>',
    '  <key>LSMinimumSystemVersion</key>   <string>11.0</string>',
    '  <key>NSHighResolutionCapable</key>  <true/>',
    '</dict>',
    '</plist>'
  ),
  file.path(app_dir, "Contents/Info.plist")
)

message("
========================================
 Build complete!
 Output: ", app_dir, "
========================================
 To distribute:
   Right-click Rtivity.app → Compress
   Share the .zip file

 Requirements for end users:
   - macOS 11+
   - R installed (https://cran.r-project.org)
   - Packages auto-install on first launch
========================================
")
