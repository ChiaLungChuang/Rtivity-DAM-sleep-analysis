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
  'CHROME="/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"',
  'PROFILE="$DIR/../Resources/chrome-profile"',
  'URL="http://127.0.0.1:9999"',
  '',
  '# Helper: open the app window (dedicated profile so we never collide with',
  '# the user\'s normal Chrome; flags suppress first-run and the "did not shut',
  '# down correctly" restore bar that appears after we force-close the window).',
  'open_window() {',
  '  if [ -f "$CHROME" ]; then',
  '    "$CHROME" --user-data-dir="$PROFILE" \\',
  '      --no-first-run --no-default-browser-check --disable-session-crashed-bubble \\',
  '      --app="$URL" --window-size=1400,900 >/dev/null 2>&1 &',
  '  else',
  '    open "$URL"',
  '  fi',
  '}',
  '',
  '# If Rtivity is already running (e.g. the user double-clicked), just open',
  '# another window against the existing server instead of starting a second',
  '# one or killing the healthy session.',
  'if curl -s "$URL" > /dev/null 2>&1; then',
  '  open_window',
  '  exit 0',
  'fi',
  '',
  '# Clear any stale/dead listener left by a previous run that did not shut down',
  '# cleanly (crash, sleep, force-quit) so it cannot block this launch.',
  'lsof -ti :9999 2>/dev/null | xargs kill -9 2>/dev/null',
  '',
  '# Immediate feedback: the first launch of the day can take 30-60s while R',
  '# loads its packages before the window appears, so the user does not think',
  '# it hung and click again.',
  'osascript -e \'display notification "Starting up - the window will open in a moment." with title "Rtivity"\' >/dev/null 2>&1 &',
  '',
  '# Start Shiny in background, log output',
  '"$R_BIN" --vanilla -f "$R_SCRIPT" --args "$SHINY_DIR" > "$LOG" 2>&1 &',
  'R_PID=$!',
  '',
  '# Wait for Shiny to be ready (up to 180s), but stop early if R already died',
  'READY=0',
  'for i in $(seq 1 180); do',
  '  sleep 1',
  '  if curl -s "$URL" > /dev/null 2>&1; then READY=1; break; fi',
  '  if ! kill -0 "$R_PID" 2>/dev/null; then break; fi',
  'done',
  '',
  'if [ "$READY" != "1" ]; then',
  '  kill "$R_PID" 2>/dev/null',
  '  osascript -e \'display dialog "Rtivity failed to start.\\n\\nCheck the log for details:\\n\'"$LOG"\'" buttons {"OK"} default button "OK" with icon stop with title "Rtivity — Failed to Start"\'',
  '  exit 1',
  'fi',
  '',
  '# Tie the window and the R server together so that when EITHER one ends, the',
  '# other is shut down too. This kills two failure modes:',
  '#   * window closed -> R server would otherwise leak and hold the port',
  '#   * R dies first (sleep/crash) -> window would otherwise be left as a dead,',
  '#     empty "can\'t be reached" window with no backend',
  'if [ -f "$CHROME" ]; then',
  '  # Watchdog: if the R server dies, close the window so the user never ends',
  '  # up staring at a dead, empty window.',
  '  ( while kill -0 "$R_PID" 2>/dev/null; do sleep 1; done',
  '    pkill -f "Resources/chrome-profile" 2>/dev/null ) &',
  '  WATCHDOG=$!',
  '',
  '  # Open the window in the FOREGROUND: with our dedicated profile there is no',
  '  # hand-off to an existing Chrome, so this blocks until the user closes the',
  '  # window and then returns immediately (prompt, no polling lag).',
  '  "$CHROME" --user-data-dir="$PROFILE" \\',
  '    --no-first-run --no-default-browser-check --disable-session-crashed-bubble \\',
  '    --app="$URL" --window-size=1400,900 >/dev/null 2>&1',
  '',
  '  # Window closed (or watchdog closed it) -> tear everything down.',
  '  kill "$WATCHDOG" 2>/dev/null',
  '  pkill -f "Resources/chrome-profile" 2>/dev/null',
  '  kill "$R_PID" 2>/dev/null',
  '  lsof -ti :9999 2>/dev/null | xargs kill -9 2>/dev/null',
  'else',
  '  open "$URL"',
  '  # No reliable way to detect when the default browser tab closes;',
  '  # keep Shiny running until this process is stopped some other way.',
  '  wait "$R_PID"',
  'fi'
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
    '  <key>CFBundleVersion</key>          <string>1.3.0</string>',
    '  <key>CFBundleShortVersionString</key><string>1.3.0</string>',
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
