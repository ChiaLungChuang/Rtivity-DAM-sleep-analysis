# Changelog

All notable changes to this fork of Rtivity are documented here.
This project adheres to [Semantic Versioning](https://semver.org/) (MAJOR.MINOR.PATCH).

## [1.3.0] — 2026-06-25

Reliability release for the standalone **macOS desktop app** (`Rtivity.app`).
No changes to the analysis features — this release makes the desktop launcher
robust so the app opens and closes cleanly every time.

### Fixed
- **"This site can't be reached" on launch.** Closing the app window now reliably
  stops the underlying R/Shiny server. Previously the server could be left running
  and holding the port, so the next launch failed to start and the window showed
  `ERR_CONNECTION_REFUSED`.
- **Empty / dead window.** If the R server stopped first (for example after the Mac
  went to sleep, or on an error), the window used to be left on screen with nothing
  behind it. The window and the server are now tied together — when either one ends,
  the other shuts down too, so you never end up staring at a blank window.
- **"Failed to Start" when the app was kept on the Desktop.** The launcher used to
  write its log and browser-profile data *inside the app bundle*. macOS blocks apps
  from writing into protected folders (`~/Desktop`, `~/Documents`, `~/Downloads`),
  so a double-click of an app sitting on the Desktop was denied that write, R never
  started, and the "Failed to Start" dialog appeared (it only worked when launched
  from a terminal). Runtime data now lives in `~/Library/Application Support/Rtivity/`,
  which is always writable, so the app launches from anywhere — Desktop, Downloads,
  or Applications — and the app bundle no longer bloats with a browser profile.
  *(macOS download re-published 2026-07-11 with this fix; still version 1.3.0.)*

### Added
- **Startup notification.** The first launch of the day can take ~30–60 s while R
  loads its packages. A "Starting up…" notice now appears immediately so it's clear
  the app is working and hasn't hung.
- **Stale-session cleanup.** Each launch clears any leftover server from a previous
  run that didn't shut down cleanly (crash, sleep, force-quit), so it can't block a
  fresh start.
- **"Already running" guard.** Launching Rtivity while it's already open now simply
  opens another window against the running server instead of starting a second copy
  or disturbing the current session.
- **Failure dialog.** If the app genuinely can't start, it now shows a clear dialog
  pointing to the log file instead of opening a broken window.

### Changed
- The app window now uses a dedicated, isolated browser profile, so it opens
  reliably even when Google Chrome is already running, and Chrome's "didn't shut
  down correctly" restore bar no longer appears.
- Version bumped to 1.3.0 across the in-app home screen, README, and both the macOS
  and Windows build scripts.

### Known limitation
- Because the macOS app is launched by a shell script, clicking the dock icon while
  Rtivity is already running may show macOS's "not responding" label. This is
  cosmetic — use the window that's already open, or close it and relaunch.

## [1.2.x] — early–mid 2026

Earlier work on this fork (see git history for details). Highlights:

- macOS port targeting R 4.5+.
- May 2026 time-picker fixes and a timezone-handling audit.
- Removal of the legacy `longitudinalData` dependency; imputation switched to
  `zoo::na.locf`.
- Added the `desktop_build/` scripts that produce the standalone macOS app and the
  Windows installer. The last installer distributed from this line was 1.2.4.

---

Original Rtivity by Silva et al. (2022), *Scientific Reports* 12, 4179.
This fork: Chuang, C.-L. (2026), Demontis Lab, St. Jude Children's Research Hospital.
