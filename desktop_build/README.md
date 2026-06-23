# Rtivity Desktop Build Scripts

Builds standalone, distributable Rtivity desktop applications for macOS and Windows. End users need no R, no Chrome, no technical knowledge — just double-click.

## Folder structure

- `0_install_prerequisites.R` — run once to install build dependencies
- `1_build_app.R` — Mac build, run on a Mac
- `2_build_windows.R` — Windows build, run on a Windows PC
- `build/` — output (gitignored): `Rtivity.app` for Mac, `windows/Rtivity Setup.exe` for Windows

## Platform guide

| Platform | Script | Output | Requirements |
|---|---|---|---|
| Mac | `1_build_app.R` | `Rtivity.app` | R + Node.js on Mac |
| Windows | `2_build_windows.R` | `Rtivity Setup.exe` | R + Node.js on Windows PC |

Build on Mac produces the Mac app. Build on Windows produces the Windows installer. Cross-compilation is not supported.

## Mac build steps

1. Install Node.js: `brew install node`
2. Run Step 0 once: `Rscript ~/Desktop/Rtivity_main/desktop_build/0_install_prerequisites.R`
3. Run Step 1: `Rscript ~/Desktop/Rtivity_main/desktop_build/1_build_app.R`
4. Output appears at `desktop_build/build/Rtivity.app`. Zip and share.

## Windows build steps

Do these on a Windows PC.

1. Install R from https://cran.r-project.org
2. Install Node.js (LTS) from https://nodejs.org
3. Copy the entire `Rtivity_main` folder to the Windows PC.
4. In RStudio, source `0_install_prerequisites.R` once.
5. Source `2_build_windows.R`.
6. Output appears at `desktop_build/build/windows/Rtivity Setup.exe`. Share this single file.

## Notes

- The Windows installer bundles a portable R runtime and Chromium (~300–500 MB).
- Windows end users just run `Rtivity Setup.exe`. Nothing else needed.
- The source app in the parent `Rtivity_main/` folder is never modified by the build scripts.