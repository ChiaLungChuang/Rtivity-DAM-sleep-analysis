# Rtivity — macOS Port for R 4.5+

A maintained fork of [Rtivity](https://github.com/Rilva/Rtivity) (Silva et al., 2022), a Shiny application for analyzing *Drosophila* activity and sleep patterns from Drosophila Activity Monitor (DAM) data. This fork ports the original Windows-only application to macOS, modernizes the UI, enforces UTC timezone safety throughout, and removes deprecated dependencies that no longer install on R 4.5+.

The original Rtivity is no longer actively maintained. This fork keeps the application running on current R releases (4.5+) and adds analysis features useful for *Drosophila* aging and circadian studies.

## What this fork adds

This fork is a drop-in replacement for the original on macOS and Windows, with the following improvements:

### Native macOS support
- macOS `.app` bundle that launches with a double-click — no R or Chrome installation walkthrough required for end users (R must be installed, packages auto-install on first run)
- Build scripts (`desktop_build/1_build_app.R` for Mac, `desktop_build/2_build_windows.R` for Windows) produce distributable bundles

### UI modernization
- **Time-picker redesign**: replaced the original `airDatepickerInput` time fields, which opened a calendar widget anchored to the current system date, with plain `textInput` fields (HH:MM format). No more clicking through a calendar to set an hour.
- **Auto-populate from data**: start and finish date/time fields are initialized from the actual timestamps in the uploaded DAM files, rather than defaulting to `Sys.time()`. Loading a January experiment no longer requires manually navigating away from today's date.

### Timezone correctness
- Full UTC enforcement across every `as.POSIXct`, `strftime`, `format`, and `difftime` call in `Data_selection_server.R` and `Sleep_bout_analysis_standalone.R`
- Fixes a chronogram time-axis offset bug where activity plots started at negative hours on machines outside UTC (e.g., a UTC−5 machine displayed ZT0 as -5)
- Survival analysis arithmetic is timezone-safe (uses numeric seconds throughout)

### Dependency modernization
- **Removed `longitudinalData`** — the package was archived from CRAN and depends on `clv`, which fails to compile against R 4.5+ (uses the removed `R_ext/PrtUtil.h` C API). NA imputation now uses `zoo::na.locf` with bidirectional fill, which is already in the dependency list.
- **Replaced Java-dependent `xlsx` with `openxlsx`** — eliminates the requirement to install a JDK and avoids `rJava` initialization failures on Apple Silicon.
- **`lubridate::Period` compatibility** with R 4.5+ behavior changes

### Analysis improvements
- **Full-recording RLE scan for death detection**: the original method could miss deaths near recording boundaries. The new `detect_dead_animals_rle` runs run-length encoding across the full activity time series, identifies the longest consecutive inactivity period per channel, and flags it as death if it exceeds the user-set threshold (default 12 h).
- **Sleep bout count analysis module** (`Sleep_bout_analysis_standalone.R`): standalone analysis of sleep bout counts per condition, supporting between-group and within-group statistical comparisons.

## Installation

### macOS (end user)
1. Install R from [CRAN](https://cran.r-project.org) (any version ≥ 4.5)
2. Download `Rtivity.app.zip` from the [Releases](../../releases) page
3. Unzip and drag `Rtivity.app` to `/Applications`
4. Double-click to launch. First run takes 1–3 minutes (auto-installs ~35 R packages)

### Windows (end user)
1. Download `Rtivity Setup.exe` from [Releases](../../releases)
2. Double-click to install
3. Launch from Start menu. No R installation required — the installer bundles a portable R runtime.

### Running from source (R / RStudio)

```r
# Install dependencies
source("install_packages.R")

# Launch app
shiny::runApp(".")
```

### Building from source
See [`desktop_build/`](desktop_build/) for instructions on producing `.app` (macOS) and `.exe` (Windows) bundles.

## Usage

After launching:

1. **Data integrity tab** — upload DAM `.txt` files; the app checks for missing values and imputes them using LOCF with bidirectional fill
2. **Data selection tab** — set experiment start/finish times (auto-populated from file timestamps), light onset time, and assign experimental conditions
3. **Activity tab** — total activity, mean activity per phase, light/dark contrast
4. **Sleep tab** — total sleep, sleep latency, sleep architecture
5. **Rhythm & Fractal tab** — periodogram analysis, detrended fluctuation analysis (DFA)
6. **Survival tab** — death detection, survival curves
7. **Visual inspection tab** — actograms per channel

## Citation

If you use Rtivity in published work, please cite **both** the original publication and this fork:

**Original**

> Silva, R.F.O., Pinho, B.R., Monteiro, N.M. et al. (2022). Automated analysis of activity, sleep, and rhythmic behaviour in various animal species with the Rtivity software. *Scientific Reports*, 12, 4179. https://doi.org/10.1038/s41598-022-08195-z

**This fork**

> Chuang, C.-L. (2026). *Rtivity — macOS port for R 4.5+* (Version 1.2.0). Demontis Lab, St. Jude Children's Research Hospital. https://github.com/ChiaLungChuang/Rtivity-DAM-sleep-analysis

## License

This repository is a derivative work of [Rtivity](https://github.com/Rilva/Rtivity), licensed under [Creative Commons Attribution 4.0 International (CC BY 4.0)](https://creativecommons.org/licenses/by/4.0/). All modifications in this fork are released under the same license. Original authorship is credited above and preserved in commit history.

## Acknowledgments

Original Rtivity authored by Silva et al. (2022). Modifications and macOS port by [Chia-Lung Chuang](https://github.com/ChiaLungChuang), Department of Developmental Neurobiology, St. Jude Children's Research Hospital (Demontis Lab).
