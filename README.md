# Rtivity — macOS Port for R 4.5+

A modified fork of [Rtivity](https://github.com/Rilva/Rtivity), porting the original Windows-only *Drosophila* activity and sleep analysis Shiny app to run natively on macOS with R 4.5.1. Four server-side files were modified; all other files are unchanged from the original.

> **License:** This repository is a derivative work of Rtivity, which is licensed under [Creative Commons Attribution 4.0 International (CC BY 4.0)](https://creativecommons.org/licenses/by/4.0/). Changes are indicated below. Original authors are credited in the Attribution section.

---

## Original Software

**Rtivity** — Automated analysis of activity, sleep, and rhythmic behaviour in various animal species.

Silva R.F.O., Pinho B.R., Monteiro N.M., Santos M.M. & Oliveira J.M.A.
*Scientific Reports* 12, 4179 (2022). https://doi.org/10.1038/s41598-022-08195-z

Original repository: https://github.com/Rilva/Rtivity

---

## Motivation

The original Rtivity distribution provides a Windows `.exe` installer bundling a portable R environment. Running the source code on macOS with R 4.5.1 required resolving several compatibility issues: a breaking change in lubridate's handling of `Period` objects in R 4.5+, a Java dependency that is broken on modern macOS, an improved death detection algorithm, and a missing sleep bout count analysis module needed for the lab's *Drosophila* experiments.

---

## Files Modified

Only the following four files were changed. All other Rtivity source files are unmodified.

| File | Changes |
|---|---|
| `Sleep_server.R` | lubridate fix; new sleep bout count module |
| `Activity_server.R` | NaN → 0 handling for activity bout parameters |
| `Survival_analysis_server.R` | RLE-based death detection replacing `curate_dead_animals()` |
| `Data_selection_server.R` | Multi-file timestamp alignment fix |

---

## Modifications in Detail

### 1. R 4.5 / lubridate Compatibility Fix (`Sleep_server.R`)

In R 4.5+, lubridate `Period` objects can no longer be used directly in arithmetic with numeric timestamps. All time calculations were refactored to use explicit numeric seconds:

```r
# Original (Windows — breaks in R 4.5+)
Time <- periodicTime - hours(l_period()) * k - threshold_hours

# Fixed (macOS R 4.5+)
period_secs     <- as.numeric(hours(l_period()),  unit = "secs")
threshold_hours <- as.numeric(hours(l_hours()),   unit = "secs")
Time <- periodicTime - period_secs * k - threshold_hours
```

### 2. Package Migration: `xlsx` → `openxlsx` (`Sleep_server.R`, `Activity_server.R`)

The `xlsx` package requires a Java runtime and is broken on modern macOS. All Excel export functionality was migrated to `openxlsx`, which is Java-free and works natively on Mac.

### 3. RLE-Based Death Detection (`Survival_analysis_server.R`)

The original used `curate_dead_animals()` from the `sleepr` package, which only checks the **last N hours** of recording. This was replaced with a custom **Run-Length Encoding (RLE)** algorithm that scans the **entire recording** for the longest continuous zero-activity period:

```r
detect_dead_animals_rle <- function(dt, death_inactivity_hours = 12) {
  # Scans the full recording using RLE
  # Animal is marked dead if the longest zero-activity run
  # meets or exceeds the threshold
}
```

This is more robust for mid-experiment deaths: an animal that dies at day 5 of a 14-day experiment will be correctly identified, whereas the original backward-scan approach may miss it if any activity occurs after the zero run.

### 4. New Module: Sleep Bout Count Analysis (`Sleep_server.R`)

The Windows version does not include sleep bout count analysis. A new `boutSleepCountSummary` module was added, supporting all existing view modes: light/dark phase, per day, per day + light phase, and custom time windows.

### 5. NaN / Zero Handling for Statistical Parameters (`Activity_server.R`, `Sleep_server.R`)

Carefully matched to biological interpretation for each parameter:

| Parameter | Behavior | Rationale |
|---|---|---|
| WASO | Keep 0 (include in N) | WASO = 0 means the animal never woke after sleep onset — valid observation |
| Sleep Latency | Keep NaN (exclude from N) | NaN means no sleep detected — not a valid latency measurement |
| Activity Bouts | Convert NaN → 0 (include in N) | No bouts = 0 bouts, a meaningful count |
| Activity Duration | Convert NaN → 0 (include in N) | Same rationale as above |

### 6. Multi-File Timestamp Alignment (`Data_selection_server.R`)

When loading multiple DAM monitor files, timestamps were misaligned due to residual sub-minute seconds. Fixed by truncating all timestamps to the nearest minute before alignment.

---

## Usage

To use these files, clone or download the full original Rtivity repository and replace the four modified files with those from this repository:

```bash
git clone https://github.com/Rilva/Rtivity
# then replace:
# Sleep_server.R
# Activity_server.R
# Survival_analysis_server.R
# Data_selection_server.R
# with the versions from this repository
```

Then open `app.R` (or the main Shiny entry point) in RStudio and run as normal.

**R version tested:** R 4.5.1 on macOS.

**Additional package required:** `openxlsx` (replaces `xlsx`). Install with:

```r
install.packages("openxlsx")
```

All other dependencies are the same as the original Rtivity installation.

---

## Attribution

This work is a derivative of Rtivity by Silva et al. (2022), licensed under CC BY 4.0.

**Original authors:**
Rui F. O. Silva, Brígida R. Pinho, Nuno M. Monteiro, Miguel M. Santos & Jorge M. A. Oliveira
University of Porto, Portugal

**Original publication:**
Silva R.F.O. et al. "Automated analysis of activity, sleep, and rhythmic behaviour in various animal species with the Rtivity software." *Scientific Reports* 12, 4179 (2022).
https://doi.org/10.1038/s41598-022-08195-z

**Original repository:** https://github.com/Rilva/Rtivity

**Changes made:** See "Modifications in Detail" section above.

---

## Fork Author

**Chia-Lung Chuang**
Postdoctoral Research Associate
Department of Developmental Neurobiology
St. Jude Children's Research Hospital
Memphis, TN
