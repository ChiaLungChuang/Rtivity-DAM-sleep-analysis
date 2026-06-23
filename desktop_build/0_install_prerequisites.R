# ============================================================
# STEP 0 — Install prerequisites for Rtivity desktop packaging
# Run this script ONCE before anything else
# ============================================================

# 1. Install electricShine from GitHub
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
if (!requireNamespace("electricShine", quietly = TRUE)) {
  remotes::install_github("chasemc/electricShine", build = FALSE)
}

# 2. Check Node.js (must be installed separately — see README)
node_check <- system("node --version", intern = TRUE, ignore.stderr = TRUE)
if (length(node_check) == 0 || grepl("not found", node_check)) {
  message("
  -------------------------------------------------------
  Node.js NOT found. Please install it before continuing:
    Mac:     brew install node
    Windows: https://nodejs.org  (LTS version)
  Then re-run this script.
  -------------------------------------------------------
  ")
} else {
  message("Node.js found: ", node_check)
  message("All prerequisites met. Proceed to 1_build_app.R")
}
