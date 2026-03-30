# Run Package Check ----
# Quick script to verify all changes pass R CMD check.
# Usage: source("analysis/run_check.R")

# Setup ----
devtools::document()

# Check ----
devtools::check()
