#!/usr/bin/env Rscript
options(error = function() { traceback(2); quit(status = 1) })

dir.create("state", recursive = TRUE, showWarnings = FALSE)
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("app_data", recursive = TRUE, showWarnings = FALSE)
dir.create("app_data/spatial", recursive = TRUE, showWarnings = FALSE)

# Fail fast if secrets missing
if (Sys.getenv("API_USER_TOKEN") == "") stop("API_USER_TOKEN is missing")

# If you’re using googlesheets4 in Script 2, enforce it here too:
# (Script 2 can also do this itself; doing it here is fine either way.)
if (Sys.getenv("GSHEETS_SERVICE_JSON") == "") {
  message("GSHEETS_SERVICE_JSON is missing. If Script 2 reads Google Sheets, it will fail.")
}

# 1) Download raw GA data
source("01_Download and format data for app/01_Download BRUVS data from GlobalArchive V1.R")

# 2) Format + build app_data/hab_data.Rdata (and spatial RDS etc.)
source("01_Download and format data for app/03_Format BRUVS and RLS data for summaries and plots.R")

# 3) Write a marker so you can see last successful run
saveRDS(Sys.time(), file = "state/last_successful_update.rds")
message("Update pipeline complete.")
