#!/usr/bin/env Rscript
options(error = function() { traceback(2); quit(status = 1) })

dir.create("state", showWarnings = FALSE, recursive = TRUE)
dir.create("app_data", showWarnings = FALSE, recursive = TRUE)

ga_token <- Sys.getenv("GA_TOKEN")
if (ga_token == "") stop("GA_TOKEN env var is missing")

marker_file <- file.path("state", "ga_last_seen.rds")

last_seen <- if (file.exists(marker_file)) readRDS(marker_file) else NULL

# ---- TODO: replace with your GA "what's new" request ----
# Example idea:
# new_items <- ga_list_new_items(token = ga_token, since = last_seen)
new_items <- list()  # placeholder

if (length(new_items) == 0) {
  message("No new GA uploads. Exiting.")
  quit(status = 0)
}

message("New items found: ", length(new_items))

# ---- TODO: download delta + rebuild cached artifacts ----
# Example:
# updated_data <- rebuild_app_cache(new_items)
# saveRDS(updated_data, "app_data/some_cache.rds")

# Write marker only after success:
new_last_seen <- Sys.time()
saveRDS(new_last_seen, marker_file)

message("Update complete.")
