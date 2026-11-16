#!/usr/bin/env Rscript
options(error = function() { traceback(2); quit(status = 1) })

if (!requireNamespace("rsconnect", quietly = TRUE)) install.packages("rsconnect")

rsconnect::setAccountInfo(
  name   = Sys.getenv("SHINYAPPS_ACCOUNT"),
  token  = Sys.getenv("SHINYAPPS_TOKEN"),
  secret = Sys.getenv("SHINYAPPS_SECRET"),
  server = "shinyapps.io"
)

# ------------------------------------------------------------
# Exactly what the running app needs - nothing else is uploaded.
#
# Everything else in the repo (data-prep and modelling scripts, plots,
# outputs, model results, raw data, secrets) stays out of the bundle.
# If the app starts reading a new file or folder, add it here.
#
#   global.R, server.R, ui.R  the app
#   R/                        helper functions sourced by global.R
#   www/                      logos, icons, favicon
#   app_data/                 hab_data, rls_data, bruv multivariate, spatial
#   data/lookups/             Dive summary text, read live by server.R
#   renv.lock                 package versions
# ------------------------------------------------------------

app_files <- c("global.R", "server.R", "ui.R", "renv.lock")
app_dirs  <- c("R", "www", "app_data", file.path("data", "lookups"))

missing_files <- app_files[!file.exists(app_files)]
if (length(missing_files) > 0) {
  stop("Missing required app file(s): ", paste(missing_files, collapse = ", "))
}

# data/lookups is optional (the app runs without it, just with no Dive
# summary text), so warn rather than fail if it isn't there.
if (!dir.exists(file.path("data", "lookups"))) {
  warning("data/lookups not found - the Dive summary text will be missing from the deployed app.")
}

# Expand folders into their files so this doesn't depend on how a given
# rsconnect version treats directories in appFiles.
dir_files <- unlist(lapply(
  app_dirs[dir.exists(app_dirs)],
  function(d) list.files(d, recursive = TRUE, full.names = TRUE, all.files = FALSE)
))

deploy_files <- c(app_files, dir_files)

message("Deploying ", length(deploy_files), " files:")
message(paste(" -", deploy_files, collapse = "\n"))

rsconnect::deployApp(
  appDir      = ".",
  appFiles    = deploy_files,
  appName     = "SA-HAB",
  forceUpdate = TRUE
)
