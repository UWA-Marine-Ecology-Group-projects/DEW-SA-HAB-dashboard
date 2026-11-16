#!/usr/bin/env Rscript
options(error = function() { traceback(2); quit(status = 1) })

if (!requireNamespace("rsconnect", quietly = TRUE)) install.packages("rsconnect")

# On GitHub Actions the account comes from repository secrets. Locally
# these are usually unset and the account RStudio already knows about
# (marine-ecology) is used instead.
if (all(nzchar(Sys.getenv(c("SHINYAPPS_ACCOUNT", "SHINYAPPS_TOKEN", "SHINYAPPS_SECRET"))))) {
  rsconnect::setAccountInfo(
    name   = Sys.getenv("SHINYAPPS_ACCOUNT"),
    token  = Sys.getenv("SHINYAPPS_TOKEN"),
    secret = Sys.getenv("SHINYAPPS_SECRET"),
    server = "shinyapps.io"
  )
} else {
  message("SHINYAPPS_* not set - using the shinyapps.io account already configured on this computer.")
}

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
#   renv.lock                 package versions - only when renv is active
# ------------------------------------------------------------

app_files <- c("global.R", "server.R", "ui.R")

# renv.lock is only sent when renv is the library in use. rsconnect checks
# the lockfile against the installed packages and refuses to deploy if they
# differ ("Library and lockfile are out of sync"), which is exactly what
# happens when renv is switched off (no .Rprofile) and your normal R library
# is in use. Without the lockfile, rsconnect records the versions of the
# packages actually installed - the ones you have just tested the app with.
if (nzchar(Sys.getenv("RENV_PROJECT")) && file.exists("renv.lock")) {
  app_files <- c(app_files, "renv.lock")
  message("renv is active - package versions taken from renv.lock.")
} else {
  message("renv is not active - package versions taken from the installed library.")
}
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
