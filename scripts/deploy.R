#!/usr/bin/env Rscript
options(error = function() { traceback(2); quit(status = 1) })

if (!requireNamespace("rsconnect", quietly = TRUE)) install.packages("rsconnect")

rsconnect::setAccountInfo(
  name   = Sys.getenv("SHINYAPPS_ACCOUNT"),
  token  = Sys.getenv("SHINYAPPS_TOKEN"),
  secret = Sys.getenv("SHINYAPPS_SECRET"),
  server = "shinyapps.io"
)

rsconnect::deployApp(
  appDir = ".",
  appName = "DEW-SA-HAB-dashboard",
  forceUpdate = TRUE
)
