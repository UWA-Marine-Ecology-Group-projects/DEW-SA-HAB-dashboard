#################################################################
# Download SA RLS data from amazon ----

# Load libraries needed -----
library(readr)
library(purrr)
library(tibble)

# Make sure destination folder exists (is gitignored so will need to run on each comp once)
dir.create(
  "data/raw/RLS",
  recursive = TRUE,
  showWarnings = FALSE
)

# Links for downloading ----
download_links <- tribble(
  ~filename,                         ~url,
  "ep_m1_sa.csv",                    "https://nrmn-prod-shared.s3.ap-southeast-2.amazonaws.com/endpoints/KGZy6iDHuFPLrXugfMcj/ep_m1_sa.csv",
  "ep_m2_inverts_sa.csv",            "https://nrmn-prod-shared.s3.ap-southeast-2.amazonaws.com/endpoints/rJ2RjvqxbV9zdv2C7Kjx/ep_m2_inverts_sa.csv",
  "ep_m2_cryptic_fish_sa.csv",       "https://nrmn-prod-shared.s3.ap-southeast-2.amazonaws.com/endpoints/NEjM7idp5B7adNFt5Jti/ep_m2_cryptic_fish_sa.csv",
  "ep_survey_list.csv",              "https://nrmn-prod-shared.s3.ap-southeast-2.amazonaws.com/endpoints/wOFiosz6AqYVBcl2pc8n/ep_survey_list.csv",
  "ep_site_list.csv",                "https://nrmn-prod-shared.s3.ap-southeast-2.amazonaws.com/endpoints/7Wf7JMXA0bzWIBVqdEeX/ep_site_list.csv",
  "ep_m0_off_transect_sighting.csv", "https://nrmn-prod-shared.s3.ap-southeast-2.amazonaws.com/endpoints/OJxNLnZWpyBWBx7vnKCU/ep_m0_off_transect_sighting.csv"
)

# Download files -----
walk2(
  download_links$url,
  download_links$filename,
  \(url, filename) {
    destination <- file.path("data/raw/RLS", filename) # Folder path
    
    message("Downloading ", filename, "...")
    
    download.file(
      url = url,
      destfile = destination,
      mode = "wb",
      quiet = FALSE
    )
  }
)


# 
# 
# 
# # Download files -----
# 
# download_rls_file <- function(url, filename, max_attempts = 5) {
# 
#   destination <- file.path("data/raw/RLS", filename)
# 
#   # Allow longer for connection + download
#   h <- curl::new_handle(
#     connecttimeout = 60,  # allow 60 sec to establish connection
#     timeout = 300         # allow 5 min for whole download
#   )
# 
#   for (attempt in seq_len(max_attempts)) {
# 
#     message(
#       "Downloading ", filename,
#       " (attempt ", attempt, "/", max_attempts, ")..."
#     )
# 
#     success <- tryCatch({
# 
#       curl::curl_download(
#         url = url,
#         destfile = destination,
#         quiet = FALSE,
#         handle = h
#       )
# 
#       TRUE
# 
#     }, error = function(e) {
# 
#       message("Download failed: ", conditionMessage(e))
#       FALSE
#     })
# 
#     if (success) {
#       message("Downloaded ", filename)
#       return(invisible(destination))
#     }
# 
#     # Wait a little before trying again
#     Sys.sleep(5)
#   }
# 
#   warning("Could not download: ", filename)
# }
# 
# 
# walk2(
#   download_links$url,
#   download_links$filename,
#   download_rls_file
# )
