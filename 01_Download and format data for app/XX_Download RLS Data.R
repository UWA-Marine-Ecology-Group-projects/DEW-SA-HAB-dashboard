library(readr)
library(purrr)
library(tibble)

# Create the output folder if it does not already exist
download_links <- tribble(
  ~filename,                         ~url,
  "ep_m1_sa.csv",                    "https://nrmn-prod-shared.s3.ap-southeast-2.amazonaws.com/endpoints/KGZy6iDHuFPLrXugfMcj/ep_m1_sa.csv",
  "ep_m2_inverts_sa.csv",            "https://nrmn-prod-shared.s3.ap-southeast-2.amazonaws.com/endpoints/rJ2RjvqxbV9zdv2C7Kjx/ep_m2_inverts_sa.csv",
  "ep_m2_cryptic_fish_sa.csv",       "https://nrmn-prod-shared.s3.ap-southeast-2.amazonaws.com/endpoints/NEjM7idp5B7adNFt5Jti/ep_m2_cryptic_fish_sa.csv",
  "ep_survey_list.csv",              "https://nrmn-prod-shared.s3.ap-southeast-2.amazonaws.com/endpoints/wOFiosz6AqYVBcl2pc8n/ep_survey_list.csv",
  "ep_site_list.csv",                "https://nrmn-prod-shared.s3.ap-southeast-2.amazonaws.com/endpoints/7Wf7JMXA0bzWIBVqdEeX/ep_site_list.csv",
  "ep_m0_off_transect_sighting.csv", "https://nrmn-prod-shared.s3.ap-southeast-2.amazonaws.com/endpoints/OJxNLnZWpyBWBx7vnKCU/ep_m0_off_transect_sighting.csv"
)

walk2(
  download_links$url,
  download_links$filename,
  \(url, filename) {
    destination <- file.path("data/raw/RLS", filename)
    
    message("Downloading ", filename, "...")
    
    download.file(
      url = url,
      destfile = destination,
      mode = "wb",
      quiet = FALSE
    )
  }
)

message("All files downloaded.")