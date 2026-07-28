# MAKARA VALIDATOR----
# remotes::install_github("jeffwalkernoaa/makaraValidatr")
library(makaraValidatr)

# simple validation with only the sheets you have locally stored, does not use data already submitted to makara
validate_submission("Makara/MakaraSubmission12022025/20260706_corrections/SWFSC-relation-errors/")

# RELATIONS VALIDATION WITH BIGQUERY, uses existing makara data 
# STEP 0: install {bigrquery}
# install.packages("bigrquery")

# STEP 1: Authenticate BigQuery
# Replace the email field with your email address
# This should open a web browser and ask you to authorize access to your Google account
bigrquery::bq_auth(
  email = "kourtney.burger@noaa.gov", 
  scopes = c(
    "https://www.googleapis.com/auth/userinfo.email", 
    "https://www.googleapis.com/auth/bigquery", 
    "https://www.googleapis.com/auth/cloud-platform"
  )
)

# STEP 2: Set Environmental Variables
# makaraValidatr uses the `BQ_PROJECT` and `BQ_DATASET` variables to connect to the Makara dataset in BigQuery
Sys.setenv(BQ_PROJECT = "ggn-nmfs-pacm-dev-1", BQ_DATASET = "makara")

# STEP 3: Run Validation with Relations
# Run validate_submission() as before but with the relations=TRUE argument
validate_submission("Makara/MakaraSubmission12022025/20260706_corrections/SWFSC-relation-errors/", relations = TRUE)
# The output should contain a "Validating Relations" section at the end, confirming it was able to access BigQuery