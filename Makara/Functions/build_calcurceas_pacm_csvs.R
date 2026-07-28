##Updated July 28 2026
# ==============================================================================
# Build CalCurCEAS PACM detections.csv and analyses.csv
# ==============================================================================
#
# Purpose
# -------
# Convert the final, manually reviewed hourly presence/absence table into the
# two PACM files used for the CalCurCEAS baleen-whale submission:
#
#   1. CalCurCEAS_Baleen_Whale_PACM_detections.csv
#   2. CalCurCEAS_Baleen_Whale_PACM_analyses.csv
#
# Expected hourly input columns
# -----------------------------
#   deployment   Example: CalCurCEAS_001
#   hour_start   UTC start of the reviewed hour
#   hour_end     UTC exclusive end of the reviewed hour
#   call_type    Fin 20 Hz, Fin 40, Blue A, Blue B, Blue D, or Bb
#   presence     1/0, TRUE/FALSE, or DETECTED/NOT_DETECTED
#
# Output behavior
# ---------------
# detections.csv:
#   One row per deployment x reviewed hour x call type. Presence is converted
#   to DETECTED or NOT_DETECTED.
#
# analyses.csv:
#   One row per deployment x reviewed hour x species when at least one call
#   type for that species was DETECTED. This reproduces the structure of the
#   accepted CalCurCEAS_Baleen_Whale_PACM_analyses.csv file.
#
# Run from the repository root
# ----------------------------
#   Rscript scripts/build_calcurceas_pacm_csvs.R
#
# Or provide an input file and output directory:
#   Rscript scripts/build_calcurceas_pacm_csvs.R path/to/hourly.csv output
#
# Recommended repository layout
# -----------------------------
#   data/All_Drifts_Hourly_Presence_Absence_by_Call_Type.csv
#   scripts/build_calcurceas_pacm_csvs.R
#   output/
#
# ============================================================================== 


# ==============================================================================
# 1. PACKAGES
# ==============================================================================

required_packages <- c(
  "dplyr",
  "tidyr",
  "readr",
  "stringr",
  "lubridate",
  "tibble"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Install the required package(s) before running this script: ",
    paste(missing_packages, collapse = ", "),
    "\nRun: install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(tibble)
})


# ==============================================================================
# 2. USER SETTINGS / COMMAND-LINE ARGUMENTS
# ==============================================================================

args <- commandArgs(trailingOnly = TRUE)

hourly_file <- if (length(args) >= 1) {
  args[[1]]
} else {
  file.path(
    "data",
    "All_Drifts_Hourly_Presence_Absence_by_Call_Type.csv"
  )
}

output_dir <- if (length(args) >= 2) {
  args[[2]]
} else {
  "output"
}

detections_output_file <- file.path(
  output_dir,
  "CalCurCEAS_Baleen_Whale_PACM_detections.csv"
)

analyses_output_file <- file.path(
  output_dir,
  "CalCurCEAS_Baleen_Whale_PACM_analyses.csv"
)

# PACM analysis metadata used in the accepted CalCurCEAS submission.
organization_code_value <- "SWFSC"
recording_code_value <- "SOUNDTRAP_RECORDING"
analysis_sample_rate_khz_value <- 384
analysis_min_frequency_khz_value <- 0
analysis_max_frequency_khz_value <- 0.4
analysis_processing_code_value <- "POST_PROCESSED"
analysis_quality_code_value <- "FULLY_VALIDATED"
analysis_protocol_reference_value <- "Unpublished"
detector_code_value <- "DEEP_ACOUSTICS"

# hour_end in the source data is exclusive (for example, 20:00:00).
# PACM output uses an inclusive endpoint (for example, 19:59:59).
use_inclusive_hour_end <- TRUE

# Stop rather than silently creating absences when an hourly call-type row is
# missing. The final reviewed table should have all six target calls per hour.
require_all_six_call_types_per_hour <- TRUE


# ==============================================================================
# 3. PACM COLUMN SCHEMAS
# ==============================================================================
# These vectors reproduce the column names and order in the uploaded PACM files.
# Optional PACM fields not populated by this workflow are written as blank cells.

pacm_detection_columns <- c(
  "organization_code",
  "deployment_code",
  "analysis_code",
  "detection_start_datetime",
  "detection_end_datetime",
  "detection_effort_secs",
  "detection_sound_source_code",
  "detection_call_type_code",
  "detection_behavior_code",
  "detection_demographic_code",
  "detection_n_validated",
  "detection_n_total",
  "detection_result_code",
  "detection_event_type",
  "detection_event_id",
  "detection_latitude",
  "detection_longitude",
  "detection_received_level_db",
  "detection_quality_code",
  "detection_n_animals",
  "detection_n_animals_min",
  "detection_n_animals_max",
  "detection_json",
  "detection_comments",
  "localization_method_code",
  "localization_latitude",
  "localization_latitude_min",
  "localization_latitude_max",
  "localization_longitude",
  "localization_longitude_min",
  "localization_longitude_max",
  "localization_distance_m",
  "localization_distance_m_min",
  "localization_distance_m_max",
  "localization_bearing",
  "localization_bearing_min",
  "localization_bearing_max",
  "localization_depth_method_code",
  "localization_depth_n_signals",
  "localization_depth_m",
  "localization_depth_m_min",
  "localization_depth_m_max",
  "localization_json",
  "localization_comments"
)

pacm_analysis_columns <- c(
  "organization_code",
  "deployment_code",
  "analysis_code",
  "recording_codes",
  "analysis_sound_source_codes",
  "analysis_granularity_code",
  "analysis_start_datetime",
  "analysis_end_datetime",
  "analysis_sample_rate_khz",
  "analysis_min_frequency_khz",
  "analysis_max_frequency_khz",
  "analysis_processing_code",
  "analysis_quality_code",
  "analysis_protocol_reference",
  "analysis_dataset_url",
  "analysis_release_data",
  "analysis_release_pacm",
  "detector_codes",
  "analysis_citation_codes",
  "detector_version",
  "detector_json",
  "detector_output_filename",
  "detector_output_uri",
  "analysis_analysts",
  "analysis_json",
  "analysis_comments"
)


# ==============================================================================
# 4. CALCURCEAS DEPLOYMENT LOOKUP
# ==============================================================================
# PACM deployment codes include the actual deployment date. Most dates match the
# first hourly bin, but CalCurCEAS_006 began before its first retained hour, so
# deployment codes should not be inferred from the hourly timestamps.
#
# Add a row here if a new deployment is added to the hourly input.

deployment_lookup <- tribble(
  ~deployment,       ~deployment_code,
  "CalCurCEAS_001",  "SWFSC_NEPAC_20240817_CALCURCEAS_001",
  "CalCurCEAS_002",  "SWFSC_NEPAC_20240820_CALCURCEAS_002",
  "CalCurCEAS_004",  "SWFSC_NEPAC_20240821_CALCURCEAS_004",
  "CalCurCEAS_006",  "SWFSC_NEPAC_20240823_CALCURCEAS_006",
  "CalCurCEAS_008",  "SWFSC_NEPAC_20240913_CALCURCEAS_008",
  "CalCurCEAS_009",  "SWFSC_NEPAC_20240914_CALCURCEAS_009",
  "CalCurCEAS_011",  "SWFSC_NEPAC_20240915_CALCURCEAS_011",
  "CalCurCEAS_012",  "SWFSC_NEPAC_20241001_CALCURCEAS_012",
  "CalCurCEAS_013",  "SWFSC_NEPAC_20241002_CALCURCEAS_013",
  "CalCurCEAS_014",  "SWFSC_NEPAC_20241002_CALCURCEAS_014",
  "CalCurCEAS_015",  "SWFSC_NEPAC_20241005_CALCURCEAS_015",
  "CalCurCEAS_016",  "SWFSC_NEPAC_20241006_CALCURCEAS_016",
  "CalCurCEAS_017",  "SWFSC_NEPAC_20241006_CALCURCEAS_017",
  "CalCurCEAS_018",  "SWFSC_NEPAC_20241006_CALCURCEAS_018",
  "CalCurCEAS_020",  "SWFSC_NEPAC_20241026_CALCURCEAS_020",
  "CalCurCEAS_021",  "SWFSC_NEPAC_20241027_CALCURCEAS_021",
  "CalCurCEAS_022",  "SWFSC_NEPAC_20241027_CALCURCEAS_022",
  "CalCurCEAS_023",  "SWFSC_NEPAC_20241028_CALCURCEAS_023",
  "CalCurCEAS_024",  "SWFSC_NEPAC_20241111_CALCURCEAS_024",
  "CalCurCEAS_025",  "SWFSC_NEPAC_20241120_CALCURCEAS_025",
  "CalCurCEAS_026",  "SWFSC_NEPAC_20241122_CALCURCEAS_026",
  "CalCurCEAS_027",  "SWFSC_NEPAC_20241123_CALCURCEAS_027"
) %>%
  mutate(deployment_key = str_to_upper(str_trim(deployment))) %>%
  select(deployment_key, deployment_code)


# ==============================================================================
# 5. HELPER FUNCTIONS
# ==============================================================================

parse_utc_datetime <- function(x) {
  raw <- str_trim(as.character(x))
  raw[raw %in% c("", "NA", "N/A", "NULL")] <- NA_character_

  parsed <- suppressWarnings(
    parse_date_time(
      raw,
      orders = c(
        "Ymd HMSz",
        "Ymd HMz",
        "Ymd HMS",
        "Ymd HM",
        "mdY HMS",
        "mdY HM",
        "dmY HMS",
        "dmY HM"
      ),
      tz = "UTC",
      exact = FALSE,
      truncated = 2
    )
  )

  # Also support Excel serial datetimes if they appear in a CSV export.
  numeric_rows <- which(
    is.na(parsed) &
      !is.na(raw) &
      str_detect(raw, "^[0-9]+(\\.[0-9]+)?$")
  )

  if (length(numeric_rows) > 0) {
    numeric_values <- suppressWarnings(as.numeric(raw[numeric_rows]))
    plausible_excel <- which(
      !is.na(numeric_values) &
        numeric_values >= 20000 &
        numeric_values <= 80000
    )

    if (length(plausible_excel) > 0) {
      parsed[numeric_rows[plausible_excel]] <- as.POSIXct(
        numeric_values[plausible_excel] * 86400,
        origin = "1899-12-30",
        tz = "UTC"
      )
    }
  }

  parsed
}

normalize_presence <- function(x) {
  raw <- str_to_upper(str_trim(as.character(x)))

  case_when(
    raw %in% c(
      "1", "TRUE", "T", "YES", "Y", "PRESENT", "DETECTED"
    ) ~ 1L,
    raw %in% c(
      "0", "FALSE", "F", "NO", "N", "ABSENT", "NOT_DETECTED"
    ) ~ 0L,
    TRUE ~ NA_integer_
  )
}

normalize_call_label <- function(x) {
  x %>%
    as.character() %>%
    str_trim() %>%
    str_to_upper() %>%
    str_replace_all("[_\\-]+", " ") %>%
    str_replace_all("\\s+", " ")
}

format_pacm_datetime <- function(x) {
  format(
    x,
    format = "%Y-%m-%dT%H:%M:%S%z",
    tz = "UTC"
  )
}

add_blank_pacm_columns <- function(data, output_columns) {
  blank_columns <- setdiff(output_columns, names(data))

  for (column_name in blank_columns) {
    data[[column_name]] <- NA_character_
  }

  data %>% select(all_of(output_columns))
}

print_problem_rows <- function(data, columns, max_rows = 20) {
  data %>%
    select(any_of(columns)) %>%
    head(max_rows) %>%
    print(n = max_rows, width = Inf)
}


# ==============================================================================
# 6. CALL-TYPE LOOKUP
# ==============================================================================
# Aliases are included so minor punctuation or naming differences do not break
# the conversion. The canonical PACM codes remain unchanged.

call_lookup <- tribble(
  ~input_call_type, ~analysis_code,  ~sound_source_code, ~call_type_code,
  "Blue A",         "BLWH_ANALYSIS", "BLWH",             "BLWH_A",
  "A",              "BLWH_ANALYSIS", "BLWH",             "BLWH_A",
  "BLWH A",         "BLWH_ANALYSIS", "BLWH",             "BLWH_A",

  "Blue B",         "BLWH_ANALYSIS", "BLWH",             "BLWH_B",
  "B",              "BLWH_ANALYSIS", "BLWH",             "BLWH_B",
  "BLWH B",         "BLWH_ANALYSIS", "BLWH",             "BLWH_B",

  "Blue D",         "BLWH_ANALYSIS", "BLWH",             "BLWH_D",
  "D",              "BLWH_ANALYSIS", "BLWH",             "BLWH_D",
  "BLWH D",         "BLWH_ANALYSIS", "BLWH",             "BLWH_D",

  "Fin 20 Hz",      "FIWH_ANALYSIS", "FIWH",             "FIWH_20HZ",
  "20 Hz",          "FIWH_ANALYSIS", "FIWH",             "FIWH_20HZ",
  "FIWH 20 Hz",     "FIWH_ANALYSIS", "FIWH",             "FIWH_20HZ",

  "Fin 40",         "FIWH_ANALYSIS", "FIWH",             "FIWH_40HZ",
  "Fin 40 Hz",      "FIWH_ANALYSIS", "FIWH",             "FIWH_40HZ",
  "Fin 40-80 Hz",   "FIWH_ANALYSIS", "FIWH",             "FIWH_40HZ",
  "40-80 Hz",       "FIWH_ANALYSIS", "FIWH",             "FIWH_40HZ",
  "FIWH 40 Hz",     "FIWH_ANALYSIS", "FIWH",             "FIWH_40HZ",

  "Bb",             "SEWH_ANALYSIS", "SEWH",             "SEWH_DS100HZ",
  "Sei DS",         "SEWH_ANALYSIS", "SEWH",             "SEWH_DS100HZ",
  "Sei 100 Hz",     "SEWH_ANALYSIS", "SEWH",             "SEWH_DS100HZ",
  "Sei DS 100 Hz",  "SEWH_ANALYSIS", "SEWH",             "SEWH_DS100HZ",
  "SEWH DS100HZ",   "SEWH_ANALYSIS", "SEWH",             "SEWH_DS100HZ"
) %>%
  mutate(normalized_call_type = normalize_call_label(input_call_type)) %>%
  select(
    normalized_call_type,
    analysis_code,
    detection_sound_source_code = sound_source_code,
    detection_call_type_code = call_type_code
  ) %>%
  distinct(normalized_call_type, .keep_all = TRUE)

expected_call_type_codes <- c(
  "BLWH_A",
  "BLWH_B",
  "BLWH_D",
  "FIWH_20HZ",
  "FIWH_40HZ",
  "SEWH_DS100HZ"
)


# ==============================================================================
# 7. READ AND PREPARE THE HOURLY DATA
# ==============================================================================

if (!file.exists(hourly_file)) {
  stop(
    "Hourly input file not found: ", hourly_file,
    "\nProvide the path as the first command-line argument or edit hourly_file.",
    call. = FALSE
  )
}

hourly <- read_csv(
  hourly_file,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
) %>%
  mutate(source_row = row_number())

required_hourly_columns <- c(
  "deployment",
  "hour_start",
  "hour_end",
  "call_type",
  "presence"
)

missing_hourly_columns <- setdiff(
  required_hourly_columns,
  names(hourly)
)

if (length(missing_hourly_columns) > 0) {
  stop(
    "Hourly file is missing required column(s): ",
    paste(missing_hourly_columns, collapse = ", "),
    call. = FALSE
  )
}

hourly_prepared <- hourly %>%
  mutate(
    deployment_original = deployment,
    call_type_original = call_type,
    presence_original = presence,
    deployment_key = str_to_upper(str_trim(deployment)),
    normalized_call_type = normalize_call_label(call_type),
    presence = normalize_presence(presence),
    start_datetime_utc = parse_utc_datetime(hour_start),
    end_datetime_exclusive_utc = parse_utc_datetime(hour_end)
  ) %>%
  left_join(deployment_lookup, by = "deployment_key") %>%
  left_join(call_lookup, by = "normalized_call_type") %>%
  mutate(
    interval_secs = as.numeric(
      difftime(
        end_datetime_exclusive_utc,
        start_datetime_utc,
        units = "secs"
      )
    ),
    end_datetime_utc = if (isTRUE(use_inclusive_hour_end)) {
      end_datetime_exclusive_utc - seconds(1)
    } else {
      end_datetime_exclusive_utc
    }
  )


# ==============================================================================
# 8. VALIDATE THE HOURLY DATA
# ==============================================================================

invalid_presence <- hourly_prepared %>%
  filter(is.na(presence))

if (nrow(invalid_presence) > 0) {
  print_problem_rows(
    invalid_presence,
    c(
      "source_row", "deployment_original", "hour_start", "call_type_original",
      "presence_original"
    )
  )
  stop(
    nrow(invalid_presence),
    " row(s) have blank or invalid presence values. Missing review results are ",
    "not converted to NOT_DETECTED.",
    call. = FALSE
  )
}

invalid_datetimes <- hourly_prepared %>%
  filter(
    is.na(start_datetime_utc) |
      is.na(end_datetime_exclusive_utc)
  )

if (nrow(invalid_datetimes) > 0) {
  print_problem_rows(
    invalid_datetimes,
    c("source_row", "deployment_original", "hour_start", "hour_end")
  )
  stop(
    nrow(invalid_datetimes),
    " row(s) have unparseable hour_start or hour_end values.",
    call. = FALSE
  )
}

invalid_intervals <- hourly_prepared %>%
  filter(is.na(interval_secs) | interval_secs != 3600)

if (nrow(invalid_intervals) > 0) {
  print_problem_rows(
    invalid_intervals,
    c(
      "source_row", "deployment_original", "hour_start", "hour_end",
      "interval_secs"
    )
  )
  stop(
    nrow(invalid_intervals),
    " row(s) are not exactly 3,600-second hourly intervals.",
    call. = FALSE
  )
}

unmatched_deployments <- hourly_prepared %>%
  filter(is.na(deployment_code)) %>%
  distinct(deployment_original)

if (nrow(unmatched_deployments) > 0) {
  print(unmatched_deployments, n = Inf)
  stop(
    "One or more deployments are missing from deployment_lookup. Add their ",
    "official PACM deployment codes in Section 4.",
    call. = FALSE
  )
}

unmatched_call_types <- hourly_prepared %>%
  filter(
    is.na(analysis_code) |
      is.na(detection_sound_source_code) |
      is.na(detection_call_type_code)
  ) %>%
  distinct(call_type_original)

if (nrow(unmatched_call_types) > 0) {
  print(unmatched_call_types, n = Inf)
  stop(
    "One or more call types are not recognized. Add an alias in Section 6.",
    call. = FALSE
  )
}

duplicate_hour_call_rows <- hourly_prepared %>%
  count(
    deployment_code,
    start_datetime_utc,
    end_datetime_exclusive_utc,
    detection_call_type_code,
    name = "n_rows"
  ) %>%
  filter(n_rows > 1)

if (nrow(duplicate_hour_call_rows) > 0) {
  print(duplicate_hour_call_rows, n = 20, width = Inf)
  stop(
    "Duplicate deployment-hour-call-type rows were found in the hourly input.",
    call. = FALSE
  )
}

reviewed_hours <- hourly_prepared %>%
  distinct(
    deployment_code,
    start_datetime_utc,
    end_datetime_exclusive_utc
  )

expected_hour_call_grid <- reviewed_hours %>%
  crossing(detection_call_type_code = expected_call_type_codes)

observed_hour_call_grid <- hourly_prepared %>%
  distinct(
    deployment_code,
    start_datetime_utc,
    end_datetime_exclusive_utc,
    detection_call_type_code
  )

missing_hour_call_types <- expected_hour_call_grid %>%
  anti_join(
    observed_hour_call_grid,
    by = c(
      "deployment_code",
      "start_datetime_utc",
      "end_datetime_exclusive_utc",
      "detection_call_type_code"
    )
  ) %>%
  arrange(
    deployment_code,
    start_datetime_utc,
    detection_call_type_code
  )

if (nrow(missing_hour_call_types) > 0) {
  print(missing_hour_call_types, n = 20, width = Inf)

  if (isTRUE(require_all_six_call_types_per_hour)) {
    stop(
      nrow(missing_hour_call_types),
      " deployment-hour-call-type combination(s) are missing. The script will ",
      "not silently create NOT_DETECTED rows.",
      call. = FALSE
    )
  } else {
    warning(
      nrow(missing_hour_call_types),
      " expected hourly call-type row(s) are missing and were not added."
    )
  }
}


# ==============================================================================
# 9. BUILD PACM DETECTIONS.CSV
# ==============================================================================

detections_required <- hourly_prepared %>%
  transmute(
    organization_code = organization_code_value,
    deployment_code,
    analysis_code,
    detection_start_datetime = format_pacm_datetime(start_datetime_utc),
    detection_end_datetime = format_pacm_datetime(end_datetime_utc),
    detection_effort_secs = as.integer(interval_secs),
    detection_sound_source_code,
    detection_call_type_code,
    detection_result_code = if_else(
      presence == 1L,
      "DETECTED",
      "NOT_DETECTED"
    )
  ) %>%
  arrange(
    deployment_code,
    detection_start_datetime,
    detection_call_type_code
  )

detections_out <- add_blank_pacm_columns(
  detections_required,
  pacm_detection_columns
)

if (!identical(names(detections_out), pacm_detection_columns)) {
  stop(
    "The detections output columns do not match the PACM schema.",
    call. = FALSE
  )
}


# ==============================================================================
# 10. BUILD PACM ANALYSES.CSV
# ==============================================================================
# The accepted CalCurCEAS file contains a species-level analysis interval only
# when that species had at least one DETECTED call during the hour.

analyses_required <- detections_required %>%
  filter(detection_result_code == "DETECTED") %>%
  distinct(
    deployment_code,
    analysis_code,
    detection_start_datetime,
    detection_end_datetime,
    .keep_all = TRUE
  ) %>%
  transmute(
    organization_code = organization_code_value,
    deployment_code,
    analysis_code,
    recording_codes = recording_code_value,
    analysis_sound_source_codes = detection_sound_source_code,
    analysis_granularity_code = "INTERVAL",
    analysis_start_datetime = detection_start_datetime,
    analysis_end_datetime = detection_end_datetime,
    analysis_sample_rate_khz = analysis_sample_rate_khz_value,
    analysis_min_frequency_khz = analysis_min_frequency_khz_value,
    analysis_max_frequency_khz = analysis_max_frequency_khz_value,
    analysis_processing_code = analysis_processing_code_value,
    analysis_quality_code = analysis_quality_code_value,
    analysis_protocol_reference = analysis_protocol_reference_value,
    analysis_release_data = TRUE,
    analysis_release_pacm = TRUE,
    detector_codes = detector_code_value
  )

analyses_out <- add_blank_pacm_columns(
  analyses_required,
  pacm_analysis_columns
)

if (!identical(names(analyses_out), pacm_analysis_columns)) {
  stop(
    "The analyses output columns do not match the PACM schema.",
    call. = FALSE
  )
}

duplicate_analysis_intervals <- analyses_required %>%
  count(
    deployment_code,
    analysis_code,
    analysis_start_datetime,
    analysis_end_datetime,
    name = "n_rows"
  ) %>%
  filter(n_rows > 1)

if (nrow(duplicate_analysis_intervals) > 0) {
  print(duplicate_analysis_intervals, n = 20, width = Inf)
  stop(
    "Duplicate species-level analysis intervals were created.",
    call. = FALSE
  )
}

# Confirm that every DETECTED row is represented by a species-level hourly
# interval in analyses.csv. NOT_DETECTED rows intentionally do not create an
# analysis interval.
positive_detection_intervals <- detections_required %>%
  filter(detection_result_code == "DETECTED") %>%
  distinct(
    deployment_code,
    analysis_code,
    detection_start_datetime,
    detection_end_datetime
  )

analysis_intervals <- analyses_required %>%
  transmute(
    deployment_code,
    analysis_code,
    detection_start_datetime = analysis_start_datetime,
    detection_end_datetime = analysis_end_datetime
  )

missing_positive_analysis_intervals <- positive_detection_intervals %>%
  anti_join(
    analysis_intervals,
    by = c(
      "deployment_code",
      "analysis_code",
      "detection_start_datetime",
      "detection_end_datetime"
    )
  )

if (nrow(missing_positive_analysis_intervals) > 0) {
  stop(
    "At least one DETECTED species-hour is missing from analyses.csv.",
    call. = FALSE
  )
}


# ==============================================================================
# 11. WRITE OUTPUTS
# ==============================================================================

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

write_csv(
  detections_out,
  detections_output_file,
  na = ""
)

write_csv(
  analyses_out,
  analyses_output_file,
  na = ""
)


# ==============================================================================
# 12. CONSOLE SUMMARY
# ==============================================================================

detection_summary <- detections_required %>%
  count(
    detection_call_type_code,
    detection_result_code,
    name = "n_rows"
  ) %>%
  arrange(
    detection_call_type_code,
    detection_result_code
  )

analysis_summary <- analyses_required %>%
  count(
    analysis_code,
    analysis_sound_source_codes,
    name = "n_rows"
  ) %>%
  arrange(analysis_code)

cat(
  "\n============================================================\n",
  "CALCURCEAS PACM FILES CREATED\n",
  "============================================================\n",
  "Hourly input: ", hourly_file, "\n",
  "Deployments: ", n_distinct(hourly_prepared$deployment_code), "\n",
  "Reviewed deployment-hours: ", nrow(reviewed_hours), "\n",
  "Detection rows: ", nrow(detections_out), "\n",
  "  DETECTED: ",
  sum(detections_required$detection_result_code == "DETECTED"), "\n",
  "  NOT_DETECTED: ",
  sum(detections_required$detection_result_code == "NOT_DETECTED"), "\n",
  "Analysis rows: ", nrow(analyses_out), "\n",
  "\nDetections output: ", detections_output_file, "\n",
  "Analyses output: ", analyses_output_file, "\n",
  sep = ""
)

cat("\nDetection rows by call type and result:\n")
print(detection_summary, n = Inf)

cat("\nAnalysis rows by species:\n")
print(analysis_summary, n = Inf)

cat("\nDone.\n")
