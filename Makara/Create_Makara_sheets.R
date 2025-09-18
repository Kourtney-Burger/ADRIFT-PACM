# Create MAKARA Sheets from Deployment Details
#----Load packages----
library(openxlsx)
library(googlesheets4)
library(dplyr)
library(here)
library(tidyverse)
library(purrr)
library(readr)
library(jsonlite)
library(tibble)

# Taiki's helper function for dates
# Helper function to create all the "YYYY-MM-DDThh:mm:ssZ" formatted date strings that JSON will want. *Note that this requires that you convert all your dates in the deploy details data that you read in earlier (or anywhere else you might read dates from) to POSIXct format.*
posixToText <- function(x) {
  format(x, '%Y-%m-%dT%H:%M:%S')
}

#----Read in Data----
# Deployment Details
deployDetails <- read_sheet("https://docs.google.com/spreadsheets/d/10bxlwfVOe1LFfj69B_YddxcA0V14m7codYwgD2YncFk/edit?gid=42687545#gid=42687545", sheet = 'deployDetails')
# select deployments of interest
deployDetails_CalCurCEAS <- deployDetails %>%
  filter(Project == 'CalCurCEAS', Status == 'Complete')

#----Deployments----
# read in template and start blank sheet
# deployments <- read_csv(here('MAKARA Templates/deployments.csv'))

# Create sheet
deployments <- deployDetails_CalCurCEAS %>%
  transmute(
    organization_code = 'SWFSC',
    deployment_code = paste0(organization_code, "_NEPac_", format(as_datetime(as.numeric(Deployment_Date)), "%Y%m%d"), "_", Data_ID),
    project_code = 'SWFSC_CalCurCEAS_2024',
    site_code = 'NEPac',
    deployment_device_codes = paste0(
      "SoundTrap_", Instrument_ID, ",",
      "HTI_", SensorNumber_1, ",",
      "HTI_", SensorNumber_2, ",",
      "satellite-tracker_", str_replace_all(GPS_ID, ",", ""), ",",
      "depth-sensor_", Depth_Sensor),
    deployment_platform_type_code = 'drifting_buoy',
    deployment_datetime = paste0(posixToText(as_datetime(as.numeric(Deployment_Date))), "Z"),
    deployment_latitude = unlist(Deployment_Latitude),
    deployment_longitude = unlist(Deployment_Longitude),
    deployment_vessel = 'R/V Bold Horizon',
    deployment_cruise = 'CalCurCEAS_2024',
    recovery_datetime = paste0(posixToText(as_datetime(as.numeric(Recovery_Date))), "Z"),
    recovery_latitude = unlist(Recovery_Latitude),
    recovery_longitude = unlist(Recovery_Longitude),
    recovery_vessel = 'R/V Bold Horizon',
    recovery_cruise = 'CalCurCEAS_2024'
    )

#----Recordings----
# read in template and start blank sheet
# recordings <- read_csv(here('MAKARA Templates/recordings.csv'))

# Create sheet
recordings <- deployDetails_CalCurCEAS %>%
  transmute(
    organization_code = 'SWFSC',
    deployment_code = paste0(organization_code, "_NEPac_", format(as_datetime(as.numeric(Deployment_Date)), "%Y%m%d"), "_", Data_ID),
    recording_code = "SoundTrap_Recordings",
    recording_device_codes = paste0(
      "SoundTrap_", Instrument_ID, ",",
      "HTI_", SensorNumber_1, ",",
      "HTI_", SensorNumber_2),
    recording_start_datetime = paste0(posixToText(as_datetime(as.numeric(Data_Start))), "Z"),
    recording_end_datetime = paste0(posixToText(as_datetime(as.numeric(Data_End))), "Z"),
    recording_duration_secs = as.numeric(RecordingDuration_m) * 60,
    recording_interval_secs = as.numeric(RecordingInterval_m) * 60,
    recording_sample_rate_khz = SampleRate_kHz,
    recording_bit_depth = 16,
    recording_channel = 1,
    recording_n_channels = 2,
    recording_filetypes = "WAV",
    recording_timezone = "UTC",
    recording_usable_start_datetime = paste0(posixToText(as_datetime(as.numeric(Data_Start))), "Z"),
    recording_usable_end_datetime = paste0(posixToText(as_datetime(as.numeric(Data_End))), "Z"),
    recording_usable_min_frequency_khz = Quality_LowFreq,
    recording_usable_max_frequency_khz = Quality_HighFreq,
    recording_quality_code = toupper(Quality_Category),
    recording_device_depth_m = Deployment_Depth_m,
#     recording_json = , # User-adjusted gain on recording device, i.e., (in dB) for array and drop hydrophone systems or (High or Low) for Soundtraps
    recording_uri = paste0("gs:/swfsc-1/2024_CalCurCEAS/drifting_recorder/audio_wav/", Data_ID)
  )


#----Tracks----
# read in template and start blank sheet
# tracks <- read_csv(here('MAKARA Templates/tracks/tracks.csv'))

# Create sheet
tracks <- deployDetails_CalCurCEAS %>%
  transmute(
    organization_code = 'SWFSC',
    start_date = as_datetime(as.numeric(Deployment_Date)),
    deployment_code = paste0(organization_code, "_NEPac_", format(as_datetime(as.numeric(Deployment_Date)), "%Y%m%d"), "_", Data_ID),
    track_code = "drifting-buoy_track",
    track_uri = paste0("gs:/swfsc-1-working/2024_CalCurCEAS/drifting_recorder/", Data_ID, "/metadata/gps/", Data_ID, "_GPS.csv")
  )

#----Tracks Positions----
# read in template and start blank sheet
# track_positions <- read_csv(here('MAKARA Templates/tracks/track_positions.csv'))

# Create sheet
#Load and clean GPS data
gps_files <- list.files(path = here('R/GPS/'), pattern = "CalCurCEAS_.*\\.csv$", full.names = TRUE)

allGPS <- map_dfr(gps_files, ~ read_csv(.x, show_col_types = FALSE) %>%
                    mutate(DeviceId = as.character(DeviceId)))

track_positions <- allGPS %>%
  transmute(
    organization_code = 'SWFSC',
    deployment_code = paste0(organization_code, "_NEPac_", format(as_datetime(UTC), "%Y%m%d"), "_", DriftName),
    track_code = "drifting-buoy_track",
    track_position_datetime = paste0(format(as_datetime(UTC), "%Y-%m-%dT%H:%M:%S"), "Z"),
    track_position_latitude = Latitude,
    track_position_longitude = Longitude
  )


#----Optional Metadata Tables----
##----Devices Table----
###----read in data----
# read in all devices from inventory spreadsheets 
recorders <- read_sheet("https://docs.google.com/spreadsheets/d/12u6cwwbuM7BfPt2GKjMXEOwr9yeK0_ztD04aNVYoFIk/edit?gid=395300949#gid=395300949", sheet = 'RECORDERS')

hydrophones <- read_sheet("https://docs.google.com/spreadsheets/d/12u6cwwbuM7BfPt2GKjMXEOwr9yeK0_ztD04aNVYoFIk/edit?gid=395300949#gid=395300949", sheet = 'Hydrophones')

gps <- read_sheet("https://docs.google.com/spreadsheets/d/12u6cwwbuM7BfPt2GKjMXEOwr9yeK0_ztD04aNVYoFIk/edit?gid=395300949#gid=395300949", sheet = 'GPS')

depth <- read_sheet("https://docs.google.com/spreadsheets/d/12u6cwwbuM7BfPt2GKjMXEOwr9yeK0_ztD04aNVYoFIk/edit?gid=395300949#gid=395300949", sheet = 'depthSensors')

# read in template and start blank sheet
devices <- read_csv(here('MAKARA Templates/support/devices.csv'))

###----Recorders----
recording_devices <- recorders %>% 
  transmute(
    organization_code = 'SWFSC',
    device_code = paste0("SoundTrap-", as.character(`Instrument_ID (serial number)`)),
    device_type_code = 'recording_device',
    device_manufacturer = 'Ocean Instruments',
    device_model_number = gsub('^ST', 'SoundTrap ', Type),
    device_model_number = gsub('HF', ' High Frequency', device_model_number),
    device_model_number = gsub('STD', '', device_model_number),
    device_serial_number = as.character(`Instrument_ID (serial number)`)
    )

# device_json
recording_devices$device_json <- ifelse(is.na(recorders$`Calibration dB re. 1 μPa (gain for 1 channel)`), 
                         recorders$`Sensitivity dB re. 1 μPa/V`, recorders$`Calibration dB re. 1 μPa (gain for 1 channel)`)

    # Function to transform device_json values
    transform_device_json <- function(x) {
      if (is.na(x) || x == "") {
        return(NA)
      }
      
      # Check if it contains both High and Low
      if (str_detect(x, "High:") & str_detect(x, "Low:")) {
        high_val <- as.numeric(str_extract(x, "(?<=High: )[-+]?[0-9]*\\.?[0-9]+"))
        low_val <- as.numeric(str_extract(x, "(?<=Low: )[-+]?[0-9]*\\.?[0-9]+"))
        return(toJSON(list(GAIN = list(HIGH = high_val, LOW = low_val)), auto_unbox = TRUE))
      }
      
      # If it's just a single value (like -4 or -1.8)
      sensitivity_val <- as.numeric(x)
      return(toJSON(list(SENSITIVITY = sensitivity_val), auto_unbox = TRUE))
    }
    
    # Apply transformation
    recording_devices <- recording_devices %>%
      mutate(device_json = sapply(device_json, transform_device_json)) 
    
###----Hydrophones----
hydrophone_devices <- hydrophones %>% 
  mutate(
    organization_code = 'SWFSC',
    device_code = paste0("HTI_", as.character(`Serial Number`)),
    device_type_code = 'hydrophone-individual',
    device_manufacturer = 'High Tech, Inc.',
    device_model_number = Model,
    device_serial_number = as.character(`Serial Number`),
    device_json = sapply(`Hydrophone Sensitivity dB re: 1V/uPa`, function(x) {
      toJSON(list(SENSITIVITY = x), auto_unbox = TRUE)
    })
  ) %>%
  select(
    organization_code,
    device_code,
    device_type_code,
    device_manufacturer,
    device_model_number,
    device_serial_number,
    device_json
  )
    
###----GPS Sensors----
gps_devices <- gps %>% 
  transmute(
    organization_code = 'SWFSC',
    device_code = paste0('satellite-tracker_', as.character(`GPS Name`)),
    device_type_code = 'satellite-tracker',
    device_manufacturer = case_when(str_detect(`GPS Name`, regex("SO", ignore_case = TRUE)) ~ "Globalstar", TRUE ~ "SPOT LLC"),
    device_model_number = case_when(str_detect(`GPS Name`, regex("SO", ignore_case = TRUE)) ~ "SmartOne Solar", TRUE ~ "SPOT"),
    device_serial_number = as.character(`GPS Name`),
    device_json = ""
    )

###----Depth Sensors----
depth_devices <- depth %>% 
  transmute(
    organization_code = 'SWFSC',
    device_code = paste0('depth-sensor_', as.character(`Depth Sensor (serial number)`)),
    device_manufacturer = 'ReefNet Inc.',
    device_type_code = 'depth-sensor',
    device_model_number = Type,
    device_serial_number = as.character(`Depth Sensor (serial number)`),
    device_json = ""
    )
    
###----Combine sheets----
devices <- bind_rows(
  recording_devices,
  hydrophone_devices,
  gps_devices,
  depth_devices
)
    
##----Projects Table----
projects <- read_csv(here('MAKARA Templates/support/projects.csv'))
# created from scratch, add onto columns for new project
projects <- tibble(
  organization_code = "SWFSC",
  project_code = c("SWFSC_PASCAL_2016","SWFSC_CCES_2018", "SWFSC_ADRIFT_2021-2023", "SWFSC_CalCurCEAS_2024"),
  project_name = c("PASCAL_2016","CCES_2018", "ADRIFT_2021-2023", "CalCurCEAS_2024"),
  project_description = c("The Passive Acoustics Survey of Cetacean Abundance Levels (PASCAL 2016), was a large scale passive acoustic survey to obtain improved distribution and population size data for deep-diving species, namely beaked whales (Ziphiidae), sperm whales (Physeter macrocephalus), and dwarf and pygmy sperm whales (Kogia sp.).", 
                          "The 2018 California Current Ecosystem Survey (CCES) was a multidisciplinary survey of the marine ecosystem from southern British Columbia, Canada to northern Baja California, Mexico. This survey was a collaboration between the Southwest Fisheries Science Center’s (SWFSC) Fishery Resource Division (FRD) and Marine Mammal and Turtle Division (MMTD). The survey included oceanographic measurements, use of multi-frequency echosounders, surface trawls, vertically and obliquely integrating net tows, continuous underway fish egg sampling, visual line-transect surveys for marine mammals, photographic capture-recapture studies of marine mammals, strip transect surveys for seabirds, and passive acoustic surveys of marine mammals using Drifting Acoustic Spar Buoy Recorders (DASBRs). The recordings collected in this project have been analyzed to detect echolocation signals from beaked whales, sperm whales, and dwarf and pygmy sperm whales.", 
                          "ADRIFT in the California Current uses passive acoustic drifting buoys to study ocean sound in the California Current Ecosystem. The relatively low-cost buoys can be deployed and recovered from most vessels, including: research, fishing, and tourist boats. They drift autonomously and can be monitored shoreside via a satellite messenger. Data collected through the ADRIFT project will be used to assess noise levels and seasonal marine mammal acoustic presence in the California Current.", 
                          "The California Current Cetacean and Ecosystem Assessment Survey 2024 (CalCurCEAS 2024) was a 4 month shipboard line-transect survey off the contiguous U.S. West Coast. Data collected included marine mammal visual observations, seabird sightings, cetacean biopsies, eDNA water samples, passive acoustic recordings, and uncrewed ariel systems (UAS) photogrammetry. These data will contribute to estimations of population abundance and distribution of cetaceans in the California Current."), 
  project_contacts = 'Shannon Rankin <shannon.rankin@noaa.gov>'
)


##----SitesTable----
sites <- read_csv(here('MAKARA Templates/support/sites.csv'))

sites <- tibble(
  organization_code = 'SWFSC', 
  site_code = c('NePAC'), 
  site_description = c('Northeast Pacific Ocean for mobile deployments')
)

   
#----Save Sheets----
write_csv(deployments, 'CalCurCEAS_MAKARA_Sheets/deployments.csv')
write_csv(recordings, 'CalCurCEAS_MAKARA_Sheets/recordings.csv')
write_csv(tracks, 'CalCurCEAS_MAKARA_Sheets/tracks.csv')
write_csv(track_positions, 'CalCurCEAS_MAKARA_Sheets/track_positions.csv')
write_csv(devices, 'CalCurCEAS_MAKARA_Sheets/devices.csv')
write_csv(projects, 'CalCurCEAS_MAKARA_Sheets/projects.csv')
write_csv(sites, 'CalCurCEAS_MAKARA_Sheets/sites.csv')

##FOR TESTING
# Create a new workbook
wb <- createWorkbook()

# Add worksheets with each data frame
addWorksheet(wb, "deployments")
writeData(wb, "deployments", deployments)

addWorksheet(wb, "recordings")
writeData(wb, "recordings", recordings)

addWorksheet(wb, "tracks")
writeData(wb, "tracks", tracks)

addWorksheet(wb, "track_positions")
writeData(wb, "track_positions", track_positions)

addWorksheet(wb, "devices")
writeData(wb, "devices", devices)

addWorksheet(wb, "projects")
writeData(wb, "projects", projects)

addWorksheet(wb, "sites")
writeData(wb, "sites", sites)

# Save the workbook
saveWorkbook(wb, file = "CalCurCEAS_MAKARA_Sheets/CalCurCEAS_MAKARA_Sheets.xlsx", overwrite = TRUE)

