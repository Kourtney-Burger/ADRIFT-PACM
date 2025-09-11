# Create MAKARA Sheets from Deployment Details
#----Load packages----
library(openxlsx)
library(googlesheets4)
library(dplyr)
library(here)
library(tidyverse)
library(purrr)
library(readr)

# Taiki's helper function for dates
# Helper function to create all the "YYYY-MM-DDThh:mm:ssZ" formatted date strings that JSON will want. *Note that this requires that you convert all your dates in the deploy details data that you read in earlier (or anywhere else you might read dates from) to POSIXct format.*
posixToText <- function(x) {
  format(x, '%Y-%m-%dT%H:%M:%S')
}

#----Read in Data----
# Deployment Details
#deployDetails <- read_sheet("https://docs.google.com/spreadsheets/d/10bxlwfVOe1LFfj69B_YddxcA0V14m7codYwgD2YncFk/edit?gid=42687545#gid=42687545", sheet = 'deployDetails')
# select deployments of interest
#deployDetails_CalCurCEAS <- deployDetails %>% 
#  filter(Project == 'CalCurCEAS', Status == 'Complete')

#

#----Deployments Sheet----
# read in template and start blank sheet
deployments <- read_csv(here('MAKARA Templates/deployments.csv'))

##----Create sheet----
organization_code <- 'SWFSC'

#deployment_code
deployDetails_CalCurCEAS <- deployDetails_CalCurCEAS %>%
  mutate(start_date_unix = as.numeric(unlist(Deployment_Date)), start_date = as_datetime(start_date_unix)) %>%
  mutate(deployment_code = paste0(organization_code, "_NEPac_", format(start_date, "%Y%m%d"), "_", Data_ID))

deployments <- deployDetails_CalCurCEAS %>%
  select(deployment_code)

#organization_code
deployments <- deployments %>%
  mutate(organization_code = rep('SWFSC', 25))

deployments <- deployments %>% 
  select(organization_code, deployment_code)

#project_code
deployments <- deployments %>%
  mutate(project_code = 'CalCurCEAS_2024')

#site_code
deployments <- deployments %>% 
  mutate(site_code = 'NEPac')

#deployment_device_codes
# deployDetails_CalCurCEAS <- deployDetails_CalCurCEAS %>% 
#   mutate(recorder == )
deployDetails_CalCurCEAS <- deployDetails_CalCurCEAS %>% 
  mutate(GPS_ID = str_replace_all(GPS_ID, ",", "/"))
deployments <- deployments %>%
  mutate(deployment_device_codes = paste0("SoundTrap640-", deployDetails_CalCurCEAS$Instrument_ID,",",
                                          "HTI-92-WB_", deployDetails_CalCurCEAS$SensorNumber_1,",",
                                          "HTI-99-HF_", deployDetails_CalCurCEAS$SensorNumber_2,
                                          ",GPS_", deployDetails_CalCurCEAS$GPS_Tracker, 
                                          "_", deployDetails_CalCurCEAS$GPS_ID, 
                                          ",", "SensusDepth_", deployDetails_CalCurCEAS$Depth_Sensor))

#deployment_platform_type_code
deployments <- deployments %>% 
  mutate(deployment_platform_type_code = 'DRIFTING_BUOY')

#deployment_datetime
deployments <- deployments %>% 
  mutate(deployment_datetime = paste0(posixToText(as_datetime(as.numeric(deployDetails_CalCurCEAS$Deployment_Date))), "Z"))

#deployment_latitude
deployments <- deployments %>% 
  mutate(deployment_latitude = deployDetails_CalCurCEAS$Deployment_Latitude)

#deployment_longitude
deployments <- deployments %>% 
  mutate(deployment_longitude = deployDetails_CalCurCEAS$Deployment_Longitude)

#deployment_vessel
deployments <- deployments %>% 
  mutate(deployment_vessel = 'R/V Bold Horizon')

#deployment_cruise
deployments <- deployments %>%
  mutate(deployment_cruise = 'CalCurCEAS_2024')

#recovery_datetime
deployments <- deployments %>% 
  mutate(recovery_datetime = paste0(posixToText(as_datetime(as.numeric(deployDetails_CalCurCEAS$Recovery_Date))), "Z"))

#recovery_latitude
deployments <- deployments %>% 
  mutate(recovery_latitude = deployDetails_CalCurCEAS$Recovery_Latitude)

#recovery_longitude
deployments <- deployments %>% 
  mutate(recovery_longitude = deployDetails_CalCurCEAS$Recovery_Longitude)

#recovery_vessel
deployments <- deployments %>% 
  mutate(recovery_vessel = 'R/V Bold Horizon')

#recovery_cruise
deployments <- deployments %>%
  mutate(recovery_cruise = 'CalCurCEAS_2024')



#----Recordings Sheet----
# read in template and start blank sheet
recordings <- read_csv(here('MAKARA Templates/recordings.csv'))

##----Create sheet---- 
organization_code <- 'SWFSC'

#deployment_code
deployDetails_CalCurCEAS <- deployDetails_CalCurCEAS %>%
  mutate(start_date_unix = as.numeric(unlist(Deployment_Date)), start_date = as_datetime(start_date_unix)) %>%
  mutate(deployment_code = paste0(organization_code, "_NEPac_", format(start_date, "%Y%m%d"), "_", Data_ID))

recordings <- deployDetails_CalCurCEAS %>%
  select(deployment_code)

#organization_code
recordings <- recordings %>%
  mutate(organization_code = rep('SWFSC', 25))

recordings <- recordings %>% 
  select(organization_code, deployment_code)

#recording_code
recordings <- recordings %>% 
  mutate(recording_code = "SoundTrap640_RECORDINGS")

#recording_device_codes
recordings <- recordings %>%
  mutate(recording_device_codes = paste0("SoundTrap640-", deployDetails_CalCurCEAS$Instrument_ID,",",
                                         "HTI-92-WB_", deployDetails_CalCurCEAS$SensorNumber_1,",",
                                         "HTI-99-HF_", deployDetails_CalCurCEAS$SensorNumber_2))

#recording_start_datetime
recordings <- recordings %>% 
  mutate(recording_start_datetime = paste0(posixToText(as_datetime(as.numeric(deployDetails_CalCurCEAS$Data_Start))), "Z"))

#recording_end_datetime
recordings <- recordings %>% 
  mutate(recording_end_datetime = paste0(posixToText(as_datetime(as.numeric(deployDetails_CalCurCEAS$Data_End))), "Z"))

#recording_duration_secs
recordings <- recordings %>% 
  mutate(recording_duration_secs = as.numeric(deployDetails_CalCurCEAS$RecordingDuration_m)*60)

#recording_interval_secs
recordings <- recordings %>% 
  mutate(recording_interval_secs = as.numeric(deployDetails_CalCurCEAS$RecordingInterval_m)*60)

#recording_sample_rate_khz
recordings <- recordings %>% 
  mutate(recording_sample_rate_khz = deployDetails_CalCurCEAS$SampleRate_kHz)

#recording_bit_depth
recordings <- recordings %>% 
  mutate(recording_bit_depth = 16)

#recording_channel
recordings <- recordings %>% 
  mutate(recording_channel = 1)

#recording_n_channels
recordings <- recordings %>% 
  mutate(recording_n_channels = 2)

#recording_filetypes
recordings <- recordings %>% 
  mutate(recording_filetypes = 'WAV')

#recording_timezone
recordings <- recordings %>% 
  mutate(recording_timezone = 'UTC')

#recording_usable_start_datetime
recordings <- recordings %>% 
  mutate(recording_usable_start_datetime = paste0(posixToText(as_datetime(as.numeric(deployDetails_CalCurCEAS$Data_Start))), "Z"))

#recording_usable_end_datetime
recordings <- recordings %>% 
  mutate(recording_usable_end_datetime = paste0(posixToText(as_datetime(as.numeric(deployDetails_CalCurCEAS$Data_End))), "Z"))

#recording_usable_min_frequency_khz
recordings <- recordings %>% 
  mutate(recording_usable_min_frequency_khz = deployDetails_CalCurCEAS$Quality_LowFreq)

#recording_usable_max_frequency_khz
recordings <- recordings %>% 
  mutate(recording_usable_max_frequency_khz = deployDetails_CalCurCEAS$Quality_HighFreq)

#recording_quality_code
recordings <- recordings %>% 
  mutate(recording_quality_code = deployDetails_CalCurCEAS$Quality_Category)

#recording_device_depth_m
recordings <- recordings %>% 
  mutate(recording_device_depth_m = deployDetails_CalCurCEAS$Deployment_Depth_m)

#recording_uri
recordings <- recordings %>% 
  mutate(recording_uri = paste0("gs:/swfsc-1/2024_CalCurCEAS/drifting_recorder/audio_wav/", deployDetails_CalCurCEAS$Data_ID))


#----Tracks Sheet----
# read in template and start blank sheet
tracks <- read_csv(here('MAKARA Templates/tracks/tracks.csv'))

##----Create sheet----
organization_code <- 'SWFSC'

#deployment_code
deployDetails_CalCurCEAS <- deployDetails_CalCurCEAS %>%
  mutate(start_date_unix = as.numeric(unlist(Deployment_Date)), start_date = as_datetime(start_date_unix)) %>%
  mutate(deployment_code = paste0(organization_code, "_NEPac_", format(start_date, "%Y%m%d"), "_", Data_ID))

tracks <- deployDetails_CalCurCEAS %>%
  select(deployment_code)

#organization_code
tracks <- tracks %>%
  mutate(organization_code = rep('SWFSC', 25))

tracks <- tracks %>% 
  select(organization_code, deployment_code)

#track_code
tracks <- tracks %>%
  mutate(track_code = 'DRIFTING-BUOY_TRACK')

#track_uri
tracks <- tracks %>%
  mutate(track_uri = paste0('gs:/swfsc-1-working/2024_CalCurCEAS/drifting_recorder/', 
                            deployDetails_CalCurCEAS$Data_ID, '/metadata/gps/', 
                            deployDetails_CalCurCEAS$Data_ID,'_GPS.csv'))

#----Tracks Positions----
# read in template and start blank sheet
track_positions <- read_csv(here('MAKARA Templates/tracks/track_positions.csv'))

##----Create sheet----
#Load and clean GPS data
gps_files <- list.files(path = here('R/GPS/'), pattern = "CalCurCEAS_.*\\.csv$", full.names = TRUE)

allGPS <- map_dfr(gps_files, function(file) {
  read_csv(file, show_col_types = FALSE) %>%
    mutate(DeviceId = as.character(DeviceId))
})

# Create track_positions from allGPS dataframe
track_positions <- allGPS %>%
  #select required columns
  mutate(
    UTC = as_datetime(UTC), 
    organization_code = 'SWFSC', 
    deployment_code = paste0(organization_code, "_NEPac_", format(UTC, "%Y%m%d"), "_", DriftName),
    track_code = 'DRIFTING-BUOY_TRACK'
  ) %>%
  #fix datetime
  mutate(
    track_position_datetime = paste0(format(UTC, "%Y-%m-%dT%H:%M:%S"), "Z")
  ) %>%
  #select/rename columns
  select(
    organization_code, 
    deployment_code, 
    track_code, 
    track_position_datetime, 
    track_position_latitude = Latitude,
    track_position_longitude = Longitude
  )

#----Save Sheets----
write_csv(deployments, 'CalCurCEAS_MAKARA_Sheets/deployments.csv')
write_csv(recordings, 'CalCurCEAS_MAKARA_Sheets/recordings.csv')
write_csv(tracks, 'CalCurCEAS_MAKARA_Sheets/tracks.csv')
write_csv(track_positions, 'CalCurCEAS_MAKARA_Sheets/track_positions.csv')