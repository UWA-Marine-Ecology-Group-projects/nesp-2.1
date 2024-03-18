###
# Project: NESP 2.1 - Esperance 2022-11
# Data:    BRUVS, BOSS
# Task:    Add metadata to meg labsheets from marks added to iPads using collector 
# author:  Brooke Gibbons
# date:    December 2022
##

library(tidyverse)
library(GlobalArchive)
library(lubridate)
library(googlesheets4)
library(sf)

# Find valid timezone ----
# Search for Perth, change to other location if the samples were not in WA.
grep("Perth", OlsonNames(), value = TRUE)

# Spatial files ----
sf_use_s2(F)

gdacrs <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
wgscrs <- "+proj=longlat +datum=WGS84"

wampa <- st_read("data/spatial/shapefiles/WA_MPA_2020.shp")
st_crs(wampa) <- gdacrs
wampa <- st_transform(wampa, wgscrs)

# Simplify names for plot legend
wampa$waname <- gsub("( \\().+(\\))", "", wampa$ZONE_TYPE)
wampa$waname <- gsub(" [1-4]", "", wampa$waname)
wampa$waname[wampa$NAME == "Hamelin Pool"]     <- "Marine Nature Reserve"
wampa$waname[wampa$NAME == "Abrolhos Islands"] <- "Fish Habitat Protection Area"
wampa$waname <- dplyr::recode(wampa$waname, 
                              "General Use" = "General Use Zone",
                              "Special Purpose Zone (Shore Based Activities)" = 
                                "Special Purpose Zone\n(Shore Based Activities)",
                              "Special Purpose Zone (Seagrass Protection) (IUCN IV)" = 
                                "Special Purpose Zone",
                              "MMA" = 'Marine Management Area' )

# Commonwealth parks
aumpa  <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")    # All aus mpas
aumpa <- st_transform(aumpa, wgscrs)

metadata.names <- c(system.number = NA_real_,
                    sample = NA_real_,
                    latitude = NA_real_,
                    longitude = NA_real_,
                    date = NA_real_,
                    time = NA_real_,
                    local.time = NA_real_,
                    site = NA_real_,
                    location = NA_real_,
                    # status = NA_real_, # add in from shapefiles
                    depth = NA_real_,
                    observer = NA_real_,
                    successful.count = NA_real_,
                    successful.length = NA_real_,
                    video.notes = NA_real_,
                    left.camera = NA_real_,
                    right.camera = NA_real_,
                    rear.camera = NA_real_)

# Check files in data folder 
dir("data/metadata/")

# Sometimes we add the camera data the day before sampling
# if the camera data is added after 12 PM then the next days date will be used
# BE CAREFUL of this if you actually started sampling after 12 PM
# You can change the time below

sampling.never.starts.after <- "12:00:00"

# Read in cameras 
bruv.cameras <- read.csv("data/metadata/BRUV_cameras_template_0.csv") %>%
  ga.clean.names() %>%
  dplyr::rename(system.number = ï.system.)%>%
  dplyr::mutate(time = mdy_hms(creationdate, tz = "UTC"))  %>%
  dplyr::mutate(local.time = with_tz(time, tz = "Australia/Perth")) %>%
  dplyr::mutate(date = substr(.$local.time, 1, 10)) %>%
  dplyr::mutate(noon = paste(date, sampling.never.starts.after)) %>% 
  dplyr::mutate(noon = ymd_hms(noon, tz = "Australia/Perth"))  %>%
  dplyr::mutate(date = ymd(date)) %>%
  dplyr::mutate(date = if_else(noon < local.time, (date + days(1)), date)) %>%
  dplyr::select(system.number, left.camera, right.camera, rear.camera, date) %>%
  dplyr::mutate(date = as.character(date)) %>%
  glimpse()

# Read in metadata
bruv.metadata <- read.csv("data/metadata/BRUV_metadata_template_0.csv") %>%
  ga.clean.names() %>%
  dplyr::rename(system.number = ï.system., depth = depth.m., longitude = x, latitude = y, video.notes = notes)%>%
  dplyr::mutate(time = mdy_hms(creationdate, tz = "UTC")) %>%
  dplyr::mutate(local.time = with_tz(time, tz = "Australia/Perth")) %>%
  dplyr::select(system.number, sample, depth, video.notes, latitude, longitude, time, local.time) %>%
  dplyr::mutate(date = substr(.$local.time, 1, 10)) %>%
  left_join(bruv.cameras) %>%
  add_column(!!!metadata.names[!names(metadata.names) %in% names(.)]) %>%
  dplyr::select(system.number, sample, latitude, longitude, date, time, local.time, site, location, depth, observer, successful.count, successful.length, video.notes, left.camera, right.camera, rear.camera) %>%
  dplyr::mutate(local.time = as.character(local.time)) %>%
  glimpse()

# metadata as sf
bruv.metadata.sf <- st_as_sf(bruv.metadata, coords = c("longitude", "latitude"), crs = wgscrs)

bruv.online <- read_sheet(url, sheet = "2022-11_Esperance_stereo-BRUVs") %>% ga.clean.names() %>%
  as.data.frame()

bruv.metadata.sf <- st_as_sf(bruv.online, coords = c("longitude", "latitude"), crs = wgscrs)

bruv.metadata.commonwealth <- bruv.metadata.sf %>%
  st_intersection(aumpa %>% dplyr::select(geometry, ZoneName)) %>%
  as.data.frame() %>%
  dplyr::select(-c(geometry)) %>%
  dplyr::mutate(status = ifelse(ZoneName %in% "National Park Zone", "No-take", "Fished")) %>%
  dplyr::rename(zone = ZoneName) %>%
  mutate(across(everything(), as.character)) %>%
  glimpse()

bruv.metadata.state <- bruv.metadata.sf %>%
  st_intersection(wampa %>% dplyr::select(geometry, waname)) %>%
  as.data.frame() %>%
  dplyr::select(-c(geometry)) %>%
  dplyr::mutate(status = ifelse(waname %in% "Sanctuary Zone", "No-take", "Fished")) %>%
  dplyr::rename(zone = waname) %>%
  mutate(across(everything(), as.character)) %>%
  glimpse()

bruv.metadata.zones <- #bruv.metadata %>%
  bruv.online %>% dplyr::select(-c(status)) %>%
  mutate(across(everything(), as.character)) %>%
  full_join(bruv.metadata.commonwealth) %>%
  # full_join(bruv.metadata.state) %>%
  dplyr::mutate(status = if_else(status %in% c(NA, NULL), "Fished", status)) %>%
  add_column(!!!metadata.names[!names(metadata.names) %in% names(.)]) %>%
  dplyr::select(system.number, sample, latitude, longitude, date, time, local.time, site, location, status, zone, depth, observer, successful.count, successful.length, video.notes) %>%
  glimpse()

write.csv(bruv.metadata.zones, "test data.csv")

# add to labsheet on google drive
# url <- "https://docs.google.com/spreadsheets/d/1ZfW-XJKP0BmY2UXPNquTxnO5-iHnG9Kw3UuJbALCcrs/edit#gid=830769099"
# sheet_append(url, bruv.metadata, sheet = "2022-11_Esperance_stereo-BRUVs")


# Read in cameras 

#### Match this to the 2022-05_PtCloates_Naked-BOSS_camera_metadata sheet.

sampling.never.starts.after <- "13:00:00"

boss.cameras <- read.csv("data/metadata/BOSS_cameras_template_0.csv") %>%
  ga.clean.names() %>%
  dplyr::rename(system.number = ï.system.)%>%
  dplyr::mutate(time = mdy_hms(creationdate, tz = "UTC"))  %>%
  dplyr::mutate(local.time = with_tz(time, tz = "Australia/Perth")) %>%
  dplyr::mutate(date = substr(.$local.time, 1, 10)) %>%
  dplyr::mutate(noon = paste(date, sampling.never.starts.after)) %>% 
  dplyr::mutate(noon = ymd_hms(noon, tz = "Australia/Perth"))  %>%
  dplyr::mutate(date = ymd(date)) %>%
  dplyr::mutate(date = if_else(noon < local.time, (date + days(1)), date)) %>%
  dplyr::select(date, system.number, 
                north.top.camera, north.bottom.camera,
                east.top.camera, east.bottom.camera,
                south.top.camera, south.bottom.camera,
                west.top.camera, west.bottom.camera,
                downwards.camera) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(!c(date, system.number), names_to = "position", values_to = "camera.number") %>%
  separate(position, into = c("face", "position"), sep = "[^[:alnum:]]+", extra = "merge") %>%
  dplyr::mutate(position = if_else(face %in% "downwards", "top.camera", position)) %>%
  distinct() %>%
  pivot_wider(names_from = position, values_from = camera.number) %>%
  glimpse()

names(boss.cameras)

# add to labsheet on google drive
# url <- "https://docs.google.com/spreadsheets/d/1ZfW-XJKP0BmY2UXPNquTxnO5-iHnG9Kw3UuJbALCcrs/edit#gid=830769099"
# sheet_append(url, boss.cameras, sheet = "2022-11_Esperance_stereo-BOSS_cameras")


# this is the only one that goes to the metadata!!

boss.metadata <- read.csv("data/metadata/BOSS_metadata_template_0.csv") %>%
  ga.clean.names() %>%
  dplyr::rename(system.number = ï.system., depth = depth.m., longitude = x, latitude = y, video.notes = notes)%>%
  dplyr::mutate(time = mdy_hms(creationdate, tz = "UTC")) %>%
  dplyr::mutate(local.time = with_tz(time, tz = "Australia/Perth")) %>%
  dplyr::select(system.number, sample, depth, video.notes, latitude, longitude, time, local.time) %>%
  dplyr::mutate(date = substr(.$local.time, 1, 10)) %>%
  # left_join(bruv.cameras) %>% # don't need to do this for BOSS as two sep sheets
  dplyr::mutate(local.time = as.character(local.time)) %>%
  glimpse()

# metadata as sf
boss.metadata.sf <- st_as_sf(boss.metadata, coords = c("longitude", "latitude"), crs = wgscrs)

boss.metadata.commonwealth <- boss.metadata.sf %>%
  st_intersection(aumpa %>% dplyr::select(geometry, ZoneName)) %>%
  as.data.frame() %>%
  dplyr::select(-c(geometry)) %>%
  dplyr::mutate(status = ifelse(ZoneName %in% "National Park Zone", "No-take", "Fished")) %>%
  dplyr::rename(zone = ZoneName) %>%
  mutate(across(everything(), as.character)) %>%
  glimpse()

boss.metadata.state <- boss.metadata.sf %>%
  st_intersection(wampa %>% dplyr::select(geometry, waname)) %>%
  as.data.frame() %>%
  dplyr::select(-c(geometry)) %>%
  dplyr::mutate(status = ifelse(waname %in% "Sanctuary Zone", "No-take", "Fished")) %>%
  dplyr::rename(zone = waname) %>%
  mutate(across(everything(), as.character)) %>%
  glimpse()

boss.metadata.zones <- boss.metadata %>%
  mutate(across(everything(), as.character)) %>%
  full_join(boss.metadata.commonwealth) %>%
  full_join(boss.metadata.state) %>%
  dplyr::mutate(status = if_else(status %in% c(NA, NULL), "Fished", status)) %>%
  add_column(!!!metadata.names[!names(metadata.names) %in% names(.)]) %>%
  dplyr::select(system.number, sample, latitude, longitude, date, time, local.time, site, location, status, zone, depth, observer, successful.count, successful.length, video.notes) %>%
  glimpse()


# add to labsheet on google drive
# url <- "https://docs.google.com/spreadsheets/d/1ZfW-XJKP0BmY2UXPNquTxnO5-iHnG9Kw3UuJbALCcrs/edit#gid=830769099"
# sheet_append(url, boss.metadata.zones, sheet = "2022-11_Esperance_stereo-BOSS")
