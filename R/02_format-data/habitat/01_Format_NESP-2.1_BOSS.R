###
# Project: SW Habitat & Fish 
# Data:    FOV habitat data
# Task:    Format Esperance & Bremer habitat data
# Author:  Claude Spencer
# Date:    February 2023
## 

# Clear memory ----
rm(list=ls())

# Libraries required ----
# To connect to GlobalArchive
library(devtools)
library(GlobalArchive)

# To tidy data
library(tidyr)
library(dplyr)
library(stringr)
library(readr)
library(ggplot2)

# Study name ----
study <- "nesp_2.1" 

# Read in metadata----
read_files_csv <- function(flnm) {
  flnm %>%
    readr::read_csv(col_types = readr::cols(.default = "c")) %>%
    GlobalArchive::ga.clean.names() %>%
    dplyr::mutate(campaignid = str_replace_all(basename(flnm), "_Metadata.csv", ""))
}


metadata <- list.files(path = "data/raw", 
                       recursive = T,
                       pattern = "_Metadata.csv",
                       full.names = T)  %>% # read in the file
  purrr::map_dfr(~read_files_csv(.)) %>%
  dplyr::select(campaignid, sample, latitude, longitude, date, site, location, successful.count, depth) %>% # select only these columns to keep
  dplyr::mutate(sample = as.character(sample)) %>% # in this example dataset, the samples are numerical
  dplyr::mutate(sample = ifelse(campaignid %in% "202111-202205_SA Commonwealth Marine Park Monitoring_StereoBRUVS" &
                                  str_detect(sample, "WE|MU"),
                                str_sub(sample, start = 1L, end = -6L), sample)) %>%
  # dplyr::filter(!successful.count %in% c("No", "no", "N", "n")) %>%
  dplyr::filter(!campaignid %in% c("201811_Comm_PearsonExpedition_StereoBRUVS")) %>% 
  glimpse() # preview

unique(metadata$campaignid)
names(metadata)

# read in the points annotations ----
read_tm_delim <- function(flnm) {
  read.delim(flnm,header = T,skip = 4,stringsAsFactors = FALSE, colClasses = "character") %>%
    dplyr::mutate(campaignid = basename(flnm)) %>%
    dplyr::mutate(campaignid = str_replace_all(.$campaignid,c("_Backwards_Dot Point Measurements.txt"= "",
                                                              "_Forwards_Dot Point Measurements.txt"= "",
                                                              "_Backwards_Relief_Dot Point Measurements.txt" = "",
                                                              "_Forwards_Relief_Dot Point Measurements.txt" = "",
                                                              "_Relief_Dot Point Measurements.txt" = "",
                                                              "_Dot Point Measurements.txt"= "",
                                                              "_Habitat" = "")))
}

# One campaign not in text file format
zeehan <- read.csv("data/raw/202205_ZEEHAN_AMP_BOSS_Habitat_Dot Point Measurements.csv") %>%
  ga.clean.names() %>%
  dplyr::mutate(sample = str_replace_all(filename, c(".jpg" = "", ".png" = "", ".jpeg" = "")),
                campaignid = "202205_ZEEHAN_AMP_BOSS") %>%
  separate(c1, into = c("catami_l2", "catami_l3"), sep = " > ") %>%
  dplyr::rename(catami_l4 = c3, catami_l5 = c4) %>%
  dplyr::select(campaignid, sample, starts_with("catami")) %>%
  glimpse()

test <- zeehan %>%
  distinct(catami_l2, catami_l3, catami_l4, catami_l5)

habitat <- list.files(path = "data/raw",
                      recursive = F,
                      pattern = "Dot Point Measurements.txt",
                      full.names = T) %>%
  purrr::map_dfr(~read_tm_delim(.)) %>% # read in the file
  ga.clean.names() %>% # tidy the column names using GlobalArchive function
  dplyr::select(campaignid, filename, opcode, period, 
                broad, morphology, type, 
                catami_l2_l3, catami_l4, catami_l5) %>% # Some campaigns use broad/morph/type, others catami levels
  dplyr::mutate(sample = ifelse(period %in% c("1", ""), opcode, NA)) %>%   # That sorts UWA bruv campaigns
  dplyr::mutate(sample = ifelse(campaignid %in% c("2022-12_Bremer_stereo-BOSS", "2022-12_Daw_stereo-BOSS"), period, sample)) %>%
  dplyr::mutate(sample = ifelse(is.na(sample), 
                                str_replace_all(filename, c(".png" = "", ".jpg"= "", ".jpeg" = "")), 
                                sample)) %>%
  dplyr::mutate(sample = ifelse(campaignid %in% "2021-04_Tasman_Fracture_CMR_stereoBRUVs",
                                str_extract(sample, "(?<=[:alpha:]{2}_)[:digit:]{1,}(?=[:graph:])"), sample)) %>%
  dplyr::mutate(sample = ifelse(campaignid %in% "202107_Huon_AMP_stereo_BRUV",
                                str_extract(sample, "(?<=Huon_)[:digit:]{1,}_[:digit:]{1,}(?=[:graph:])"), sample)) %>%
  dplyr::mutate(sample = ifelse(campaignid %in% "Salisbury_Investigator_MBH_BOSS_habitat",
                                str_replace_all(sample, c("_" = "-", "INC" = "INV")), sample)) %>%
  dplyr::mutate(sample = ifelse(campaignid %in% "202111-202205_SA Commonwealth Marine Park Monitoring_StereoBRUVS" &
                                  str_length(sample) == 11,
                                str_replace_all(sample, "([^_]+)_(.*)", "O"), sample)) %>%
  dplyr::mutate(sample = ifelse(campaignid %in% "202011-202011_SA Commonwealth Marine Park Monitoring_StereoBRUVS",
                                str_replace_all(sample, c("SW" = "CKWI", "SC" = "CKCI",
                                                          "SE" = "CKEI", "NW" = "WKWI",
                                                          "NE" = "WKEI", "NC" = "WKCI")), sample)) %>%
  dplyr::select(campaignid, sample, broad, morphology, type, starts_with("catami")) %>%
  separate(catami_l2_l3, into = c("catami_l2", "catami_l3"), sep = " > ") %>%
  bind_rows(zeehan) %>%
  dplyr::filter(!campaignid %in% c("201811_Comm_PearsonExpedition_StereoBRUVS")) %>% 
  glimpse() # preview

# test <- habitat %>%
#   dplyr::filter(campaignid %in% c("202011-202011_SA Commonwealth Marine Park Monitoring_StereoBRUVS",
#                                   "202111-202205_SA Commonwealth Marine Park Monitoring_StereoBRUVS")) %>%
#   distinct(campaignid, sample)

no.annotations <- habitat %>%
  group_by(campaignid, sample) %>%
  dplyr::summarise(points.annotated = n()) # Some images only have 3 directions annotated

# Check that the image names match the metadata samples -----
missing.metadata <- anti_join(habitat,metadata, 
                              by = c("campaignid","sample")) %>%
  distinct(campaignid, sample)

write.csv(missing.metadata, file = "data/raw/habitat-missing-metadata.csv",
          row.names = F)

missing.habitat <- anti_join(metadata,habitat, 
                             by = c("campaignid","sample")) %>%
  distinct(campaignid, sample) 

write.csv(missing.habitat, file = "data/raw/metadata-missing-habitat.csv",
          row.names = F)

# CREATE broad classifications
con <- read.csv("data/schema/CATAMI-UWA_conversion.csv") %>%
  ga.clean.names() %>%
  separate(catami_l2_l3, into = c("catami_l2", "catami_l3"), sep = " > ") %>%
  dplyr::mutate(catami_l2 = str_trim(catami_l2)) %>%
  dplyr::mutate(catami_l3 = str_trim(catami_l3)) %>%
  dplyr::mutate(catami_l4 = str_trim(catami_l4)) %>%
  dplyr::select(-catami_l5) %>%
  distinct() %>%
  dplyr::rename(broad.new = broad) %>%
  glimpse()

broad.points <- habitat %>%
  dplyr::mutate(broad = ifelse(catami_l2 %in% "Seagrasses" & catami_l4 %in% "", 
                                   "Seagrasses_Thalassodendron sp.", broad)) %>%
  dplyr::mutate(broad = ifelse(broad %in% "Seagrasses", paste(broad, type, sep = "_"), broad)) %>%
  dplyr::mutate(broad = ifelse(broad %in% "Seagrasses_",                        # Only a few in geographe bay that have no genus - they all grow on soft
                               "Seagrasses_Posidonia sp. with epiphytes algae", broad)) %>%
  left_join(con) %>%
  dplyr::mutate(broad = ifelse(is.na(broad), broad.new, broad)) %>%
  dplyr::mutate(broad = ifelse(catami_l2 %in% "Seagrasses" & !is.na(broad), paste(broad, catami_l4, sep = "_"), broad)) %>%
  dplyr::mutate(id = 1:nrow(.)) %>% # Key for tidyr::spread
  dplyr::select(-c(catami_l2, catami_l3, catami_l4, morphology, type, catami_l5, broad.new)) %>%
  dplyr::filter(!broad %in% c("",NA,"Unknown","Open.Water","Open Water")) %>%
  dplyr::mutate(broad = paste("broad", broad, sep = ".")) %>%
  dplyr::mutate(broad = str_replace_all(broad, " with epiphytes algae|\\s\\(Caab 63600903\\)|\\s\\(Caab 63600903\\)", "")) %>%
  dplyr::mutate(count = 1) %>%
  dplyr::group_by(campaignid, sample) %>%
  tidyr::pivot_wider(names_from = broad, values_from = count, values_fill = 0) %>%
  dplyr::group_by(campaignid, sample) %>%
  dplyr::summarise(across(starts_with("broad"), sum)) %>%
  ungroup() %>%
  # dplyr::select(-id) %>%
  dplyr::mutate(total.points.annotated = rowSums(.[,3:(ncol(.))],na.rm = TRUE )) %>%
  dplyr::mutate(`broad.Invertebrate complex` = `broad.Invertebrate complex` + `broad.Invertebrate Complex`) %>%
  dplyr::select(-`broad.Invertebrate Complex`) %>%
  ga.clean.names() %>%
  glimpse()

unique(broad.points$campaignid)

# Write final habitat data----
habitat.broad.points <- metadata %>%
  left_join(broad.points, by = c("campaignid","sample")) %>%
  # dplyr::filter(!is.na(broad.total.points.annotated)) %>%
  dplyr::select(-successful.count) %>%
  dplyr::mutate(planned.or.exploratory = case_when(campaignid %in% "2022-12_Daw_stereo-BOSS" &
                                                     str_detect(sample, "DAW-DC-C") ~ "Captain's pick",
                                                   campaignid %in% "2022-12_Bremer_stereo-BOSS" &
                                                     str_detect(sample, "c") ~ "Captain's pick",
                                                   campaignid %in% "2022-11_Salisbury_stereo-BRUVs" &
                                                     str_detect(sample, "SAL-BV-C") ~ "Captain's pick",
                                                   campaignid %in% "2022-11_Investigator_stereo-BRUVs" &
                                                     str_detect(sample, "CP") ~ "Captain's pick",
                                                   campaignid %in% "202107_Huon_AMP_stereo_BRUV" &
                                                     str_detect(sample, "175") ~ "Captain's pick",
                                                   .default = "MBH")) %>%
  dplyr::mutate(depth = abs(as.numeric(depth))) %>%
  glimpse()

unique(habitat.broad.points$campaignid)
unique(habitat.broad.points$location)

write.csv(habitat.broad.points,file = paste0("data/staging/",study,"_random-points_broad.habitat.csv"), 
          row.names = FALSE)
