#####################################################################
# Project: API query for NESP 2.1 project
# Data:    NESP 2.1 habitat and metadata
# Task:    Use GlobalArchive API to query data and save as an RDS
# Author:  Brooke Gibbons
# Date:    March 2024
#####################################################################

# Load libraries needed -----
library(httr)
library(tidyverse)
library(RJSONIO)
library(devtools)
devtools::install_github("GlobalArchiveManual/CheckEM") # If there has been any updates to the package then CheckEM will install
library(CheckEM)

username <- "public"
password <- "sharedaccess"
syn_id <- 20

# First, API call to the GlobalArchive benthic species list to join to the habitat data ----
benthic_list <- CheckEM::ga_api_benthic_list(username, password)

# API call for metadata ----
metadata <- CheckEM::ga_api_metadata(username, password, synthesis_id = syn_id) %>%
  glimpse()

# API call for habitat annotation data ----
habitat <- CheckEM::ga_api_habitat(username, password, synthesis_id = syn_id) %>%
  dplyr::mutate(subject = str_replace_all(subject, "AnnotationSubject", "GlobalArchiveBenthicList")) %>%
  left_join(benthic_list) %>%
  glimpse()

# Save as RDS
saveRDS(metadata, "data/tidy/NESP 2.1-api_metadata.RDS")
saveRDS(habitat, "data/tidy/NESP 2.1-api_habitat.RDS")
