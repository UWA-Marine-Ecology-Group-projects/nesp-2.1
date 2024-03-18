# Tidy UTAS Australia Morphospecies Catalogue schema for use at UWA - remove all the hectic columns
# Author: Claude Spencer
# Date: 15/12/2022

library(tidyverse)

schema <- read.delim("data/schema/AMC_schema_TM_20220817.txt", header = T) %>%
  dplyr::filter(!str_detect(CATAMI_L2_L3, 
                            "Bioturbation|Bedforms|Brachiopods|Chain|Crustacea|Fishes|Jellies|Molluscs|Relief|Rope|Replicate|Sea spiders|Worms|Physical|Fishing line")) %>%
  dplyr::mutate(CATAMI_L4 = ifelse(CATAMI_L2_L3 %in% "Seagrasses > Elliptical leaves", AMC, CATAMI_L4)) %>%
  dplyr::mutate(CATAMI_L4 = ifelse(CATAMI_L2_L3 %in% "Seagrasses > Strap-like leaves", AMC, CATAMI_L4)) %>%
  dplyr::mutate(CATAMI_L5 = ifelse(CATAMI_L2_L3 %in% "Macroalgae > Large canopy-forming", AMC, CATAMI_L5)) %>%
  dplyr::select(-c(CATAMI_L6_L7, AMC, FieldOfView, uuid)) %>%
  distinct() %>%
  glimpse()

write.table(schema, file = "data/schema/NESP-2.1_AMC-schema_TM_UWA.txt", sep = "\t",
            row.names = F, quote = F)
