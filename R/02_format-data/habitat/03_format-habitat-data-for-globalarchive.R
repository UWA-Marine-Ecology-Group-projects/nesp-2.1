library(tidyverse)
devtools::install_github("GlobalArchiveManual/CheckEM")
3
library(CheckEM)

crosswalk <- read.csv("data/raw/habitat-crosswalk.csv")

habitat <- read.csv("data/tidy/NESP-2.1_broad-habitat.csv") %>%
  dplyr::select(-c(latitude, longitude, planned.or.exploratory, location, total.points.annotated, depth, broad.seagrasses.all))

catami <- CheckEM::catami %>%
  mutate_all(~if_else(. == "", NA, .))

long_hab <- habitat %>%
  pivot_longer(!c(campaignid, sample), names_to = "broad", values_to = "count") %>%
  left_join(crosswalk) %>%
  dplyr::select(-c(broad)) %>%
  dplyr::filter(count > 0) %>%
  replace(.=="NULL", NA) %>%
  mutate_all(~if_else(. == "", NA, .)) %>%
  left_join(catami) %>%
  dplyr::select(-qualifiers) %>%
  dplyr::mutate(caab_code = if_else(level_2 %in% "Sessile invertebrates", 2, caab_code))

unique(long_hab$caab_code)

extra1 <- read.csv("data/staging/2021-06-ApolloMP_stereoBRUVs._broad.habitat_points.csv") %>%
  dplyr::select(sample, latitude, longitude, site, location) %>%
  dplyr::mutate(campaignid = "2021-06-ApolloMP_stereoBRUVs") %>%
  dplyr::mutate(latitude = as.numeric(latitude))%>%
  dplyr::mutate(longitude = as.numeric(longitude))

extra2 <- read.csv("data/staging/Beagle_AMP_Stereo_BRUV_Habitat_NESP_Formatted.csv") %>%
  dplyr::select(sample, latitude, longitude, location, status, depth, ) %>%
  dplyr::mutate(campaignid = "Beagle_AMP_Stereo_BRUV") %>%
  dplyr::mutate(latitude = as.numeric(latitude))%>%
  dplyr::mutate(longitude = as.numeric(longitude)) %>%
  dplyr::mutate(sample = as.character(sample))

extra3 <- read.csv("data/staging/Freycinet_202108_Habitat.point.score.csv") %>%
  clean_names() %>%
  dplyr::select(sample, latitude, longitude, site, location, status, depth) %>%
  dplyr::mutate(campaignid = "Freycinet_202108") %>%
  dplyr::mutate(latitude = as.numeric(latitude))%>%
  dplyr::mutate(longitude = as.numeric(longitude))

extra4 <- list.files(path = "data/staging",
                  pattern = "_broad.habitat.csv",
                  full.names = T,
                  recursive = T) %>%
  purrr::map_dfr(~read_csv(., col_types = cols(.default = "c"))) %>%
  dplyr::select(campaignid, sample, latitude, longitude, location, depth, site, status) %>%
  dplyr::mutate(latitude = as.numeric(latitude))%>%
  dplyr::mutate(longitude = as.numeric(longitude)) %>%
  dplyr::mutate(depth = as.numeric(depth)) %>%
  glimpse()


metadata <- bind_rows(extra1, extra2, extra3, extra4) %>%
  dplyr::mutate(date.time = "2000-01-01T12:00:00+08:00") %>%
  dplyr::mutate(successful.count = NA) %>%
  dplyr::mutate(successful.length = NA) %>%
  replace_na(list(status = "Fished", depth = 00000.1)) %>%
  dplyr::mutate(observer.count = "Unknown", observer.length = "Unknown") 
                  
  
  

unique(metadata$campaignid)

names(metadata) %>% sort()

unique(metadata$depth) %>% sort()
unique(metadata$latitude) %>% sort()
unique(metadata$status) 
unique(metadata$successful.count)
unique(metadata$successful.length)

write.csv(metadata, "data/tidy/NESP 2.1 metadata.csv", row.names = FALSE)
write.csv(long_hab, "data/tidy/NESP 2.1 habitat.csv", row.names = FALSE)



test <- anti_join(long_hab, metadata) %>%
  distinct(campaignid)


claudes <- read.csv("data/tidy/NESP-2.1_broad-habitat.csv")
