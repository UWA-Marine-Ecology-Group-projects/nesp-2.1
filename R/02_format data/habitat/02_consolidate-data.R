# Clear the environment of large items
rm(list = ls())

library(tidyverse)
library(GlobalArchive)
library(sf)
library(terra)

dat <- list.files(path = "data/staging",
                  pattern = "_broad.habitat.csv",
                  full.names = T,
                  recursive = T) %>%
  purrr::map_dfr(~read_csv(., col_types = cols(.default = "c"))) %>%
  dplyr::mutate(ifelse(is.na(total.points.annotated), broad.total.points.annotated, total.points.annotated)) %>%
  dplyr::select(-c(broad.kelps, broad.reef, starts_with("fov"), mean.relief, sd.relief, 
                   method, starts_with("successful"), commonwealth.zone, state.zone, 
                   status, date, time, site, broad.total.points.annotated, total.points.annotated)) %>%   
  dplyr::select(campaignid, sample, location, planned.or.exploratory,longitude, 
                latitude, depth, starts_with("broad")) %>%
  dplyr::mutate(location = case_when(location %in% "NPZ6" ~ "Abrolhos",
                                     location %in% "NPZ9" ~ "Abrolhos",
                                     location %in% "South-west" ~ "South-west Corner",
                                     .default = location)) %>%
  dplyr::mutate_at(vars(starts_with("broad")), ~replace_na(., "0")) %>%
  dplyr::mutate_at(vars(starts_with("broad")), as.numeric) %>%
  dplyr::mutate(total.points.annotated = rowSums(.[,8:(ncol(.))],na.rm = TRUE )) %>%
  dplyr::mutate(across(everything(), as.character)) %>%
  dplyr::filter(total.points.annotated > 0) %>%
  glimpse()

unique(dat$campaignid)

apollo <- read.csv("data/staging/2021-06-ApolloMP_stereoBRUVs._broad.habitat_points.csv") %>%
  ga.clean.names() %>%
  dplyr::mutate(campaignid = "2021-06-ApolloMP_stereoBRUVs",
                planned.or.exploratory = "MBH") %>%
  dplyr::select(campaignid, sample, longitude, latitude, location,  planned.or.exploratory,starts_with("broad")) %>%
  dplyr::mutate(total.points.annotated = rowSums(.[,7:(ncol(.))], na.rm = TRUE )) %>%
  dplyr::mutate(across(everything(), as.character)) %>%
  glimpse()

beagle <- read.csv("data/staging/Beagle_AMP_Stereo_BRUV_Habitat_NESP_Formatted.csv") %>%
  ga.clean.names() %>%
  dplyr::mutate(campaignid = "Beagle_AMP_Stereo_BRUV") %>%
  dplyr::rename(total.points.annotated = broad.total.points.annotated.minus.openwater) %>%
  dplyr::mutate(broad.unconsolidated = broad.unconsolidated.sand + broad.unconsolidated.bivalves +
                  broad.unconsolidated.screwshells + broad.unconsolidated.biogenic) %>%
  dplyr::select(campaignid, sample, longitude, latitude, location, planned.or.exploratory, starts_with("broad"), 
                -c(broad.openwater, broad.total.points.annotated, broad.unconsolidated.sand, 
                   broad.unconsolidated.bivalves, broad.unconsolidated.screwshells, broad.unconsolidated.biogenic)) %>%
  dplyr::mutate_at(vars(starts_with("broad")), ~replace_na(., 0)) %>%
  dplyr::mutate(across(everything(), as.character)) %>%
  glimpse()

freycinet <- read.csv("data/staging/Freycinet_202108_Habitat.point.score.csv") %>%
  ga.clean.names() %>%
  dplyr::mutate(campaignid = "Freycinet_202108",
                broad.consolidated = broad.consolidated.rock.turf.mat + 
                  broad.consolidated.rock.veneer,
                broad.invertebrate.complex = broad.invertebrate.complex.complex.1. +
                  broad.invertebrate.complex.complex.2. + broad.invertebrate.complex.complex.3. +
                  broad.invertebrate.complex.complex.4.,
                broad.macroalgae = broad.macroalgae.erect.fine.branching.red +
                  broad.macroalgae.large.canopy.forming.brown,
                broad.sponges = broad.sponges.crusts.encrusting +
                  broad.sponges.erect.forms.branching + broad.sponges.erect.forms.lamiar +
                  broad.sponges.erect.forms.palmate + broad.sponges.hollow.forms.cups.and.alikes +
                  broad.sponges.large.multi.form.10cm. + broad.sponges.massive.forms.simple) %>%
  dplyr::rename(broad.unconsolidated = broad.unconsolidated.pebble.gravel.biogenic.coquina.shellhash) %>%
  dplyr::select(campaignid, sample, longitude, latitude, location, planned.or.exploratory,
                broad.consolidated, broad.invertebrate.complex, broad.macroalgae,
                broad.sponges, broad.unconsolidated) %>%
  dplyr::mutate(total.points.annotated = rowSums(.[,7:(ncol(.))], na.rm = TRUE )) %>%
  dplyr::mutate(across(everything(), as.character)) %>%
  glimpse()

final.dat <- bind_rows(apollo, beagle, dat, freycinet) %>%
  dplyr::mutate_at(vars(starts_with("broad")), ~replace_na(., "0")) %>%
  dplyr::filter(planned.or.exploratory %in% "MBH") %>%
  dplyr::mutate(broad.seagrasses.all = as.numeric(broad.seagrasses) + as.numeric(broad.seagrasses_amphibolis.sp.) + 
                  as.numeric(broad.seagrasses_posidonia.sp.) +
                  as.numeric(broad.seagrasses_halophila.sp.) + as.numeric(broad.seagrasses_rupia.sp.) + 
                  as.numeric(broad.seagrasses_zostera.sp.) +
                  as.numeric(broad.seagrasses_thalassodendron.sp.)) %>%
  glimpse()

unique(final.dat$campaignid)
unique(final.dat$location)

write.csv(final.dat, "data/tidy/NESP-2.1_broad-habitat.csv",
          row.names = F)


test <- final.dat %>%
  dplyr::mutate(longitude = as.numeric(longitude),
                latitude = as.numeric(latitude)) %>%
  vect(geom = c("longitude", "latitude"), crs = "epsg:4326")
plot(test)

bathy <- rast("data/spatial/rasters/raw bathymetry/Australian_Bathymetry_and_Topography_2023_250m_MSL_cog.tif")

dat.depth <- cbind(final.dat, terra::extract(bathy, test)) %>%
  glimpse()

ggplot() +
  geom_point(data = dat.depth %>% dplyr::filter(broad.seagrasses.all >  0), 
             aes(x = Australian_Bathymetry_and_Topography_2023_250m_MSL_cog, y = broad.seagrasses.all))
