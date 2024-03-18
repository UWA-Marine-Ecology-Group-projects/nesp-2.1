# THIS MIGHT BE BROKEN NOW THAT I HAVE FORMATTED DATA IN 01_FORMAT-DATA

###
# Project: Wide-field view stereo-video drop-camera
# Data:    BOSS Habitat data
# Task:    Visualise habitat data
# Author:  Claude Spencer
# Date:    August 2023
##


# Clear the environment ----
rm(list = ls())

# Load libraries ----
library(tidyverse)
library(GlobalArchive)
library(ggplot2)
library(scatterpie)
library(sf)
library(ggnewscale)
library(terra)

# Study name ----
study <- "2022-11_Esperance" 

# Load habitat data and join with metadata ----
tidy.habitat <- read.csv(paste0("data/tidy/", study,"_random-points_broad.habitat.csv")) %>%
  dplyr::mutate(`Sessile invertebrates` = broad.ascidians + broad.bryozoa +
                  broad.hydroids + broad.invertebrate.complex + broad.octocoral.black +
                  broad.sea.stars + broad.sponges + broad.stony.corals) %>%
  dplyr::rename(Rock = broad.consolidated, Sand = broad.unconsolidated,
                Seagrass = broad.seagrasses, Macroalgae = broad.macroalgae) %>%
  glimpse()

# Load shapefiles for plotting ----
sf_use_s2(F)                                                                    # Needed so st_crop doesn't error when cropping with lat longs                                                                   # Otherwise st_crop errors - need to find a better way
# Marine park data from CAPAD 2022
marine.parks <- st_read("data/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Marine.shp") %>%
  st_crop(xmin = 118,
          xmax = 125,
          ymin = -36,
          ymax = -32) %>%
  dplyr::mutate(ZONE_TYPE = str_replace_all(ZONE_TYPE, 
                                            "\\s*\\([^\\)]+\\)", "")) %>%
  dplyr::filter(ZONE_TYPE %in% c("National Park Zone", "Sanctuary Zone"))

# High res outline for Australia
aus <- st_read("data/spatial/shapefiles/cstauscd_r.mif", crs = 4283) %>%
  dplyr::filter(!FEAT_CODE %in% "sea") %>%
  st_crop(xmin = 118,
          xmax = 125,
          ymin = -36,
          ymax = -32)

# Coastal waters limit
cwatr <- st_read("data/spatial/shapefiles/amb_coastal_waters_limit.shp") %>%
  st_crop(xmin = 118,
          xmax = 125,
          ymin = -36,
          ymax = -32)

hab_fills <- scale_fill_manual(values = c("Sessile invertebrates" = "plum",
                                          "Macroalgae" = "darkgoldenrod4",
                                          "Seagrass" = "forestgreen",
                                          "Rock" = "grey40",
                                          "Sand" = "wheat"), 
                               name = "Habitat")

bathy <- rast("data/spatial/rasters/raw bathymetry/bath_250_good.tif") %>%
  crop(ext(118,
       125,
       -36,
       -32)) %>%
  as.data.frame(xy = T)

# Visualise data as a scatterpie ----
p1 <- ggplot() +
  geom_sf(data = aus, fill = "seashell2", colour = "black", size = 0.2) +
  geom_sf(data = marine.parks, aes(fill = ZONE_TYPE), alpha = 2/5, colour = NA) +
  scale_fill_manual(values = c("National Park Zone" = "#7bbc63",
                               "Sanctuary Zone" = "#bfd054"),
                    name = "Marine Parks") +
  new_scale_fill() +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.3) +
  geom_contour(data = bathy, aes(x = x, y = y, z = bath_250_good), 
                              breaks = c(-30, -70, -200),                                # Add here as needed
                              colour = "black", size = 0.1) +
  geom_scatterpie(data = tidy.habitat %>% arrange(Sand), aes(x = longitude, y = latitude),
                  cols = c("Sessile invertebrates", "Macroalgae", "Seagrass", "Rock", "Sand"),
                  pie_scale = 0.15, colour = NA) +
  hab_fills +
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(min(tidy.habitat$longitude), 121.3),
           ylim = c(min(tidy.habitat$latitude), max(tidy.habitat$latitude))) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#b9d1d6", colour = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(0.5, "cm"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8, face = "bold"))

png(filename = paste0("plots/", study, "_eastern_scatterpies.png"),
    units = "in", res = 300, height = 5, width = 7.5)
p1
dev.off()

p2 <- ggplot() +
  geom_sf(data = aus, fill = "seashell2", colour = "black", size = 0.2) +
  geom_sf(data = marine.parks, aes(fill = ZONE_TYPE), alpha = 2/5, colour = NA) +
  scale_fill_manual(values = c("National Park Zone" = "#7bbc63",
                               "Sanctuary Zone" = "#bfd054"),
                    name = "Marine Parks") +
  new_scale_fill() +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.3) +
  geom_contour(data = bathy, aes(x = x, y = y, z = bath_250_good), 
               breaks = c(-30, -70, -200),                                # Add here as needed
               colour = "black", size = 0.1) +
  geom_scatterpie(data = tidy.habitat %>% arrange(Sand), aes(x = longitude, y = latitude),
                  cols = c("Sessile invertebrates", "Macroalgae", "Seagrass", "Rock", "Sand"),
                  pie_scale = 0.15, colour = NA) +
  hab_fills +
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(121.8, max(tidy.habitat$longitude)),
           ylim = c(min(tidy.habitat$latitude), max(tidy.habitat$latitude))) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#b9d1d6", colour = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(0.5, "cm"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8, face = "bold"))

png(filename = paste0("plots/", study, "_western_scatterpies.png"),
    units = "in", res = 300, height = 5, width = 8)
p2
dev.off()
