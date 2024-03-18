rm(list = ls())

library(terra)
library(sf)
library(tidyverse)

aus <- st_read("data/spatial/shapefiles/cstauscd_r.mif",
               crs = 4283) %>%
  st_transform(4326) %>%
  dplyr::filter(!FEAT_CODE %in% "sea") %>%
  terra::vect()

bathy <- terra::rast("data/spatial/rasters/raw bathymetry/Australian_Bathymetry_and_Topography_2023_250m_MSL_cog.tif") %>%
  terra::crop(ext(110, 155, -45, -23.4394)) %>%
  terra::mask(aus, inverse = T) %>%
  terra::clamp(lower = -250, values = F) %>%
  terra::trim()
plot(bathy)

preds <- terra::terrain(bathy, v = c("slope", "roughness", "TPI", 
                                     "TRI","aspect", "TRIriley",
                                     "TRIrmsd"),
                        unit = "degrees", 
                        neighbors = 8)
plot(preds)

preds <- rast(list(bathy, preds))
names(preds)[1] <- "depth"
plot(preds)
preds <- terra::wrap(preds)

saveRDS(preds, file = "data/spatial/rasters/raw bathymetry/NESP-2.1_bathymetry-derivatives.rds")
