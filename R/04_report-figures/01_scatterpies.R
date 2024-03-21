library(tidyverse)
library(scatterpie)
library(sf)
library(ggnewscale)
library(terra)
library(tidyterra)
library(scales)

habi <- read.csv("data/tidy/NESP-2.1_broad-habitat.csv") %>%
  dplyr::mutate("Sessile invertebrates" = broad.ascidians + broad.bryozoa + broad.octocoral.black + 
                  broad.sponges + broad.hydroids + broad.crinoids + broad.invertebrate.complex +
                  broad.stony.corals + broad.true.anemones + broad.zoanthids) %>%
  dplyr::rename('Shelf unvegatated sediments' = broad.unconsolidated,
                "Bare rocky reef" = broad.consolidated,
                Seagrass = broad.seagrasses.all,
                Macroalgae = broad.macroalgae) %>%
  dplyr::mutate(amp = case_when(location %in% "Apollo CMR" ~ "Apollo",
                                location %in% "Beagle AMP" ~ "Beagle",
                                location %in% "South-west Corner" ~ "South-west Corner",
                                location %in% "Abrolhos" ~ "Abrolhos",
                                location %in% c("Western Kangaroo Island (Commonwealth)",
                                                "Western Kangaroo Island (State)") ~ "Western Kangaroo Island",
                                location %in% "Geographe" ~ "Geographe",
                                location %in% c("Tas Fracture", "Tas Fracture (Ref)") ~ "Tasman Fracture",
                                location %in% "Huon AMP" ~ "Huon",
                                location %in% "Murat Reef (Commonwealth)" ~ "Murat",
                                location %in% "Western Eyre" ~ 'Western Eyre',
                                location %in% c("Eastern Recherche", "Salisbury MBH") ~ "Eastern Recherche",
                                location %in% "Franklin AMP" ~ "Franklin",
                                location %in% "Zeehan AMP" ~ "Zeehan",
                                location %in% "Murray AMP" ~ "Murray",
                                location %in% "Investigator MBH" ~ "Investigator",
                                location %in% c("Freycinet AMP", "Freycinet North", "Freycinet Inshore") ~ "Freycinet")) %>%
  dplyr::mutate(amp = if_else(campaignid %in% "2022-11_Investigator_stereo-BRUVs", "Investigator", amp)) %>%
  dplyr::mutate(amp = if_else(campaignid %in% "2022-12_Bremer_stereo-BOSS", "Bremer", amp)) %>%
  glimpse()

hab_fills <- scale_fill_manual(values = c("Sessile invertebrates" = "plum",
                                          "Macroalgae" = "darkgoldenrod4",
                                          "Seagrass" = "forestgreen",
                                          "Bare rocky reef" = "grey40",
                                          "Shelf unvegatated sediments" = "wheat"), 
                               name = "Habitat")

# Load necessary spatial files
# Australian outline and state and commonwealth marine parks
aus    <- st_read("data/spatial/shapefiles/aus-shapefile-w-investigator-stokes.shp") %>%
  st_transform(4326)

# Read in bathymetry data
bathy <- rast("data/spatial/rasters/raw bathymetry/Australian_Bathymetry_and_Topography_2023_250m_MSL_cog.tif") %>%
  clamp(upper = 20, values = F)
names(bathy) <- "Depth"
plot(bathy)
summary(bathy)

topo <- rast("data/spatial/rasters/raw bathymetry/Australian_Bathymetry_and_Topography_2023_250m_MSL_cog.tif") %>%
  clamp(lower = 0, values = F)
plot(topo)

# Hillshading
hill <- rast("data/spatial/rasters/raw bathymetry/250m_australia_hillshade.tif")

# Hillshading palette
pal_greys <- hcl.colors(1000, "Grays")

# Load marine parks
marine.parks <- st_read("data/spatial/shapefiles/Australian_Marine_Parks.shp") %>%
  glimpse()
plot(marine.parks["ZONENAME"])

unique(marine.parks$ZONENAME)
mpa_fills <- scale_fill_manual(values = c("National Park Zone" = "#7bbc63",
                                          "Habitat Protection Zone" = "#fff8a3",# Commonwealth MPA colours
                                          # "Habitat Protection Zone (Reefs)" = "#fbff85",
                                          "Multiple Use Zone" = "#b9e6fb",
                                          # "Recreational Use Zone" = "#ffb36b",
                                          # "Sanctuary Zone" = "#f7c0d8",
                                          "Special Purpose Zone (Mining Exclusion)" = "#368ac1",
                                          # "Special Purpose Zone (Trawl)" = "#3e8ec4",
                                          "Special Purpose Zone" = "#6daff4"
))

unique(habi$amp)

# APOLLO
habitat_park <- dplyr::filter(habi, amp %in% "Apollo")

e <- ext(143.4725, 143.6833,-39.0183, -38.8312)

hillshadec <- terra::crop(hill, e)
topoc <- terra::crop(topo, e)
bathyc <- terra::crop(bathy, e)
ausc <- st_crop(aus, e)

png("plots/report/apollo_scatterpie.png", height = 7, width = 9,
    res = 900, units = "in")
ggplot() +
  geom_spatraster(data = hillshadec, alpha = 1, show.legend = F, maxcell = Inf) +
  scale_fill_gradientn(colors = pal_greys, na.value = NA) +
  new_scale_fill() +
  geom_spatraster(data = bathyc, maxcell = Inf, alpha = 0.6) +
  scale_fill_gradientn(colours = c("#061442","#014091", "#2b63b5","#6794d6"),
                       values = rescale(c(-100, -40,-20, 0)),
                       na.value = "#A0C3D8", name = "Depth")  +
  new_scale_fill() +
  geom_sf(data = ausc) +
  geom_sf(data = marine.parks, aes(fill = ZONENAME), alpha = 0.3, colour = NA) +
  scale_fill_manual(values = c("Multiple Use Zone" = "#b9e6fb"), 
                    name = "Australian Marine Parks") +
  new_scale_fill() +
  geom_point(data = habitat_park, aes(x = longitude, y = latitude),
             fill = "white", colour = "white", alpha = 0.1, size = 7, shape = 16) +
  geom_scatterpie(data = habitat_park, aes(x = longitude, y = latitude),
                  cols = c("Sessile invertebrates", "Macroalgae", "Seagrass", "Bare rocky reef", "Shelf unvegatated sediments"),
                  colour = NA, pie_scale = 1) +
  hab_fills +
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(143.4725, 143.65), 
           ylim = c(-39, -38.85), 
           crs = 4326) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
dev.off()

# BREMER
habitat_park <- dplyr::filter(habi, amp %in% "Bremer")

e <- ext(119.487, 120.134, -34.8, -34.042)

hillshadec <- terra::crop(hill, e)
topoc <- terra::crop(topo, e)
bathyc <- terra::crop(bathy, e)
ausc <- st_crop(aus, e)

png("plots/report/bremer_scatterpie.png", height = 6.5, width = 9,
    res = 900, units = "in")
ggplot() +
  geom_spatraster(data = hillshadec, alpha = 1, show.legend = F, maxcell = Inf) +
  scale_fill_gradientn(colors = pal_greys, na.value = NA) +
  new_scale_fill() +
  geom_spatraster(data = bathyc, maxcell = Inf, alpha = 0.6) +
  scale_fill_gradientn(colours = c("#061442","#014091", "#2b63b5","#6794d6"),
                       values = rescale(c(-100, -40,-20, 0)),
                       na.value = "#A0C3D8", name = "Depth")  +
  new_scale_fill() +
  geom_sf(data = ausc) +
  geom_sf(data = marine.parks, aes(fill = ZONENAME), alpha = 0.3, colour = NA) +
  scale_fill_manual(values = c("Special Purpose Zone (Mining Exclusion)" = "#368ac1",
                    "National Park Zone" = "#7bbc63"), 
                    name = "Australian Marine Parks") +
  new_scale_fill() +
  geom_point(data = habitat_park, aes(x = longitude, y = latitude),
             fill = "white", colour = "white", alpha = 0.1, size = 5, shape = 16) +
  geom_scatterpie(data = habitat_park, aes(x = longitude, y = latitude),
                  cols = c("Sessile invertebrates", "Macroalgae", "Seagrass", "Bare rocky reef", "Shelf unvegatated sediments"),
                  colour = NA, pie_scale = 1.1) +
  hab_fills +
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(119.55, 120.1), 
           ylim = c(-34.680, -34.1), 
           crs = 4326) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
dev.off()

# BEAGLE
habitat_park <- dplyr::filter(habi, amp %in% "Beagle")

e <- ext(146.37, 147.594,-39.702, -38.9)

hillshadec <- terra::crop(hill, e)
topoc <- terra::crop(topo, e)
bathyc <- terra::crop(bathy, e)
ausc <- st_crop(aus, e)

png("plots/report/beagle_scatterpie.png", height = 5.5, width = 9,
    res = 900, units = "in")
ggplot() +
  geom_spatraster(data = hillshadec, alpha = 1, show.legend = F, maxcell = Inf) +
  scale_fill_gradientn(colors = pal_greys, na.value = NA) +
  new_scale_fill() +
  geom_spatraster(data = bathyc, maxcell = Inf, alpha = 0.6) +
  scale_fill_gradientn(colours = c("#061442","#014091", "#2b63b5","#6794d6"),
                       values = rescale(c(-100, -40,-20, 0)),
                       na.value = "#A0C3D8", name = "Depth")  +
  new_scale_fill() +
  geom_sf(data = ausc) +
  geom_sf(data = marine.parks, aes(fill = ZONENAME), alpha = 0.3, colour = NA) +
  scale_fill_manual(values = c("Multiple Use Zone" = "#b9e6fb"), 
                    name = "Australian Marine Parks") +
  new_scale_fill() +
  geom_point(data = habitat_park, aes(x = longitude, y = latitude),
             fill = "white", colour = "white", alpha = 0.1, size = 4, shape = 16) +
  geom_scatterpie(data = habitat_park, aes(x = longitude, y = latitude),
                  cols = cols = c("Sessile invertebrates", "Macroalgae", "Seagrass", "Bare rocky reef", "Shelf unvegatated sediments"),
                  colour = NA, pie_scale = 0.45) +
  hab_fills +
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(146.404, 147.594), 
           ylim = c(-39.702, -38.952), 
           crs = 4326) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
dev.off()

# SOUTH-WEST CORNER
habitat_park <- dplyr::filter(habi, amp %in% "South-west Corner")

e <- ext(114.5249, 115.0796,-34.1800, -33.6326)

hillshadec <- terra::crop(hill, e)
topoc <- terra::crop(topo, e)
bathyc <- terra::crop(bathy, e)
ausc <- st_crop(aus, e)

png("plots/report/swc_scatterpie.png", height = 5.5, width = 9,
    res = 900, units = "in")
ggplot() +
  geom_spatraster(data = hillshadec, alpha = 1, show.legend = F, maxcell = Inf) +
  scale_fill_gradientn(colors = pal_greys, na.value = NA) +
  new_scale_fill() +
  geom_spatraster(data = bathyc, maxcell = Inf, alpha = 0.6) +
  scale_fill_gradientn(colours = c("#061442","#014091", "#2b63b5","#6794d6"),
                       values = rescale(c(-100, -40,-20, 0)),
                       na.value = "#A0C3D8", name = "Depth")  +
  new_scale_fill() +
  geom_sf(data = ausc) +
  geom_sf(data = marine.parks, aes(fill = ZONENAME), alpha = 0.3, colour = NA) +
  scale_fill_manual(values = c("Special Purpose Zone (Mining Exclusion)" = "#368ac1",
                               "National Park Zone" = "#7bbc63"), 
                    name = "Australian Marine Parks") +
  new_scale_fill() +
  geom_point(data = habitat_park, aes(x = longitude, y = latitude),
             fill = "white", colour = "white", alpha = 0.1, size = 4, shape = 16) +
  geom_scatterpie(data = habitat_park, aes(x = longitude, y = latitude),
                  cols = c("Sessile invertebrates", "Macroalgae", "Seagrass", "Bare rocky reef", "Shelf unvegatated sediments"),
                  colour = NA, pie_scale = 0.05) +
  hab_fills +
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(114.56, 115.), 
           ylim = c(-34.13, -33.67), 
           crs = 4326) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
dev.off()

# ABROLHOS
habitat_park <- dplyr::filter(habi, amp %in% "Abrolhos")

e <- ext(112.985, 113.835,-28.366, -27.030)

hillshadec <- terra::crop(hill, e)
topoc <- terra::crop(topo, e)
bathyc <- terra::crop(bathy, e)
ausc <- st_crop(aus, e)

png("plots/report/abrolhos_scatterpie.png", height = 8, width = 7,
    res = 900, units = "in")
ggplot() +
  geom_spatraster(data = hillshadec, alpha = 1, show.legend = F, maxcell = Inf) +
  scale_fill_gradientn(colors = pal_greys, na.value = NA) +
  new_scale_fill() +
  geom_spatraster(data = bathyc, maxcell = Inf, alpha = 0.6) +
  scale_fill_gradientn(colours = c("#061442","#014091", "#2b63b5","#6794d6"),
                       values = rescale(c(-100, -40,-20, 0)),
                       na.value = "#A0C3D8", name = "Depth")  +
  new_scale_fill() +
  geom_sf(data = ausc) +
  geom_sf(data = marine.parks, aes(fill = ZONENAME), alpha = 0.3, colour = NA) +
  scale_fill_manual(values = c("Multiple Use Zone" = "#b9e6fb",
                               "National Park Zone" = "#7bbc63",
                               "Special Purpose Zone" = "#6daff4"), 
                    name = "Australian Marine Parks") +
  new_scale_fill() +
  geom_point(data = habitat_park, aes(x = longitude, y = latitude),
             fill = "white", colour = "white", alpha = 0.1, size = 3.5, shape = 16) +
  geom_scatterpie(data = habitat_park, aes(x = longitude, y = latitude),
                  cols = c("Sessile invertebrates", "Macroalgae", "Seagrass", "Bare rocky reef", "Shelf unvegatated sediments"),
                  colour = NA, pie_scale = 0.8) +
  hab_fills +
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(113.05, 113.8), 
           ylim = c(-28.26, -27.1), 
           crs = 4326) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
dev.off()

# WESTERN KANGAROO ISLAND
habitat_park <- dplyr::filter(habi, amp %in% "Western Kangaroo Island")

e <- ext(136.1596, 136.6431,-35.8705, -35.6133)

hillshadec <- terra::crop(hill, e)
topoc <- terra::crop(topo, e)
bathyc <- terra::crop(bathy, e)
ausc <- st_crop(aus, e)

png("plots/report/western-kangaroo_scatterpie.png", height = 4.5, width = 9,
    res = 900, units = "in")
ggplot() +
  geom_spatraster(data = hillshadec, alpha = 1, show.legend = F, maxcell = Inf) +
  scale_fill_gradientn(colors = pal_greys, na.value = NA) +
  new_scale_fill() +
  geom_spatraster(data = bathyc, maxcell = Inf, alpha = 0.6) +
  scale_fill_gradientn(colours = c("#061442","#014091", "#2b63b5","#6794d6"),
                       values = rescale(c(-100, -40,-20, 0)),
                       na.value = "#A0C3D8", name = "Depth")  +
  new_scale_fill() +
  geom_sf(data = ausc) +
  geom_sf(data = marine.parks, aes(fill = ZONENAME), alpha = 0.3, colour = NA) +
  scale_fill_manual(values = c("National Park Zone" = "#7bbc63",
                               "Special Purpose Zone (Mining Exclusion)" = "#368ac1"), 
                    name = "Australian Marine Parks") +
  new_scale_fill() +
  geom_point(data = habitat_park, aes(x = longitude, y = latitude),
             fill = "white", colour = "white", alpha = 0.1, size = 4, shape = 16) +
  geom_scatterpie(data = habitat_park, aes(x = longitude, y = latitude),
                  cols = c("Sessile invertebrates", "Macroalgae", "Seagrass", "Bare rocky reef", "Shelf unvegatated sediments"),
                  colour = NA, pie_scale = 1.7) +
  hab_fills +
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(136.2, 136.6), 
           ylim = c(-35.85, -35.62), 
           crs = 4326) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
dev.off()

# GEOGRAPHE
habitat_park <- dplyr::filter(habi, amp %in% "Geographe")

e <- ext(115.1117, 115.5147,-33.6413, -33.4490)

hillshadec <- terra::crop(hill, e)
topoc <- terra::crop(topo, e)
bathyc <- terra::crop(bathy, e)
ausc <- st_crop(aus, e)

png("plots/report/geographe_scatterpie.png", height = 5, width = 9,
    res = 900, units = "in")
ggplot() +
  geom_spatraster(data = hillshadec, alpha = 1, show.legend = F, maxcell = Inf) +
  scale_fill_gradientn(colors = pal_greys, na.value = NA) +
  new_scale_fill() +
  geom_spatraster(data = bathyc, maxcell = Inf, alpha = 0.6) +
  scale_fill_gradientn(colours = c("#061442","#014091", "#2b63b5","#6794d6"),
                       values = rescale(c(-100, -40,-20, 0)),
                       na.value = "#A0C3D8", name = "Depth")  +
  new_scale_fill() +
  geom_sf(data = ausc) +
  geom_sf(data = marine.parks, aes(fill = ZONENAME), alpha = 0.3, colour = NA) +
  scale_fill_manual(values = c("National Park Zone" = "#7bbc63",
                               "Special Purpose Zone (Mining Exclusion)" = "#368ac1",
                               "Multiple Use Zone" = "#b9e6fb",
                               "Habitat Protection Zone" = "#fff8a3"), 
                    name = "Australian Marine Parks") +
  new_scale_fill() +
  geom_point(data = habitat_park, aes(x = longitude, y = latitude),
             fill = "white", colour = "white", alpha = 0.1, size = 4, shape = 16) +
  geom_scatterpie(data = habitat_park, aes(x = longitude, y = latitude),
                  cols = c("Sessile invertebrates", "Macroalgae", "Seagrass", "Bare rocky reef", "Shelf unvegatated sediments"),
                  colour = NA, pie_scale = 0.5) +
  hab_fills +
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(115.14, 115.49), 
           ylim = c(-33.62, -33.46), 
           crs = 4326) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
dev.off()

# TASMAN FRACTURE
habitat_park <- dplyr::filter(habi, amp %in% "Tasman Fracture")

e <- ext(146.106, 146.561,-43.902, -43.592)

hillshadec <- terra::crop(hill, e)
topoc <- terra::crop(topo, e)
bathyc <- terra::crop(bathy, e)
ausc <- st_crop(aus, e)

png("plots/report/tasman-fracture_scatterpie.png", height = 5, width = 9,
    res = 900, units = "in")
ggplot() +
  geom_spatraster(data = hillshadec, alpha = 1, show.legend = F, maxcell = Inf) +
  scale_fill_gradientn(colors = pal_greys, na.value = NA) +
  new_scale_fill() +
  geom_spatraster(data = bathyc, maxcell = Inf, alpha = 0.6) +
  scale_fill_gradientn(colours = c("#061442","#014091", "#2b63b5","#6794d6"),
                       values = rescale(c(-100, -40,-20, 0)),
                       na.value = "#A0C3D8", name = "Depth")  +
  new_scale_fill() +
  geom_sf(data = ausc) +
  geom_sf(data = marine.parks %>% dplyr::mutate(ZONENAME = str_remove_all(ZONENAME, "Marine ")), 
          aes(fill = ZONENAME), alpha = 0.3, colour = NA) +
  scale_fill_manual(values = c("National Park Zone" = "#7bbc63",
                               "Multiple Use Zone" = "#b9e6fb"), 
                    name = "Australian Marine Parks") +
  new_scale_fill() +
  geom_point(data = habitat_park, aes(x = longitude, y = latitude),
             fill = "white", colour = "white", alpha = 0.1, size = 4, shape = 16) +
  geom_scatterpie(data = habitat_park, aes(x = longitude, y = latitude),
                  cols = c("Sessile invertebrates", "Macroalgae", "Seagrass", "Bare rocky reef", "Shelf unvegatated sediments"),
                  colour = NA, pie_scale = 0.45) +
  hab_fills +
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(146.15, 146.51), 
           ylim = c(-43.85, -43.62), 
           crs = 4326) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
dev.off()

# HUON
habitat_park <- dplyr::filter(habi, amp %in% "Huon")

e <- ext(146.810, 147.824,-44.082, -43.522)

hillshadec <- terra::crop(hill, e)
topoc <- terra::crop(topo, e)
bathyc <- terra::crop(bathy, e)
ausc <- st_crop(aus, e)

png("plots/report/huon_scatterpie.png", height = 6, width = 9,
    res = 900, units = "in")
ggplot() +
  geom_spatraster(data = hillshadec, alpha = 1, show.legend = F, maxcell = Inf) +
  scale_fill_gradientn(colors = pal_greys, na.value = NA) +
  new_scale_fill() +
  geom_spatraster(data = bathyc, maxcell = Inf, alpha = 0.6) +
  scale_fill_gradientn(colours = c("#061442","#014091", "#2b63b5","#6794d6"),
                       values = rescale(c(-100, -40,-20, 0)),
                       na.value = "#A0C3D8", name = "Depth")  +
  new_scale_fill() +
  geom_sf(data = ausc) +
  geom_sf(data = marine.parks %>% dplyr::mutate(ZONENAME = str_remove_all(ZONENAME, "Marine ")), 
          aes(fill = ZONENAME), alpha = 0.3, colour = NA) +
  scale_fill_manual(values = c("Multiple Use Zone" = "#b9e6fb"), 
                    name = "Australian Marine Parks") +
  new_scale_fill() +
  geom_point(data = habitat_park, aes(x = longitude, y = latitude),
             fill = "white", colour = "white", alpha = 0.1, size = 3.5, shape = 16) +
  geom_scatterpie(data = habitat_park, aes(x = longitude, y = latitude),
                  cols = c("Sessile invertebrates", "Macroalgae", "Seagrass", "Bare rocky reef", "Shelf unvegatated sediments"),
                  colour = NA, pie_scale = 0.45) +
  hab_fills +
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(146.86, 147.65), 
           ylim = c(-44.04, -43.55), 
           crs = 4326) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
dev.off()

# MURAT
habitat_park <- dplyr::filter(habi, amp %in% "Murat")

e <- ext(132.410, 132.905,-32.780, -32.287)

hillshadec <- terra::crop(hill, e)
topoc <- terra::crop(topo, e)
bathyc <- terra::crop(bathy, e)
ausc <- st_crop(aus, e)

png("plots/report/murat_scatterpie.png", height = 6, width = 9,
    res = 900, units = "in")
ggplot() +
  geom_spatraster(data = hillshadec, alpha = 1, show.legend = F, maxcell = Inf) +
  scale_fill_gradientn(colors = pal_greys, na.value = NA) +
  new_scale_fill() +
  geom_spatraster(data = bathyc, maxcell = Inf, alpha = 0.6) +
  scale_fill_gradientn(colours = c("#061442","#014091", "#2b63b5","#6794d6"),
                       values = rescale(c(-100, -40,-20, 0)),
                       na.value = "#A0C3D8", name = "Depth")  +
  new_scale_fill() +
  geom_sf(data = ausc) +
  geom_sf(data = marine.parks %>% dplyr::mutate(ZONENAME = str_remove_all(ZONENAME, "Marine ")), 
          aes(fill = ZONENAME), alpha = 0.3, colour = NA) +
  scale_fill_manual(values = c("National Park Zone" = "#7bbc63"), 
                    name = "Australian Marine Parks") +
  new_scale_fill() +
  geom_point(data = habitat_park, aes(x = longitude, y = latitude),
             fill = "white", colour = "white", alpha = 0.1, size = 4.5, shape = 16) +
  geom_scatterpie(data = habitat_park, aes(x = longitude, y = latitude),
                  cols = c("Sessile invertebrates", "Macroalgae", "Seagrass", "Bare rocky reef", "Shelf unvegatated sediments"),
                  colour = NA, pie_scale = 1) +
  hab_fills +
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(132.45, 132.85), 
           ylim = c(-32.73, -32.35), 
           crs = 4326) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
dev.off()

# WESTERN EYRE
habitat_park <- dplyr::filter(habi, amp %in% "Western Eyre")

e <- ext(133.518, 133.997,-33.541, -33.071)

hillshadec <- terra::crop(hill, e)
topoc <- terra::crop(topo, e)
bathyc <- terra::crop(bathy, e)
ausc <- st_crop(aus, e)

png("plots/report/western-eyre_scatterpie.png", height = 8, width = 9,
    res = 900, units = "in")
ggplot() +
  geom_spatraster(data = hillshadec, alpha = 1, show.legend = F, maxcell = Inf) +
  scale_fill_gradientn(colors = pal_greys, na.value = NA) +
  new_scale_fill() +
  geom_spatraster(data = bathyc, maxcell = Inf, alpha = 0.6) +
  scale_fill_gradientn(colours = c("#061442","#014091", "#2b63b5","#6794d6"),
                       values = rescale(c(-100, -40,-20, 0)),
                       na.value = "#A0C3D8", name = "Depth")  +
  new_scale_fill() +
  geom_sf(data = ausc) +
  geom_sf(data = marine.parks %>% dplyr::mutate(ZONENAME = str_remove_all(ZONENAME, "Marine ")), 
          aes(fill = ZONENAME), alpha = 0.3, colour = NA) +
  scale_fill_manual(values = c("National Park Zone" = "#7bbc63",
                               "Special Purpose Zone" = "#6daff4"), 
                    name = "Australian Marine Parks") +
  new_scale_fill() +
  geom_point(data = habitat_park, aes(x = longitude, y = latitude),
             fill = "white", colour = "white", alpha = 0.1, size = 8, shape = 16) +
  geom_scatterpie(data = habitat_park, aes(x = longitude, y = latitude),
                  cols = c("Sessile invertebrates", "Macroalgae", "Seagrass", "Bare rocky reef", "Shelf unvegatated sediments"),
                  colour = NA, pie_scale = 1.5) +
  hab_fills +
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(133.55, 133.95), 
           ylim = c(-33.5, -33.1), 
           crs = 4326) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
dev.off()

# INVESTIGATOR
habitat_park <- dplyr::filter(habi, amp %in% "Investigator")

e <- ext(120.694, 121.341,-34.632, -33.75)

hillshadec <- terra::crop(hill, e)
topoc <- terra::crop(topo, e)
bathyc <- terra::crop(bathy, e)
ausc <- st_crop(aus, e)

png("plots/report/investigator_scatterpie.png", height = 10, width = 9,
    res = 900, units = "in")
ggplot() +
  geom_spatraster(data = hillshadec, alpha = 1, show.legend = F, maxcell = Inf) +
  scale_fill_gradientn(colors = pal_greys, na.value = NA) +
  new_scale_fill() +
  geom_spatraster(data = bathyc, maxcell = Inf, alpha = 0.6) +
  scale_fill_gradientn(colours = c("#061442","#014091", "#2b63b5","#6794d6"),
                       values = rescale(c(-100, -40,-20, 0)),
                       na.value = "#A0C3D8", name = "Depth")  +
  new_scale_fill() +
  geom_sf(data = ausc) +
  geom_sf(data = marine.parks %>% dplyr::mutate(ZONENAME = str_remove_all(ZONENAME, "Marine ")), 
          aes(fill = ZONENAME), alpha = 0.3, colour = NA) +
  scale_fill_manual(values = c("National Park Zone" = "#7bbc63",
                               "Special Purpose Zone" = "#6daff4",
                               "Special Purpose Zone (Mining Exclusion)" = "#368ac1"), 
                    name = "Australian Marine Parks") +
  new_scale_fill() +
  geom_point(data = habitat_park, aes(x = longitude, y = latitude),
             fill = "white", colour = "white", alpha = 0.1, size = 6.5, shape = 16) +
  geom_scatterpie(data = habitat_park, aes(x = longitude, y = latitude),
                  cols = c("Sessile invertebrates", "Macroalgae", "Seagrass", "Bare rocky reef", "Shelf unvegatated sediments"),
                  colour = NA, pie_scale = 1.1) +
  hab_fills +
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(120.75, 121.25), 
           ylim = c(-34.57, -33.791), 
           crs = 4326) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
dev.off()

# EASTERN RECHERCHE
habitat_park <- dplyr::filter(habi, amp %in% "Eastern Recherche")

e <- ext(123.295, 124.646,-34.751, -33.5)

hillshadec <- terra::crop(hill, e)
topoc <- terra::crop(topo, e)
bathyc <- terra::crop(bathy, e)
ausc <- st_crop(aus, e)

png("plots/report/eastern-recherche_scatterpie.png", height = 6, width = 9,
    res = 900, units = "in")
ggplot() +
  geom_spatraster(data = hillshadec, alpha = 1, show.legend = F, maxcell = Inf) +
  scale_fill_gradientn(colors = pal_greys, na.value = NA) +
  new_scale_fill() +
  geom_spatraster(data = bathyc, maxcell = Inf, alpha = 0.6) +
  scale_fill_gradientn(colours = c("#061442","#014091", "#2b63b5","#6794d6"),
                       values = rescale(c(-100, -40,-20, 0)),
                       na.value = "#A0C3D8", name = "Depth")  +
  new_scale_fill() +
  geom_sf(data = ausc) +
  geom_sf(data = marine.parks %>% dplyr::mutate(ZONENAME = str_remove_all(ZONENAME, "Marine ")), 
          aes(fill = ZONENAME), alpha = 0.3, colour = NA) +
  scale_fill_manual(values = c("National Park Zone" = "#7bbc63",
                               "Special Purpose Zone" = "#6daff4"), 
                    name = "Australian Marine Parks") +
  new_scale_fill() +
  geom_point(data = habitat_park, aes(x = longitude, y = latitude),
             fill = "white", colour = "white", alpha = 0.1, size = 4, shape = 16) +
  geom_scatterpie(data = habitat_park, aes(x = longitude, y = latitude),
                  cols = c("Sessile invertebrates", "Macroalgae", "Seagrass", "Bare rocky reef", "Shelf unvegatated sediments"),
                  colour = NA, pie_scale = 0.65) +
  hab_fills +
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(123.35, 124.4), 
           ylim = c(-34.62, -33.594), 
           crs = 4326) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
dev.off()

# FRANKLIN
habitat_park <- dplyr::filter(habi, amp %in% "Franklin")

e <- ext(144.064, 144.775,-41.115, -40.392)

hillshadec <- terra::crop(hill, e)
topoc <- terra::crop(topo, e)
bathyc <- terra::crop(bathy, e)
ausc <- st_crop(aus, e)

png("plots/report/franklin_scatterpie.png", height = 6, width = 9,
    res = 900, units = "in")
ggplot() +
  geom_spatraster(data = hillshadec, alpha = 1, show.legend = F, maxcell = Inf) +
  scale_fill_gradientn(colors = pal_greys, na.value = NA) +
  new_scale_fill() +
  geom_spatraster(data = bathyc, maxcell = Inf, alpha = 0.6) +
  scale_fill_gradientn(colours = c("#061442","#014091", "#2b63b5","#6794d6"),
                       values = rescale(c(-100, -40,-20, 0)),
                       na.value = "#A0C3D8", name = "Depth")  +
  new_scale_fill() +
  geom_sf(data = ausc) +
  geom_sf(data = marine.parks %>% dplyr::mutate(ZONENAME = str_remove_all(ZONENAME, "Marine ")), 
          aes(fill = ZONENAME), alpha = 0.3, colour = NA) +
  scale_fill_manual(values = c("Multiple Use Zone" = "#b9e6fb"), 
                    name = "Australian Marine Parks") +
  new_scale_fill() +
  geom_point(data = habitat_park, aes(x = longitude, y = latitude),
             fill = "white", colour = "white", alpha = 0.1, size = 4, shape = 16) +
  geom_scatterpie(data = habitat_park, aes(x = longitude, y = latitude),
                  cols = c("Sessile invertebrates", "Macroalgae", "Seagrass", "Bare rocky reef", "Shelf unvegatated sediments"),
                  colour = NA, pie_scale = 1.2) +
  hab_fills +
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(144.1, 144.7), 
           ylim = c(-41.05, -40.5), 
           crs = 4326) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
dev.off()

# ZEEHAN
habitat_park <- dplyr::filter(habi, amp %in% "Zeehan")

e <- ext(143.130, 143.996,-40.021, -39.651)

hillshadec <- terra::crop(hill, e)
topoc <- terra::crop(topo, e)
bathyc <- terra::crop(bathy, e)
ausc <- st_crop(aus, e)

png("plots/report/zeehan_scatterpie.png", height = 4.2, width = 9,
    res = 900, units = "in")
ggplot() +
  geom_spatraster(data = hillshadec, alpha = 1, show.legend = F, maxcell = Inf) +
  scale_fill_gradientn(colors = pal_greys, na.value = NA) +
  new_scale_fill() +
  geom_spatraster(data = bathyc, maxcell = Inf, alpha = 0.6) +
  scale_fill_gradientn(colours = c("#061442","#014091", "#2b63b5","#6794d6"),
                       values = rescale(c(-100, -40,-20, 0)),
                       na.value = "#A0C3D8", name = "Depth")  +
  new_scale_fill() +
  geom_sf(data = ausc) +
  geom_sf(data = marine.parks %>% dplyr::mutate(ZONENAME = str_remove_all(ZONENAME, "Marine ")), 
          aes(fill = ZONENAME), alpha = 0.3, colour = NA) +
  scale_fill_manual(values = c("Multiple Use Zone" = "#b9e6fb"), 
                    name = "Australian Marine Parks") +
  new_scale_fill() +
  geom_point(data = habitat_park, aes(x = longitude, y = latitude),
             fill = "white", colour = "white", alpha = 0.1, size = 4, shape = 16) +
  geom_scatterpie(data = habitat_park, aes(x = longitude, y = latitude),
                  cols = c("Sessile invertebrates", "Macroalgae", "Seagrass", "Bare rocky reef", "Shelf unvegatated sediments"),
                  colour = NA, pie_scale = 0.55) +
  hab_fills +
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(143.18, 143.9), 
           ylim = c(-39.97, -39.77), 
           crs = 4326) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
dev.off()

# MURRAY
habitat_park <- dplyr::filter(habi, amp %in% "Murray")

e <- ext(136.535, 138.776,-37.319, -35.837)

hillshadec <- terra::crop(hill, e)
topoc <- terra::crop(topo, e)
bathyc <- terra::crop(bathy, e)
ausc <- st_crop(aus, e)

png("plots/report/murray_scatterpie.png", height = 4.5, width = 9,
    res = 900, units = "in")
ggplot() +
  geom_spatraster(data = hillshadec, alpha = 1, show.legend = F, maxcell = Inf) +
  scale_fill_gradientn(colors = pal_greys, na.value = NA) +
  new_scale_fill() +
  geom_spatraster(data = bathyc, maxcell = Inf, alpha = 0.6) +
  scale_fill_gradientn(colours = c("#061442","#014091", "#2b63b5","#6794d6"),
                       values = rescale(c(-100, -40,-20, 0)),
                       na.value = "#A0C3D8", name = "Depth")  +
  new_scale_fill() +
  geom_sf(data = ausc) +
  geom_sf(data = marine.parks %>% dplyr::mutate(ZONENAME = str_remove_all(ZONENAME, "Marine ")), 
          aes(fill = ZONENAME), alpha = 0.3, colour = NA) +
  scale_fill_manual(values = c("Multiple Use Zone" = "#b9e6fb",
                               "Special Purpose Zone" = "#6daff4",
                               "Special Purpose Zone (Mining Exclusion)" = "#368ac1"), 
                    name = "Australian Marine Parks") +
  new_scale_fill() +
  geom_point(data = habitat_park, aes(x = longitude, y = latitude),
             fill = "white", colour = "white", alpha = 0.1, size = 3, shape = 16) +
  geom_scatterpie(data = habitat_park, aes(x = longitude, y = latitude),
                  cols = c("Sessile invertebrates", "Macroalgae", "Seagrass", "Bare rocky reef", "Shelf unvegatated sediments"),
                  colour = NA, pie_scale = 0.4) +
  hab_fills +
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(136.7, 138.6), 
           ylim = c(-37.1, -35.93), 
           crs = 4326) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
dev.off()

# FREYCINET
habitat_park <- dplyr::filter(habi, amp %in% "Freycinet")

e <- ext(148.229, 148.775,-42.305, -41.575)

hillshadec <- terra::crop(hill, e)
topoc <- terra::crop(topo, e)
bathyc <- terra::crop(bathy, e)
ausc <- st_crop(aus, e)

png("plots/report/freycinet_scatterpie.png", height = 11, width = 9,
    res = 900, units = "in")
ggplot() +
  geom_spatraster(data = hillshadec, alpha = 1, show.legend = F, maxcell = Inf) +
  scale_fill_gradientn(colors = pal_greys, na.value = NA) +
  new_scale_fill() +
  geom_spatraster(data = bathyc, maxcell = Inf, alpha = 0.6) +
  scale_fill_gradientn(colours = c("#061442","#014091", "#2b63b5","#6794d6"),
                       values = rescale(c(-100, -40,-20, 0)),
                       na.value = "#A0C3D8", name = "Depth")  +
  new_scale_fill() +
  geom_sf(data = ausc) +
  geom_sf(data = marine.parks %>% dplyr::mutate(ZONENAME = str_remove_all(ZONENAME, "Marine ")), 
          aes(fill = ZONENAME), alpha = 0.3, colour = NA) +
  scale_fill_manual(values = c("Multiple Use Zone" = "#b9e6fb",
                               "National Park Zone" = "#7bbc63",
                               "Recreational Use Zone" = "#ffb36b"), 
                    name = "Australian Marine Parks") +
  new_scale_fill() +
  geom_point(data = habitat_park, aes(x = longitude, y = latitude),
             fill = "white", colour = "white", alpha = 0.1, size = 5, shape = 16) +
  geom_scatterpie(data = habitat_park, aes(x = longitude, y = latitude),
                  cols = c("Sessile invertebrates", "Macroalgae", "Seagrass", "Bare rocky reef", "Shelf unvegatated sediments"),
                  colour = NA, pie_scale = 1) +
  hab_fills +
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(148.3, 148.7), 
           ylim = c(-42.25, -41.62), 
           crs = 4326) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
dev.off()

# crop_and_scatterpie <- function(hillshade, bathymetry, topography, data, park.name) {
#   
#   habitat_park <- dplyr::filter(data, amp %in% park.name) 
#   
#   e <- st_bbox(habitat_park) %>%
#     st_as_sfc() %>%
#     st_transform(9473) %>%
#     st_buffer(dist = 1000) %>%
#     st_transform(4326)
#   st_bbox(e)
#   
#   hillshadec <- terra::crop(hillshade, e)
#   topoc <- terra::crop(topography, e)
#   bathyc <- terra::crop(bathymetry, e)
#   
#   plot_dat <- as.data.frame(habitat_park)
#   
#   ggplot() +
#     geom_spatraster(data = hillshadec, alpha = 1, show.legend = F, maxcell = Inf) +
#     scale_fill_gradientn(colors = pal_greys, na.value = NA) +
#     new_scale_fill() +
#     geom_spatraster(data = bathyc, maxcell = Inf, alpha = 0.6) +
#     scale_fill_gradientn(colours = c("#061442","#014091", "#2b63b5","#6794d6"),
#                          values = rescale(c(-250, -80,-40, 0)),
#                          na.value = "#A0C3D8", name = "Depth")  +
#     new_scale_fill() +
#     geom_spatraster(data = topoc, show.legend = F, maxcell = Inf) + 
#     scale_fill_hypso_tint_c(palette = "dem_poster",
#                             alpha = 0.6,
#                             na.value = "transparent") +
#     new_scale_fill() +
#     geom_point(data = plot_dat, aes(x = longitude, y = latitude),
#                fill = "white", alpha = 0.1, size = 4, shape = 16) +
#     geom_scatterpie(data = plot_dat, aes(x = longitude, y = latitude),
#                     cols = c("Sessile invertebrates", "Macroalgae", "Seagrass", "Rock", "Sand"),
#                     colour = NA, pie_scale = 0.5) +
#     hab_fills +
#     labs(x = "Longitude", y = "Latitude") +
#     coord_sf(xlim = c(min(plot_dat$longitude), max(plot_dat$longitude)),
#              ylim = c(min(plot_dat$latitude), max(plot_dat$latitude)),
#              crs = 4326) +
#     theme_minimal() +
#     theme(panel.grid.major = element_blank(), 
#           panel.grid.minor = element_blank())
#   
# }
# 
# # Then loop through the function for each marine park
# 
# for (i in 1:length(unique(habitat_sf$amp))) {
#   tempplot <- crop_and_scatterpie(hill, bathy, topo, habitat_sf, unique(habitat_sf$amp)[i])
#   assign(paste0(unique(habitat_sf$amp)[i], "_scatterpies"), tempplot,  envir = .GlobalEnv)
# }
# 
# # Save out for each marine park
# # Abrolhos
# png("plots/report/abrolhos_scatterpie.png", height = 9, width = 7,
#     res = 900, units = "in")
# Abrolhos_scatterpies
# dev.off()
# 
# # Apollo
# png("plots/report/apollo_scatterpie.png", height = 4, width = 9,
#     res = 900, units = "in")
# Apollo_scatterpies
# dev.off()
# 
# # Beagle
# png("plots/report/beagle_scatterpie.png", height = 6, width = 9,
#     res = 900, units = "in")
# Beagle_scatterpies
# dev.off()
# 
# # Eastern Recerhce
# png("plots/report/eastern-recherche_scatterpie.png", height = 9, width = 9,
#     res = 900, units = "in")
# `Eastern Recherche_scatterpies`
# dev.off()
# 
# # Franklin
# png("plots/report/franklin_scatterpie.png", height = 9, width = 7,
#     res = 900, units = "in")
# Franklin_scatterpies
# dev.off()
# 
# # Freycinet
# png("plots/report/freycinet_scatterpie.png", height = 9, width = 6,
#     res = 900, units = "in")
# Freycinet_scatterpies
# dev.off()
# 
# # Geographe
# png("plots/report/geographe_scatterpie.png", height = 4, width = 9,
#     res = 900, units = "in")
# Geographe_scatterpies
# dev.off()
# 
# # Huon
# png("plots/report/huon_scatterpie.png", height = 9, width = 9,
#     res = 900, units = "in")
# Huon_scatterpies
# dev.off()
# 
# # Investigator
# png("plots/report/investigator_scatterpie.png", height = 9, width = 7,
#     res = 900, units = "in")
# Investigator_scatterpies
# dev.off()
# 
# # Murat
# png("plots/report/murat_scatterpie.png", height = 9, width = 7,
#     res = 900, units = "in")
# Murat_scatterpies
# dev.off()
