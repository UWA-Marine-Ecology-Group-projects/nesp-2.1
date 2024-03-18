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
  dplyr::rename(Sand = broad.unconsolidated,
                Rock = broad.consolidated,
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
  glimpse()

hab_fills <- scale_fill_manual(values = c("Sessile invertebrates" = "plum",
                                          "Macroalgae" = "darkgoldenrod4",
                                          "Seagrass" = "forestgreen",
                                          "Rock" = "grey40",
                                          "Sand" = "wheat"), 
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

# APOLLO
habitat_park <- dplyr::filter(habi, amp %in% "Apollo")
min(habitat_park$longitude)
max(habitat_park$longitude)
min(habitat_park$latitude)
max(habitat_park$latitude)

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
                  cols = c("Sessile invertebrates", "Macroalgae", "Seagrass", "Rock", "Sand"),
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









crop_and_scatterpie <- function(hillshade, bathymetry, topography, data, park.name) {
  
  habitat_park <- dplyr::filter(data, amp %in% park.name) 
  
  e <- st_bbox(habitat_park) %>%
    st_as_sfc() %>%
    st_transform(9473) %>%
    st_buffer(dist = 1000) %>%
    st_transform(4326)
  st_bbox(e)
  
  hillshadec <- terra::crop(hillshade, e)
  topoc <- terra::crop(topography, e)
  bathyc <- terra::crop(bathymetry, e)
  
  plot_dat <- as.data.frame(habitat_park)
  
  ggplot() +
    geom_spatraster(data = hillshadec, alpha = 1, show.legend = F, maxcell = Inf) +
    scale_fill_gradientn(colors = pal_greys, na.value = NA) +
    new_scale_fill() +
    geom_spatraster(data = bathyc, maxcell = Inf, alpha = 0.6) +
    scale_fill_gradientn(colours = c("#061442","#014091", "#2b63b5","#6794d6"),
                         values = rescale(c(-250, -80,-40, 0)),
                         na.value = "#A0C3D8", name = "Depth")  +
    new_scale_fill() +
    geom_spatraster(data = topoc, show.legend = F, maxcell = Inf) + 
    scale_fill_hypso_tint_c(palette = "dem_poster",
                            alpha = 0.6,
                            na.value = "transparent") +
    new_scale_fill() +
    geom_point(data = plot_dat, aes(x = longitude, y = latitude),
               fill = "white", alpha = 0.1, size = 4, shape = 16) +
    geom_scatterpie(data = plot_dat, aes(x = longitude, y = latitude),
                    cols = c("Sessile invertebrates", "Macroalgae", "Seagrass", "Rock", "Sand"),
                    colour = NA, pie_scale = 0.5) +
    hab_fills +
    labs(x = "Longitude", y = "Latitude") +
    coord_sf(xlim = c(min(plot_dat$longitude), max(plot_dat$longitude)),
             ylim = c(min(plot_dat$latitude), max(plot_dat$latitude)),
             crs = 4326) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())
  
}

# Then loop through the function for each marine park

for (i in 1:length(unique(habitat_sf$amp))) {
  tempplot <- crop_and_scatterpie(hill, bathy, topo, habitat_sf, unique(habitat_sf$amp)[i])
  assign(paste0(unique(habitat_sf$amp)[i], "_scatterpies"), tempplot,  envir = .GlobalEnv)
}

# Save out for each marine park
# Abrolhos
png("plots/report/abrolhos_scatterpie.png", height = 9, width = 7,
    res = 900, units = "in")
Abrolhos_scatterpies
dev.off()

# Apollo
png("plots/report/apollo_scatterpie.png", height = 4, width = 9,
    res = 900, units = "in")
Apollo_scatterpies
dev.off()

# Beagle
png("plots/report/beagle_scatterpie.png", height = 6, width = 9,
    res = 900, units = "in")
Beagle_scatterpies
dev.off()

# Eastern Recerhce
png("plots/report/eastern-recherche_scatterpie.png", height = 9, width = 9,
    res = 900, units = "in")
`Eastern Recherche_scatterpies`
dev.off()

# Franklin
png("plots/report/franklin_scatterpie.png", height = 9, width = 7,
    res = 900, units = "in")
Franklin_scatterpies
dev.off()

# Freycinet
png("plots/report/freycinet_scatterpie.png", height = 9, width = 6,
    res = 900, units = "in")
Freycinet_scatterpies
dev.off()

# Geographe
png("plots/report/geographe_scatterpie.png", height = 4, width = 9,
    res = 900, units = "in")
Geographe_scatterpies
dev.off()

# Huon
png("plots/report/huon_scatterpie.png", height = 9, width = 9,
    res = 900, units = "in")
Huon_scatterpies
dev.off()

# Investigator
png("plots/report/investigator_scatterpie.png", height = 9, width = 7,
    res = 900, units = "in")
Investigator_scatterpies
dev.off()

# Murat
png("plots/report/murat_scatterpie.png", height = 9, width = 7,
    res = 900, units = "in")
Murat_scatterpies
dev.off()
