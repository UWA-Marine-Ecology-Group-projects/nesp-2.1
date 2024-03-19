library(tidyverse)
library(ggplot2)
library(terra)

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
  dplyr::select(campaignid, sample, longitude, latitude, amp, Macroalgae, Sand, Rock, `Sessile invertebrates`, Seagrass) %>%
  glimpse()

habi_vect <- vect(habi, geom = c("longitude", "latitude"), crs = "epsg:4326")

bathy <- rast("data/spatial/rasters/raw bathymetry/Australian_Bathymetry_and_Topography_2023_250m_MSL_cog.tif")
names(bathy) <- "depth"

habi_plot <- cbind(habi, terra::extract(bathy, habi_vect)) %>%
  dplyr::mutate(depthclass = case_when(between(depth, -30, 0) ~ "Shallow",
                                       between(depth, -70, -30) ~ "Mesophotic",
                                       between(depth, -300, -70) ~ "Rariphotic")) %>%
  pivot_longer(cols = c("Sand", "Rock", "Macroalgae", "Sessile invertebrates", "Seagrass"),
               names_to = "habitat", values_to = "value") %>%
  dplyr::mutate(habitat = case_when(habitat %in% "Sand" ~ "Shelf unvegatated sediments",
                                    habitat %in% "Rock" ~ "Bare rocky reef",
                                    .default = habitat)) %>%
  dplyr::group_by(amp) %>%
  dplyr::mutate(habitat_percentage = value / sum(value)) %>%
  glimpse()

hab_fills <- scale_fill_manual(values = c("Sessile invertebrates" = "plum",
                                          "Macroalgae" = "darkgoldenrod4",
                                          "Seagrass" = "forestgreen",
                                          "Bare rocky reef" = "grey40",
                                          "Shelf unvegatated sediments" = "wheat"), 
                               name = "Habitat")

for (i in 1:length(unique(habi_plot$amp))) {
  png(filename = paste0("plots/report/", unique(habi_plot$amp)[i], "_stacked-bar.png"),
      height = 6, width = 9, res = 300, units = "in")
  print(ggplot(dplyr::filter(habi_plot, amp %in% unique(habi_plot$amp)[i]), 
         aes(fill = habitat, y = habitat_percentage, x = depthclass)) + 
    hab_fills +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Depth class", y = "% cover", title = unique(habi_plot$amp)[i]) +
    theme_classic())
  dev.off()
}
