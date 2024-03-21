library(tidyverse)
library(ggplot2)
library(terra)

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
  dplyr::select(campaignid, sample, longitude, latitude, amp, Macroalgae, `Shelf unvegatated sediments`, 
                `Bare rocky reef`, `Sessile invertebrates`, Seagrass) %>%
  glimpse()

habi_vect <- vect(habi, geom = c("longitude", "latitude"), crs = "epsg:4326")

bathy <- rast("data/spatial/rasters/raw bathymetry/Australian_Bathymetry_and_Topography_2023_250m_MSL_cog.tif")
names(bathy) <- "depth"

habi_plot <- cbind(habi, terra::extract(bathy, habi_vect)) %>%
  dplyr::mutate(depthclass = case_when(between(depth, -30, 0) ~ "Shallow",
                                       between(depth, -70, -30) ~ "Mesophotic",
                                       between(depth, -300, -70) ~ "Rariphotic")) %>%
  pivot_longer(cols = c("Shelf unvegatated sediments", "Bare rocky reef", "Macroalgae", "Sessile invertebrates", "Seagrass"),
               names_to = "habitat", values_to = "value") %>%
  dplyr::group_by(amp) %>%
  dplyr::mutate(habitat_percentage = value / sum(value) * 100,
                depthclass = factor(depthclass, levels = c("Shallow", "Mesophotic", "Rariphotic")),
                colour = case_when(habitat %in% "Sessile invertebrates" ~ "plum",
                                   habitat %in% "Macroalgae" ~ "darkgoldenrod4",
                                   habitat %in% "Seagrass" ~ "forestgreen",
                                   habitat %in% "Bare rocky reef" ~ "grey40",
                                   habitat %in% "Shelf unvegatated sediments" ~ "wheat")) %>%
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
  dat <- dplyr::filter(habi_plot, amp %in% unique(habi_plot$amp)[i] & value > 0)
  cols <- rlang::set_names(dat$colour, dat$habitat)
  print(ggplot(dat, 
         aes(fill = habitat, y = habitat_percentage, x = depthclass)) + 
    hab_fills +
    # scale_fill_manual(values = cols, name = "Habitat") +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Depth class", y = "% cover", title = unique(habi_plot$amp)[i]) +
    theme_classic())
  dev.off()
}
