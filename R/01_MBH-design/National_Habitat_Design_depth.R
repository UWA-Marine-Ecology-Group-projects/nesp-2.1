##National shelf habitat sampling design based on GA250 bathy with 70:30 effort in 0-120m and 120-250m respectively. 
##Rationale being that we have mud/silt starting at this point so need less sampling effort
##Prepared by jacquomo.monk@utas.edu.au
##1/09/2022

# Clear memory
rm(list=ls()) 


library(MBHdesign)
library(sp)
library(raster)
library(rgdal)
library(fields)
library(rasterVis)
library(tidyverse)
library(googledrive)
library(terra)
library(Rcpp)
library(SpaDES)
library(dplyr)

set.seed( 66)

##########################################Read in bathy raster##############################
# depth <- drive_download("shelf_depth.asc",                                      # Run once
#                         path = "data/spatial/rasters/raw bathymetry/shelf_depth.asc") 
depth <- read.asciigrid("data/spatial/rasters/raw bathymetry/shelf_depth_south.asc") # change to #change to regions ("_west.asc" or"_northeast.asc"  or "_southeast.asc" ) if fails to run and increasing nSampsToConsider doesn't help
# depth <- read.asciigrid("shelf_depth_south.asc")
##########################################Convert depth into a data.frame and generate strata##############################
##convert to df
depth.mat           <- as.matrix( depth)
depth               <- as.data.frame(
  cbind(coordinates( depth), as.numeric( depth.mat)))
colnames(depth)     <- c("Easting", "Northing", "Depth")
depth               <- depth[order( depth$Northing,
                                    depth$Easting),]
depth[is.na(depth)] <- -9999
#3Strata
# depth_class <- depth %>%
#             mutate(bathClass = cut( depth$Depth, breaks = c(-99999,0,-120,-251),
#                                       labels = c("outside","shallow","deep")))%>%
#             glimpse()

depth_class <- depth %>%
  dplyr::mutate(bathClass = cut( depth$Depth, breaks = c(-99999,-251,-120,0),
                                 labels = c("outside","deep","shallow")))

# depth_class <- depth %>%
#   mutate(bathClass = cut( depth$Depth, breaks = c(-99999,-251,0),
#                           labels = c("outside","inside")))%>%
#   glimpse()

##Check we don't have any NAs
summary(depth_class$bathClass)

# force the breaks so R doesn't use pretty
tabl <- table(depth_class$bathClass)

# levels(depth_class$bathClass)
# 
## Set number of samples 
# n.shallow <- n * 0.8
# n.deep <- n * 0.2
# n.out <- 0 # Remove this?
# 
# #Find the inclusion probability for each stratum
# loc <- list()
# loc$inclProbs <- 1/tabl
# #Matching up locations to probabilties
# loc$iprob <- rep( NA, length( depth_class$bathClass))
# loc$iprob[depth_class$bathClass %in% "shallow"] <- loc$inclProbs[3] * n.shallow # weighting by number of samples in area
# loc$iprob[depth_class$bathClass %in% "deep"]    <- loc$inclProbs[2] * n.deep    # Half as many in the deep
# loc$iprob[depth_class$bathClass %in% "outside"] <- loc$inclProbs[1] * n.out     
# 
# # design <- data.frame(Easting = depth_class$Easting, Northing=depth_class$Northing,
# #                       bathClass = depth_class$bathClass, inclProb=loc$iprob)  # 0.000000e+00, 3.108981e-05, 8.853464e-07 outside, deep, shallow
# 
# test2 <- as.data.frame(loc$inclProbs) %>%
#   dplyr::mutate(bathClass = as.character(Var1))
# 
# design <- depth_class %>%
#   inner_join(test2, by = "bathClass")
# 
# test <- design %>%
#   dplyr::group_by(bathClass) %>%
#   dplyr::summarise(mean = mean(Freq)) %>%
#   glimpse()

##define the inclusion probs for each class
ncells          <- sum(!is.na(depth_class$bathClass))

combined.levels <- data.frame( bathClass = c("outside","shallow","deep"),
                               rel.import = c(0,
                                              0.3,
                                              0.7))

# combined.levels <- data.frame( bathClass = c("outside","inside"),
#                                rel.import = c(0,
#                                               1.0))

combined.levels$SampProb   <- tabl / ncells
combined.levels$SampProb   <- as.numeric(combined.levels$SampProb)
combined.levels$incl.probs <- combined.levels$rel.import / combined.levels$SampProb
combined.levels            <- replace(combined.levels, is.na(combined.levels), 0) #set outside data 0
combined.levels$incl.probs <- combined.levels$incl.probs / sum( combined.levels$incl.probs)
#write.csv(combined.levels,"strata.csv") #you many want to write this out for later

##Define class a character for depth
combined.levels$bathClass <- as.character( combined.levels$bathClass)
depth_class$bathClass <- as.character( depth_class$bathClass)


##Create design by joining combined levels
design <- depth_class %>%
  inner_join(combined.levels, by = "bathClass")

# head(design, 10)

##check things have worked
# min(design$incl.probs)
# max(design$incl.probs)

##convert incl. probs into a raster
# inclProb_raster <- rasterFromXYZ(xyz = design[,c("Easting","Northing","Freq")])
inclProb_raster <- rasterFromXYZ(xyz = design[,c("Easting","Northing","incl.probs")])
plot(inclProb_raster)
#inclProb25_raster <- aggregate( inclProb_raster, fact=5, fun= max)# only need to run in massive datasets
# plot(inclProb_raster)
#Always a good idea to write out this raster incase things crash and you can re-read this in negating rerunning the above bit of code
# writeRaster(inclProb_raster,"data/spatial/rasters/raw bathymetry/National_Sampling_Master_1M_inclProb_raster_rug.tif", overwrite = T)
writeRaster(inclProb_raster,"data/spatial/rasters/raw bathymetry/National_Sampling_Master_inclProb_raster.tif", overwrite = T)
# inclProb_raster <- raster("data/spatial/rasters/raw bathymetry/National_Sampling_Master_inclProb_raster.tif") # Read from raster
rm(list=setdiff(ls(), "inclProb_raster")) # Remove all items apart from the inclusion probs - run faster
# #########################################Set up the sampling design##############################
# ##number of drops----
# 
# 
# inclProb_split <- splitRaster(inclProb_raster, nx = 8, ny = 4)
# inclProb_test <- inclProb_split[[7]] # Need to run for eastern states [[2]]
# plot(inclProb_test)
# # If this is still too unbalanced - re run depth as a raster
# # Split into 2 tiles and run MBH on this
# 
# # n <- 100 # set to what you want. For national model I think we'll need 10M points to be useful (enough samples) at park scales
# 
# # 5 mil probably optimal - but won't run with that density
# # Subset raster to smaller size and run with the same relative density
# 
# ##Run sampling design with quasiSamp.raster- faster than qausiSamp

n <- 200000

samp <- quasiSamp.raster(n = n, 
                         inclusion.probs = inclProb_raster, 
                         randStartType = 2, 
                         nSampsToConsider = n*100) # may need to up this value if not running at a national scale
plot(inclProb_raster)
points( samp[,c("x","y")], pch=20, cex=0.01, col = "red")

## assign sampling order
samp$DropC <- 1:nrow(samp)

## Write out sampling file 
write.csv(samp,"data/mbh-design/National_Sampling_Master.csv") # write out the each region

test <- read.csv("data/mbh-design/National_Sampling_Master.csv") %>%
  glimpse()
deep <- test %>%
  dplyr::filter(inclusion.probabilities == 0.3744583)
shallow <- test %>%
  dplyr::filter(inclusion.probabilities == 1.0000000)

nrow(shallow)/200000
nrow(deep)/200000

## now we have sampling design. worth checking in arcmap or similar that you have enough sampling density to be useful for each AMP that we intend to visit

# samp <- quasiSamp.raster(n = n, 
#                          inclusion.probs = !is.na(inclProb_raster), 
#                          randStartType = 2, 
#                          nSampsToConsider = n*500)
# points( samp[,c("x","y")], pch=20, cex=0.01, col = "red")
