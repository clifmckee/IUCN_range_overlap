##########################
# R version 4.0.3 "Bunny-Wunnies Freak Out"
# Works: 2021-02-10
##########################

# Set working directory
setwd("~/Dropbox/Old code/Pteropus distributions")

# Necessary packages
library(sp)
library(rgeos)
library(maps)
library(mapdata)
library(maptools)
library(geosphere)
library(ggplot2)
library(viridis)
library(cowplot)
library(ggthemes)
library(reshape2)
library(RColorBrewer)
library(rgdal)
library(dplyr)
library(tidyr)
library(stringr)
library(mapproj)
library(broom)
library(raster)
library(rmapshaper)

# Define functions
"%ni%" <- Negate("%in%")

###################
### Import data ###
###################

# Read in the shape file
load(file="./mammterr.RData")

# Data for world map
world_map <- map_data("world")

# Create a list of species names to filter mammterr
allnames <- unique(str_subset(mammterr$binomial, "Pteropus"))

# # Alternative code - just a list of species names
# allnames <- c("Pteropus giganteus", "Pteropus lylei", "Pteropus vampyrus", "Pteropus intermedius")

# Filter mammterr down to just binomial names
all.binomial <- mammterr$binomial

# Check to see if allnames are in the binomial names
unique.allnames <- unique(allnames)

# Which rows of the data are the species I care about
keep <- list()
for(i in 1:length(unique.allnames)){
  keep[[i]] <- which(all.binomial == unique.allnames[i])
}
x <- keep[[1]]
for(i in 2:length(unique.allnames)){
  x <- c(x, keep[[i]])
}
keep.species <- data.frame(x = x, species = mammterr[x,]$binomial)
myspecies.distr <- mammterr[x,]

###########################################################
### Species distribution plots - single species example ###
###########################################################

# Choose one species and format into data frame
# tol sets the threshold for the smallest polygon size, but increases computation time
sp.range <- tidy(gSimplify(subset(myspecies.distr, binomial=="Pteropus hypomelanus"), tol = 0.005))

# Plot map of single species
ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
               color = NA,
               fill = "grey90") +
  geom_polygon(data = sp.range, aes(x = long, y = lat, group = group),
               color = NA,
               fill = "#CC79A7") +
  theme_map(base_size = 16) +
  coord_cartesian(xlim = c(0, 180), ylim = c(-38, 38)) +
  theme(legend.text = element_text(size = 12)) +
  ggtitle("Pteropus hypomelanus")
# Output plot to file
ggsave("./Pteropus_species_range_single.png",
       device = "png", dpi = 300, width = 10, height = 5, units = "in")

###########################################################
### Species distribution plots - all overlapping ranges ###
###########################################################

# Format all species into data frame
# tol sets the threshold for the smallest polygon size, but increases computation time
myspecies.distr.df <- tidy(gSimplify(myspecies.distr, tol = 0.005))

# Plot map of all species ranges with transparency
ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
               color = NA,
               fill = "grey90") +
  geom_polygon(data = myspecies.distr.df, aes(x = long, y = lat, group = group),
               color = NA,
               fill = "#009E73",
               alpha = 0.2) +
  theme_map(base_size = 16) +
  coord_cartesian(xlim = c(0, 180), ylim = c(-38, 38)) +
  theme(legend.text = element_text(size = 12))
# Output plot to file
ggsave("./Pteropus_species_range_overlap_transparent.png",
       device = "png", dpi = 300, width = 10, height = 5, units = "in")

# Plot map of all species ranges without transparency
ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
               color = NA,
               fill = "grey90") +
  geom_polygon(data = myspecies.distr.df, aes(x = long, y = lat, group = group),
               color = NA,
               fill = "#009E73") +
  theme_map(base_size = 16) +
  coord_cartesian(xlim = c(0, 180), ylim = c(-38, 38)) +
  theme(legend.text = element_text(size = 12))
# Output plot to file
ggsave("./Pteropus_species_range_overlap_notransparent.png",
       device = "png", dpi = 300, width = 10, height = 5, units = "in")

#################################################################
### Species distribution plots - number of overlapping ranges ###
#################################################################

# Sample points across the species distributions on a regular grid
# n increases resolution, but also computation time
spp.points <- spsample(myspecies.distr, n = 50000, type = "regular")

# Summarize the species distributions that overlap the chosen points
spp.pointsInPolygons <- sp::over(x = spp.points, y = myspecies.distr, returnList = TRUE)

# Count the number of intersections
spp.counting <- lapply(spp.pointsInPolygons, FUN = function(x) nrow(x))
spp.over.df <- data.frame("point" = rownames(t(do.call("cbind", spp.counting))), 
                          "count" = t(do.call("cbind", spp.counting)), 
                          "polygon" = paste(spp.pointsInPolygons))

# Summarize counts in a data frame
spp.points.df <- as.data.frame(spp.points)
spp.points.df$count <- spp.over.df$count

# Combine world map data and species range data into one object
sf_data <- list(world = world_map, overlap = spp.points.df)

# Plot map of species range overlap counts for all species
ggplot() +
  geom_polygon(data = sf_data$world, aes(x = long, y = lat, group = group),
               color = NA,
               fill = "grey90") +
  geom_tile(data = sf_data$overlap, aes(x = x1, y = x2, fill = count)) +
  scale_fill_viridis(option = "D", name = "Species") +
  theme_map(base_size = 16) +
  coord_cartesian(xlim = c(0, 180), ylim = c(-38, 38)) +
  theme(legend.text = element_text(size = 12))
# Output plot to file
ggsave("./Pteropus_species_range_overlap_counts.png",
       device = "png", dpi = 300, width = 10, height = 5, units = "in")

###################
### End of code ###
###################