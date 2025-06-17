# Merge GEE labels and save to SHP
# HS
# May 10,2024 


# set working directory
setwd("/Users/hstouter/Desktop/NASA_LCLUC/RF_datasets/all_labels/")

library(sf)
library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)

source("RF_functions.R") # save this .R file in the same place as this file

# Load in shp files from folder 
agriculture <- list.files("agriculture", pattern = "\\.shp$", full.names = TRUE)
bare <- list.files("bare", pattern = "\\.shp$", full.names = TRUE)
built <- list.files("built", pattern = "\\.shp$", full.names = TRUE)
forest <- list.files("forest", pattern = "\\.shp$", full.names = TRUE)
oil_palm <- list.files("oil_palm", pattern = "\\.shp$", full.names = TRUE)
water <- list.files("water", pattern = "\\.shp$", full.names = TRUE)
mining <- list.files("mining", pattern = "\\.shp$", full.names = TRUE)

# clean labels 
agriculture <- clean_labels(agriculture, class = "agriculture")
bare <- clean_labels(bare, class = "bare")
built <- clean_labels(built, class = "built")
forest <- clean_labels(forest, class = "forest")
oil_palm <- clean_labels(oil_palm, class = "oil_palm")
water <- clean_labels(water, class = "water")
mining <- clean_labels(mining, class = "mining")

# merge labels 
all_labels <- rbind(agriculture, bare, built, forest, oil_palm, water, mining)
plot(all_labels)


# Pull coordinates and create lat and long columns
coords <- st_coordinates(all_labels)
all_labels$long <- coords[, 1]
all_labels$lat <- coords[, 2]

#plot labels 
ggplot()+
  geom_point(aes( x = all_labels$long, y = all_labels$lat, color = all_labels$class))+
  theme_classic()

# export as shp 
st_write(all_labels, "all_labels_mining.shp", driver = "ESRI Shapefile") #change file name


all_labels_csv <- all_labels %>% select(-x)
# export as csv
write.csv(all_labels, "all_labels.csv") #change file names 
