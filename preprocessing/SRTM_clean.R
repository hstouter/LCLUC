# Hannah Stouter
# January 28, 2025


# SRTM data

library(raster)
library(terra)
library(dplyr)
library(sf)
library(ggplot2)
library(sp)



getwd()
setwd("/Users/hstouter/Desktop/NASA_LCLUC/RF_datasets/")

# read in data
srtm_list <- list.files("STRM_DEM", pattern = "\\.tif$", full.names = TRUE)

# convert to raster
srtm <- lapply(srtm_list, rast)

# mosaic rasters together 
srtm <- do.call(mosaic, srtm)

# clip to study area
roi <- st_read("roi/small_buffer/small_buffer.shp")

# check and crs
srtm_crs <- st_crs(crs(srtm)) 
roi <- st_transform(roi, crs = srtm_crs$epsg) 

# crop srtm to the study area
srtm <- crop(srtm, roi)

# export saved raster 
head(srtm)

# standardize names
names(srtm) <- c("elevation")

# Topographic measurments 

# Slope, or steepness of each cell 
slope <- terrain(srtm, "slope", unit = "degrees", neighbors = 8)

# Aspect, direction of slope (in compass degrees)
aspect <- terrain(srtm, "aspect", unit = "degrees", neighbors = 8)


# Roughness
roughness <- terrain(srtm, "roughness", neighbors = 8)

# Topographic roughness index (TRI), or difference in elevation between neighboring pixels
tri <- terrain(srtm, "TRI", neighbors = 8)
 

# convert to raster
srtm <- raster(srtm)
slope <- raster(slope)
aspect <- raster(aspect)
roughness <- raster(roughness)
tri <- raster(tri)

# create stacked raster 
dem <- stack(srtm, slope, aspect, roughness, tri)

writeRaster(dem, "dem.tif", format="GTiff", overwrite=TRUE)





