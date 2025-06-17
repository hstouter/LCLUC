# Texture metric - Gray level co-occurance matrix 

install.packages("glcm")
library(glcm)
library(sp)
library(sf)
library(terra)
library(raster)
library(tidyverse)
library(ggplot2)


setwd("/Users/hstouter/Library/CloudStorage/Box-Box/NASA - Land Cover Land Use Change (2023 - 2026)/Data/LCLUC/RF_Datasets/test_may_2025/EVI_HLS/2022_test_small")

rast_stack <- stack("HLS_composite/S30_2022_median.tif")



plot(rast_stack)
plotRGB(rast_stack, r = 4, g = 3, b = 2, stretch = "linear")

plot(rast_stack)
red <- raster_stack[[4]]
blue <- raster_stack[[3]]
green <- raster_stack[[2]]
nir <- raster_stack[[9]]

rast_list <- as.list(rast_stack)

rast_list <- lapply(rast_list, raster)

glcm_s30 <- lapply(rast_list, glcm)


HLS_S30 <- c("coastal_areosol", "blue", "green", "red", "red_edge_1", "red_edge_2", "red_edge_3", "NIR_broad", "NIR_narrow", "SWIR1", "SWIR2", "water_vapor", "cirrus")


# Output directory (create if doesn't exist)
outDir <- "/Users/hstouter/Library/CloudStorage/Box-Box/NASA - Land Cover Land Use Change (2023 - 2026)/Data/LCLUC/RF_Datasets/glcm"

# Save each RasterStack with appropriate name
for (i in seq_along(glcm_s30)) {
  stack_name <- paste0("S30_2022_glcm_", HLS_S30[i], ".tif")
  out_path <- file.path(outDir, stack_name)
  
  writeRaster(glcm_s30[[i]], filename = out_path, format = "GTiff", overwrite = TRUE)
}


s30_red <- glcm(red)
s30_blue <- glcm(blue)
s30_green <- glcm(green)
s30_nir <- glcm(nir)

head(test)

test_stack <- stack(test)

plot(test)

plot(raster)
