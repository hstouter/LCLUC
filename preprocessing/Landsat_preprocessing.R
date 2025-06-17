# HS
# Landsat pre-processing
# April 14, 2025

# Install packages
library(sp)
library(sf)
library(raster)
library(terra)
library(tidyverse)
library(ggplot2)
library(purrr)
require(dplyr)
require(tibble)

setwd("/Users/hstouter/Library/CloudStorage/Box-Box/NASA - Land Cover Land Use Change (2023 - 2026)/Data/LCLUC/RF_Datasets/landsat/")
source("Landsat_preprocessing_functions.R")

# Landsat prepossing 
# Quality control (mask developed based on this tutorial: https://code.usgs.gov/eros-user-services/processing_landsat_data/decoding-the-landsat-pixel-quality-assessment-band/-/blob/main/.ipynb_checkpoints/QA_Pixel_Decoding_and_Masking_v3-checkpoint.ipynb?ref_type=heads)
# Create annual composite of median values 
# Mosaics scenes 
# Mask to study area




# Bits are ordered `76543210` and correspond to the following quality descriptions:  
#   
#   |Bit Number|      LS 8/9         |      LS 4-7        |
#   |----------|---------------------|---------------------|
#   |    1     | Aerosol |  11       |
#   |----------|---------------------|---------------------|
#   |    2     |         |  01       |
#   |----------|---------------------|---------------------|
#   |    3     |         |  00       |
#   |----------|---------------------|---------------------|
#   |    4     | Water   |  1        |
#   |----------|---------------------|---------------------|
#   |    5     |         |  0        |
#   |----------|---------------------|---------------------|
#   |    6     |  Snow/  |  1        |
#   |----------|---------------------|---------------------|
#   |    7     | Water      |
#   |----------|---------------------|---------------------|
#   |   8-9    |   Cloud Confidence  |   Cloud Confidence  |
#   |----------|---------------------|---------------------|
#   |  10-11   |    Cloud Shadow     |     Cloud Shadow    |
#   |          |     Confidence      |      Confidence     |
#   |----------|---------------------|---------------------|
#   |  12-13   | Snow/Ice Confidence | Snow/Ice Confidence |
#   |----------|---------------------|---------------------|
#   |  14-15   |  Cirrus confidence  |       Unused        |
#   |----------|---------------------|---------------------|

setwd("/Users/hstouter/Library/CloudStorage/Box-Box/NASA - Land Cover Land Use Change (2023 - 2026)/Data/LCLUC/RF_Datasets/landsat/")


ls_qa_files <- list.files("LS8_9_python_test/",pattern = "QA_PIXEL.*\\.TIF$", full.names = TRUE)


# Read in fmask as rasters
ls_qa_rasters <- lapply(ls_qa_files, rast)

plot(ls_qa_rasters[[10]])

# Set roi to match extents
roi <- st_read("/Users/hstouter/Desktop/NASA_LCLUC/RF_datasets/ynd_test_buffer.shp")
roi <- vect(roi)
roi <- project(roi, "EPSG:4326")

# Align fmask rasters
ls_qa_rasters <- align_rasters(ls_qa_rasters, roi)

# Stack fmask rasters
ls_qa_stack <- rast(ls_qa_rasters)


# Create mask 
decode_bit <- function(test, bit) {
  # Bitwise AND with 1 shifted left by 'bit' places
  mask <- app(qa_raster, function(x) bitwAnd(x, bitwShiftL(1, bit)) > 0)
  return(mask)
}


# Apply mask to raster sack
qa_mask_arr <- (qa_mask(ls_qa_stack, 'cloud') | qa_mask(ls_qa_stack, 'shadow'))





plot(qa_mask_arr[[16]])
plotRGB(s30_rasters[[16]], r = 4, g = 3, b = 2, stretch = "linear")

# Stack rasters by date

ls_files <- list.files("LS8_9_python_test/",pattern = "_B.*\\.TIF$", full.names = TRUE)

ls_raster <- lapply(ls_files, rast)

ls_raster <- align_rasters(ls_rasters, roi)


test <- ls_raster[1:32]

test_Dates <- order_by_date(test)

plot(test_Dates[[1]])

# Mask rasters with Fmask for cloud and cloud shadows



qmasked_l30_list <- qmask_mask(l30_rasters, roi, qmask_stack_l30)
qmasked_s30_list <- qmask_mask(s30_rasters, roi, qmask_stack_s30)



plot(qmask_stack_s30[[10:20]])
plotRGB(l30_rasters[[14]], r = 4, g = 3, b = 2, stretch = "linear")
plotRGB(qmasked_l30_list[[14]], r = 4, g = 3, b = 2, stretch = "linear")

plot

### HLS S30
# HLS_S30 <- c("B01","B02","B03","B04","B05","B06","B07","B08","B09","B10","B11","B12","B8A")
HLS_S30 <- c("coastal_areosol", "blue", "green", "red", "red_edge_1", "red_edge_2", "red_edge_3", "NIR_broad", "NIR_narrow", "SWIR1", "SWIR2", "water_vapor", "cirrus")

median_raster_s30 <- annual_median(qmasked_s30_list, HLS_S30)

### HLS L30
# HLS_L30 <- c("B01","B02","B03","B04","B05","B06","B07","B09","B10","B11")
HLS_L30 <- c("coastal_areosol", "blue", "green", "red", "NIR_narrow", "SWIR1", "SWIR2", "cirrus", "TIR1", "TIR2")

median_raster_l30 <- annual_median(qmasked_l30_list, HLS_L30)



plotRGB(median_raster_s30, r = 4, g = 3, b = 2, stretch = "linear")
plotRGB(median_raster_l30, r = 4, g = 3, b = 2, stretch = "linear")

plotRGB(median_raster_s30, r = 4, g = 3, b = 2, stretch = "linear", colNA = "yellow")
plotRGB(median_raster_l30, r = 4, g = 3, b = 2, stretch = "linear", colNA = "yellow")



writeRaster(median_raster_l30, "HLS_composite/L30_2022_median.tif")
writeRaster(median_raster_s30, "HLS_composite/S30_2022_median.tif")



qmasked_l30_list <- lapply(qmasked_l30_list, function(r) {
  names(r) <- HLS_L30
  r
})

qmasked_s30_list <- lapply(qmasked_s30_list, function(r) {
  names(r) <- HLS_S30
  r
})

hls_list <- c(qmasked_l30_list, qmasked_s30_list)

hls_list <- align_rasters(hls_list, roi)

median_raster <- annual_median(hls_list)

plotRGB(median_raster, r = 4, g = 3, b = 2, stretch = "linear")

plot(median_raster)

summary(median_raster)

# scale raster
median_raster <- median_raster*0.0001
plot(median_raster)

l30_median <- rast("HLS_composite/L30_2022_median.tif")
s30_median <- rast("HLS_composite/s30_2022_median.tif")



writeRaster(median_raster, "HLS_composite/HLS30_2022_median.tif")

s30 <- rast("HLS_composite/S30_2022_median_small.tif")


plotRGB(median_raster, r = "red", g = "green", b = "blue", stretch = "linear")
