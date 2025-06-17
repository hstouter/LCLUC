# Hannah Stouter
# May 23, 2025

# Process sentinel RTC data 

library(sp)
library(sf)
library(terra)
library(raster)
library(tidyverse)
library(stringr)


# Unzip and pull VV VH tif files 
# Create VV/VH band 
# Create an annual composite (Oct 1 - Sept 30) using the median value for VV, VH VV/VH
# Mosaic scene 
# Mask to study area 
# Stack VV, VH, VV/HH

setwd("/Users/hstouter/Library/CloudStorage/Box-Box/NASA - Land Cover Land Use Change (2023 - 2026)/Data/LCLUC/Sentinel_1")

zip_dir <- ("/Users/hstouter/Library/CloudStorage/Box-Box/NASA - Land Cover Land Use Change (2023 - 2026)/Data/LCLUC/Sentinel_1/sigma_2022_test_zip/")

roi <- st_read("/Users/hstouter/Desktop/NASA_LCLUC/RF_datasets/ynd_test_buffer.shp")
plot(roi)

zip_files <- list.files(zip_dir, pattern = "\\.zip$", full.names = TRUE)

# Initialize empty lists for VV and VH files
vv_bands <- list()
vh_bands <- list()

# Loop through zip files
for (zip_file in zip_files) {
  
  # Unzip to a temporary directory
  temp_unzip_dir <- tempfile(pattern = "unzipped_")
  dir.create(temp_unzip_dir)
  unzip(zip_file, exdir = temp_unzip_dir)
  
  # Find all .tif files
  tif_files <- list.files(temp_unzip_dir, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)
  
  # Filter for VV and VH files
  vv_files <- tif_files[str_detect(tif_files, "VV")]
  vh_files <- tif_files[str_detect(tif_files, "VH")]
  
  # Load rasters and append to lists
  vv_bands <- c(vv_bands, lapply(vv_files, rast))
  vh_bands <- c(vh_bands, lapply(vh_files, rast))
}


# Create VV/VH band
vvvh_bands <- mapply(function(vv, vh) {vv / vh},
                       vv_bands, vh_bands, SIMPLIFY = FALSE)

# set all bands to the same extent --> roi 
roi <- vect(roi)
roi <- project(roi, "EPSG:4326")


# Mask rasters to study area
align_rasters <- function(band_list, roi){
  
  # Update and check raster roi
  rasters <- lapply(band_list, function(r) {
    if (!identical(crs(r), "EPSG:4326")) {
      r <- project(r, "EPSG:4326")
    }
    return(r)
  })
  
  # Create template raster
  template_raster <- rast(
    extent = ext(roi),
    resolution = res(rasters[[1]]),
    crs = "EPSG:4326"
  )
  
  # Align all rasters in the list
  aligned_rasters <- lapply(rasters, function(r) {
    r_proj <- project(r, "EPSG:4326")                    # Ensure WGS84
    r_crop <- crop(r_proj, roi)                          # Crop to ROI
    r_mask <- mask(r_crop, roi)                          # Mask by ROI
    r_resampled <- resample(r_mask, template_raster)     # Align resolution + extent
  })
  
  # stack aligned rasters
  stack <- rast(aligned_rasters)
  
  # Create median annual raster 
  annual_raster <- median(stack, na.rm = TRUE)
}

# vv_test <- vv_rasters[1:10]
# vh_test <- vh_rasters[1:10]
# vvvh_test <- vvvh_rasters[1:10]

# Align rasters
# vv_stack <- align_rasters(vv_bands, roi)
vh_stack <- align_rasters(vh_bands, roi)

vvvh_stack <- align_rasters(vvvh_bands, roi)

# Save 
writeRaster(vv_stack, "S1_VV_2022_small_test.tif")
writeRaster(vh_stack, "S1_VH_2022_small_test.tif")
writeRaster(vvvh_stack, "S1_VV_VH_2022_small_test.tif")

# Create annual stack and update name
vv_stack <- rast("S1_VV_2022_small_test.tif")

s1_2022 <- c(vv_stack, vh_stack, vvvh_stack)
names(s1_2022) <- c("VV", "VH", "VV_VH")

# Save raster will all bands 
writeRaster(s1_2022, "s1_2022_test.tif")



