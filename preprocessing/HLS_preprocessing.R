# HS
# April 14, 2025

setwd("/Users/hstouter/Desktop/NASA_LCLUC/RF_datasets/AppEEARS_download/")
source("HLS_preprocessing_functions.R")

# HLS prepossing 
    # Quality control (mask developed based on this tutorial: https://github.com/nasa/HLS-Data-Resources/blob/main/r/HLS_Tutorial.Rmd)
    # Create annual composite of median values 
    # Mosaics scenes 
    # Mask to study area

# Bits are ordered `76543210` and correspond to the following quality descriptions:  
#   
#   |Bit Number|Mask Name|Bit Value|Description|
#   |----------|---------|---------|-----------|
#   |   7-6    | Aerosol |  11     |   High    |
#   |          | Level   |  10     |   Medium  |
#   |          |         |  01     |   Low     |
#   |          |         |  00     |   Clear   |
#   |----------|---------|---------|-----------|
#   |    5     | Water   |  1      |    Yes    |
#   |          |         |  0      |    No     |
#   |----------|---------|---------|-----------|
#   |    4     |  Snow/  |  1      |    Yes    |
#   |          |  Ice    |  0      |    No     |
#   |----------|---------|---------|-----------|
#   |    3     | Cloud   |  1      |    Yes    |
#   |          | Shadow  |  0      |    No     |
#   |----------|---------|---------|-----------|
#   |    2     | Cloud/  |  1      |    Yes    |
#   |          | Shadow  |  0      |    No     |
#   |          | Adjacent|         |           |
#   |----------|---------|---------|-----------|
#   |    1     | Cloud   |  1      |    Yes    |
#   |          |         |  0      |    No     |
#   |----------|---------|---------|-----------|
#   |    0     | Cirrus  |Reserved |    NA     |
#   |----------|---------|---------|-----------|


fmask <- rast("output_test/HLSS30.020_2022335_to_2022365/HLSS30.020_Fmask_doy2022346_aid0001_32N.tif")

fmask_files <- list.files("output_test/HLSS30.020_2022335_to_2022365/", pattern = "Fmask", full.names = TRUE)

fmask_stack <- rast(fmask_files)

# Test fmask
plot(fmask_stack)


# Select bits to mask 
selected_bit_nums <- c(1,2,3)

qmask_stack <- build_mask(fmask_stack, selected_bit_nums)
plot(qmask)


# Load in data by date

file_dir <- "output_test/HLSS30.020_2022335_to_2022365/"

# List all raster files in the folder that match the Landsat bands (e.g., .tif)
raster_files <- list.files(file_dir, pattern = "B", full.names = TRUE)

# Function to extract the date from the file name (assuming the date is part of the filename)
# Adjust the regex or date extraction method based on your file naming convention
extract_date <- function(filename) {
  # Extract the year and day of year (DOY) from the filename, e.g., doy2022346 -> 2022-346
  date_str <- sub(".*doy(\\d{4})(\\d{3}).*", "\\1-\\2", filename)
  return(date_str)  # returns a "YYYY-DDD" format
}




# Add the extracted date to the file names
raster_files_with_dates <- tibble(
  file = raster_files,
  date = sapply(raster_files, extract_date)
)

raster_files_with_dates <- raster_files_with_dates %>%
  arrange(date)

# Group by date and stack the bands for each date
stacked_rasters_by_date <- raster_files_with_dates %>%
  group_by(date) %>%
  group_map(~ {
    # Load all rasters for the current date
    rasters_for_date <- rast(.x$file)
    # Return a stack of these rasters
    return(rasters_for_date)
  })


stacked_rasters_by_date
plot(stacked_rasters_by_date[[1]])


# Group by date and stack the bands for each date
stacked_rasters_by_date <- raster_files_with_dates %>%
  group_by(date) %>%
  group_map(~ {
    # Load all rasters for the current date
    rasters_for_date <- rast(.x$file)
    # Return a stack of these rasters
    return(rasters_for_date)
  })

# Check the stacked rasters by date
stacked_rasters_by_date

# Example: Plot the stack for the first date
plot(stacked_rasters_by_date[[1]])


# Apply mask to layer stack 

raster_masked <- mapply(function(x, y) {
  mask(x, y, maskvalue = TRUE, updatevalue = NA)
}, stacked_rasters_by_date, qmask_stack, SIMPLIFY = FALSE)


plot(raster_masked[[4]])

raster_masked <- mask(stacked_rasters_by_date[[1]], qmask_stack[[1]], maskvalue = TRUE, updatevalue = NA)
plot(raster_masked)

# Apply the mask to all bands in the raster stack list 
masked_raster_list <- lapply(1:length(stacked_rasters_by_date), function(i) {
  # Apply the mask for each element in the list
  mask(stacked_rasters_by_date[[i]], qmask_stack[[i]], maskvalue = TRUE, updatevalue = NA)
})


# Create annual median composit
stacked_rasters <- rast(masked_raster_list)


composite_median <- app(stacked_rasters, fun = median, na.rm = TRUE)

plot(composite_median)

# Initialize an empty list to store the median of each band
median_bands <- list()


# Loop through each band (assuming each raster has 10 bands)

nlyr(stacked_rasters_by_date[[1]])

for (i in 1:nlyr(stacked_rasters_by_date[[1]])) {
  # Extract the corresponding band from each raster stack and combine them
  test <- lapply(masked_raster_list, function(raster) raster[[1]])
  
  # Stack all the extracted bands together
  combined_band_stack <- rast(band_stack)
  
  # Calculate the median for the current band across all stacks
  median_band <- app(combined_band_stack, fun = median, na.rm = TRUE)
  
  # Append the median band to the list
  median_bands[[band_num]] <- median_band
}

# update band names 

band_names <- c("B01","B02","B03","B04","B05","B06","B07","B08","B09","B10","B11","B12","B8A")

names(median_raster) <- band_names 



median_raster <- rast(median_bands)

plot(median_raster)
