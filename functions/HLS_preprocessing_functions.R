

# Function to build mask for quality control

build_mask <- function(fmask, selected_bit_nums){
  # Create a mask of all zeros
  mask <- rast(fmask, vals=0)
  for (b in selected_bit_nums){
    # Apply Bitwise AND to fmask values and selected bit numbers
    mask_temp <- app(fmask, function(x) bitwAnd(x, bitwShiftL(1,b)) >0)
    # Update Mask to maintain only 1 layer with bitwise OR
    mask <- mask | mask_temp
  }
  return(mask)
}


# Load in raster and sort each by date

stack_raster_by_date <- function(file_dir){
  # List all raster files in the folder that match the Landsat bands (e.g., .tif)
  raster_files <- list.files(file_dir, pattern = "B", full.names = TRUE)
  
  # Extract the date from the file name (assuming the date is part of the filename)
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
  
  
  # Group by date and stack the bands for each date
  raster_stack <- raster_files_with_dates %>%
    group_by(date) %>%
    group_map(~ {
      # Load all rasters for the current date
      rasters_for_date <- rast(.x$file)
      # Return a stack of these rasters
      return(rasters_for_date)
    })
  
  # Check the stacked rasters by date
  return(raster_stack)
  
}
