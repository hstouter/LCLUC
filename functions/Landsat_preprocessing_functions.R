# June 11, 2025 
# 

# Function to build mask for quality control
decode_bit <- function(qa_raster, bit) {
  terra::app(qa_raster, function(x) bitwAnd(x, bitwShiftL(1, bit)) > 0)
}

# Decode bits 
qa_mask <- function(qa_raster, mask_type) {
  mask_type <- tolower(mask_type)
  
  if (mask_type == "fill") {
    return(decode_bit(qa_raster, 0))
  } else if (mask_type == "dilated") {
    return(decode_bit(qa_raster, 1))
  } else if (mask_type == "cirrus") {
    return(decode_bit(qa_raster, 2))
  } else if (mask_type == "cloud") {
    return(decode_bit(qa_raster, 3))
  } else if (mask_type == "shadow") {
    return(decode_bit(qa_raster, 4))
  } else if (mask_type == "snow") {
    return(decode_bit(qa_raster, 5))
  } else if (mask_type == "clear") {
    return(decode_bit(qa_raster, 6))
  } else if (mask_type == "water") {
    return(decode_bit(qa_raster, 7))
  } else if (mask_type == "high cloud") {
    return(decode_bit(qa_raster, 8) & decode_bit(qa_raster, 9))
  } else if (mask_type == "mid cloud") {
    return(!decode_bit(qa_raster, 8) & decode_bit(qa_raster, 9))
  } else if (mask_type == "low cloud") {
    return(decode_bit(qa_raster, 8) & !decode_bit(qa_raster, 9))
  } else if (mask_type == "high shadow") {
    return(decode_bit(qa_raster, 10) & decode_bit(qa_raster, 11))
  } else if (mask_type == "mid shadow") {
    return(!decode_bit(qa_raster, 10) & decode_bit(qa_raster, 11))
  } else if (mask_type == "low shadow") {
    return(decode_bit(qa_raster, 10) & !decode_bit(qa_raster, 11))
  } else if (mask_type == "high snow/ice") {
    return(decode_bit(qa_raster, 12) & decode_bit(qa_raster, 13))
  } else if (mask_type == "mid snow/ice") {
    return(!decode_bit(qa_raster, 12) & decode_bit(qa_raster, 13))
  } else if (mask_type == "low snow/ice") {
    return(decode_bit(qa_raster, 12) & !decode_bit(qa_raster, 13))
  } else if (mask_type == "high cirrus") {
    return(decode_bit(qa_raster, 14) & decode_bit(qa_raster, 15))
  } else if (mask_type == "mid cirrus") {
    return(!decode_bit(qa_raster, 14) & decode_bit(qa_raster, 15))
  } else if (mask_type == "low cirrus") {
    return(decode_bit(qa_raster, 14) & !decode_bit(qa_raster, 15))
  } else {
    stop(paste("Invalid mask type:", mask_type))
  }
}

# Align Rasters

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
    resolution = c(0.0002711898, 0.0002711898),
    crs = "EPSG:4326"
  )
  
  # Align all rasters in the list
  aligned_rasters <- lapply(rasters, function(r) {
    r_proj <- project(r, "EPSG:4326")                    # Ensure WGS84
    r_crop <- crop(r_proj, roi)                          # Crop to ROI
    r_mask <- mask(r_crop, roi)                          # Mask by ROI
    r_resampled <- resample(r_mask, template_raster)     # Align resolution + extent
  })
  
  return(aligned_rasters)
  
}


# Load in raster and sort each by date
#keyword = what you want the file to search by. For bands "HLSS30.*_B.*\\.tif$"

order_by_date <- function(raster_files){
  
  # Extract the date from the file name (assuming the date is part of the filename)
  extract_date <- function(filename) {
    date_str <- sub(".*doy(\\d{4})(\\d{3}).*", "\\1-\\2", filename)
    return(date_str)  # "YYYY-DDD" format
  }
  
  # Add the extracted date to the file names
  raster_files_with_dates <- tibble(
    file = raster_files,
    date = sapply(raster_files, extract_date)
  )
  
  # Group by date and create a list of stacked rasters
  raster_stack <- raster_files_with_dates %>%
    group_by(date) %>%
    summarise(files = list(file), .groups = "drop") %>%
    mutate(stack = map(files, ~ rast(unlist(.x)))) %>%  # <- Pass vector of files directly to rast()
    pull(stack)
  
  return(raster_stack)
}

# Apply a quality mask (from the fmask) to a list of rasters

qmask_mask <- function(raster, roi, qmask) {
  
  qmask_list <- as.list(qmask)
  
  # Align raster to ROI
  HLS_aligned <- align_rasters(raster, roi)
  
  qmask_aligned <- align_rasters(qmask, roi)
  # # Align quality masks to HLS data
  # qmask_aligned <- lapply(1:length(HLS_aligned), function(i) {
  #   resample(qmask_list[[i]], HLS_aligned[[i]], method = "near")
  # })
  
  
  # Apply mask once, correctly aligned
  masked_list <- mapply(function(x, y) {
    mask(x, y, maskvalue = TRUE, updatevalue = NA)
  }, HLS_aligned, qmask_aligned, SIMPLIFY = FALSE)
  
  # Re-align final masked rasters to ROI
  masked_raster_list <- align_rasters(masked_list, roi)
  
  return(masked_raster_list)
}

# Remove error files 


# Calculate the annual median for a list of rasters 
annual_median <- function(masked_raster_list, band_names) {
  
  median_bands <- list()
  
  for (i in 1:nlyr(masked_raster_list[[1]])) {
    # Extract the corresponding band from each raster stack and combine them
    band_stack <- lapply(masked_raster_list, function(raster) raster[[i]])
    
    # Stack all the extracted bands together
    combined_band_stack <- rast(band_stack)
    
    # Calculate the median for the current band across all stacks
    med <- app(combined_band_stack, fun = function(x) {
      if (all(is.na(x))) {
        return(NA)
      } else {
        return(median(x, na.rm = TRUE))
      }
    })
    
    # Append the median band to the list
    median_bands[[i]] <- med
  }
  
  names(median_bands) <- band_names 
  
  median_raster <- rast(median_bands)
  
  return(median_raster)
}
