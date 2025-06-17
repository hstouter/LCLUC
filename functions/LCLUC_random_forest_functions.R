# Random Forest LCLUC model functions

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


predictor_dataset <- function(rasters, labels){
  
  # Extract information about each band for each label point 
  extracted_values <- raster::extract(rasters, labels)
  
  # Combine extracted data and label data 
  all_label_data <- cbind(labels, extracted_values)
  
  
  ##### CREATE THREE DATASETS TO TEST THE ACCURACY OF THE RANDOM FOREST WITH DIFFERENT NUMBERS OF LAND USE TYPES #####
  # convert to data table 
  
  all_label_data <- as.data.table(all_label_data)
  
  all_label_data <- all_label_data %>% select(-long, -lat, -ID)
  
  return(all_label_data)
}

# Subset labels to run the model with fewer classes

subset_labels <- function(labels, classes){
  # Create dataset with only labels for "forest", "water" and "bare"
  subset <- ifelse(
    labels$class %in% classes,
    labels$class,
    "nonforest"
  )
  
  
  subset <- cbind(subset, labels)
  subset <- subset %>% dplyr::select(-class)
  subset <- subset %>% rename(class = subset)
  
  subset$class <- as.factor(subset$class)
  
  return(subset)
  
}


# Split data into test and training 

split_data <- function(data){
  
  data$random <- runif(nrow(data), 0, 1)
  
  train_data <- subset(data, data$random <0.70)
  train_data <- train_data %>% dplyr::select(-random)
  train_data$class <- as.factor(train_data$class)
  
  
  test_data <- subset(data, data$random >0.70)
  test_data <- test_data %>% dplyr::select(-random)
  test_data$class <- as.factor(test_data$class)

  return(list(train = train_data, test = test_data))

}


built_empty_raster <- function(model_raster){
  # get number of pixel
  cell_indices <- 1:ncell(rasters)
  
  # extract coordinates from each pixel 
  rast_coords <- xyFromCell(rasters, cell_indices)
  
  raster_stack_data <- as.data.frame(rasters)
  raster_stack_data <- as.data.table(rasters)
  head(raster_stack_data)
  
  # combine coordinates with band values 
  pixels_coords <- cbind(rast_coords, raster_stack_data)
  
  
  # convert to matrix 
  pixels_coords <- as.matrix(pixels_coords)
  
  
  # create an empty row for forest to match test data set
  class <- rep(NA, nrow(pixels_coords))
  
  # rename columns to match test data set 
  empty_raster <- cbind(class, pixels_coords)
  colnames(empty_raster)[2] <- "X"
  colnames(empty_raster)[3] <- "Y"

  return(empty_raster)
}
