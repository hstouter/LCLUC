# Hannah Stouter
# AppEEARS data download 
# March 18, 2025

setwd("/Users/hstouter/Library/CloudStorage/Box-Box/NASA - Land Cover Land Use Change (2023 - 2026)/Data/LCLUC/RF_Datasets/test_may_2025/AppEEARS_download")
source("AppEARS_functions.R")

# Inputs 
  # Dataset
  # Bands
  # Dates
  # ROI - shp file 

# Required packages
packages <- c('earthdatalogin', 'getPass','httr','jsonlite','ggplot2','dplyr','tidyr','readr','geojsonio','geojsonR', 'sp', 'terra', 'rasterVis', 'RColorBrewer', 'jsonlite', "sf", "sp")

# Add packages to library
lapply(packages, library, character.only = TRUE)

# AppEEARS URL
API_URL = 'https://appeears.earthdatacloud.nasa.gov/api/'

# Login to earth access and save token
token <- login()  


# View available datasets
available_datasets()

# Select data sets to download 
data <- "HLSS30.020"
# data <- "HLSL30.020"

# data <- "MCD64A1.061" #MODIS burned area

# Select bands to download
datasets(data) # check available bands
# Function will return dataframe with the bands for the dataset selected
bands <- c("B01","B02","B03","B04","B05","B06","B07","B08","B09","B10","B11","B12","B8A","Fmask") #HLSS30 bands
# bands <- c("B01","B02","B03","B04","B05","B06","B07","B09","B10","B11","Fmask") #HLSL30 bands
# bands <- c("Burn_Date", "Burn_Date_Uncertainty", "First_Day", "Last_Day", "QA") # MODIS BA
layers <- desired_data(bands, data)
head(layers) # check that layers and bands are correct

#### Set up JSON for area request ####
shp <- st_read("/Users/hstouter/Library/CloudStorage/Box-Box/Fire/shp_files/dja_buffer_v1.shp")

# Check ROI is correct
leaflet() %>% 
  addPolygons(data = shp, fill = FALSE) %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>% 
  addMiniMap(zoomLevelFixed = 5)

plot(shp)


# Convert roi to json
roi_json <- roi(shp)

# specifiy the projection
projection <- projection()

# Compile a JSON Object
taskName <- 'S30_dja_region_2020_2021' 
taskType <- 'area'
projection <- projection$Name
outFormat <- 'geotiff'
startDate <- '10-01-2020'
endDate <- '09-30-2021'

# For a reoccuring date
# startDate <- '10-01'
# endDate <- '09-30'
# recurring <- TRUE
# fromYear <- 2000
# toYear <- 

# Create JSON
task_json <- createJSON()

# Check json
task_json

# Submit JSON
submit_request(task_json)

#### Download request ####

task_id <- ""


# Load all previous AppEARRS requests 

# This looks AppEARS requests 
# limit = the number of previous requests you want to extract
params <- list(limit = 1, pretty = TRUE)

# can also search by name
# params <- list(name = "HLSS30_test_dec_2022")

# Request the task status of last 2 requests from task URL
response_req <- GET(
  paste0(API_URL, "task"),
  query = params,
  httr::add_headers(Authorization = token))

# Retrieve content of the request as JSON
status_response <- toJSON(content(response_req), auto_unbox = TRUE)

# Print the prettified response
prettify(status_response)


# Get task id from status response (can also get this manually)
task_id <- fromJSON(status_response)[[7]]

#### Download a Request ####

# task_id <- fromJSON(task_response)[[1]]
response <- GET(paste0(API_URL, "bundle/", task_id), add_headers(Authorization = token))
bundle_response <- prettify(toJSON(content(response), auto_unbox = TRUE))


### 4b. Download Files in a Request (Automation) 

outDir <- "/Users/hstouter/Library/CloudStorage/Box-Box/NASA - Land Cover Land Use Change (2023 - 2026)/Data/LCLUC/dja_test/HLS_S30/S30_2016"
# Set output directory 

bundle <- fromJSON(bundle_response)$files

# For nested directories (i.e multi-bands landsat )
for (id in bundle$file_id){
  # Retrieve the filename from the file_id
  filename <- bundle[bundle$file_id == id,]$file_name
  # Create a destination directory to store the file in
  filepath <- paste(outDir,filename, sep = "/")
  # Write the file to disk using the destination directory and file name
  response <- GET(paste0(API_URL, "bundle/", task_id, "/", id), 
                  write_disk(filepath, overwrite = TRUE), progress(),
                  add_headers(Authorization = token))
}

# For non_nested files
for (id in bundle$file_id) {
  # Retrieve the filename from the file_id
  filename <- bundle[bundle$file_id == id, ]$file_name
  # Keep only the filename (drop directories)
  filename_clean <- basename(filename)
  # Build full path directly under outDir
  filepath <- file.path(outDir, filename_clean)
  # Download the file
  response <- GET(
    paste0(API_URL, "bundle/", task_id, "/", id),
    write_disk(filepath, overwrite = TRUE),progress(),
    add_headers(Authorization = token)
  )
}









