# Hannah Stouter
# AppEEARS data download 
# March 18, 2025

setwd("/Users/hstouter/Desktop/NASA_LCLUC/RF_datasets/AppEEARS_download/")
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

# Select bands to download
datasets(data) # check available bands
# Function will return dataframe with the bands for the dataset selected
bands <- c("B01","B02","B03","B04","B05","B06","B07","B08","B09","B10","B11","B12","B8A","Fmask") #HLSS30 bands
# bands <- c("B01","B02","B03","B04","B05","B06","B07","B09","B10","B11","Fmask") #HLSL30 bands
layers <- desired_data(bands, data)
head(layers) # check that layers and bands are correct

#### Set up JSON for area request ####
shp <- st_read("shp/roi_test.shp")

# Check ROI is correct
leaflet() %>% 
  addPolygons(data = shp, fill = FALSE) %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>% 
  addMiniMap(zoomLevelFixed = 5)

# Convert roi to json
roi_json <- roi(shp)

# specifiy the projection
projection <- projection()

# Compile a JSON Object
taskName <- 'HLSS30_test_dec_2022'
taskType <- 'area'
projection <- projection$Name
outFormat <- 'geotiff'
startDate <- '12-01-2022'
endDate <- '12-31-2022'

# For a reoccuring date
# startDate <- '05-01'                 
# endDate <- '06-30'
# recurring <- TRUE                    
# fromYear <- 2018
# toYear <- 2020

# Create JSON
task_json <- createJSON()

# Check json
task_json

# Submit JSON
submit_request(task_json)

# Download request

outDir <- "/Users/hstouter/Desktop/NASA_LCLUC/RF_datasets/AppEEARS_download/output_test/"
task_id <- ""












