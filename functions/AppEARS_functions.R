# March 13, 2025 
# AppEARS Functions

# Required packages
packages <- c('earthdatalogin', 'getPass','httr','jsonlite','ggplot2','dplyr','tidyr','readr','geojsonio','geojsonR', 'sp', 'terra', 'rasterVis', 'RColorBrewer', 'jsonlite', "sf", "sp")
# Add packages to library
lapply(packages, library, character.only = TRUE)

#### Log in ####

login <- function(){
  # AppEEARS URL
  API_URL = 'https://appeears.earthdatacloud.nasa.gov/api/'
  
  #log into Earth data
  secret <- jsonlite::base64_enc(paste(
    getPass::getPass(msg = "Enter your NASA Earthdata Login Username:"),
    getPass::getPass(msg = "Enter your NASA Earthdata Login Password:"),
    sep = ":"))
  
  login_req <- httr::POST(
    paste0(API_URL,"login"),
    httr:: add_headers(
      "Authorization" = paste("Basic", gsub("\n", "", secret)),
      "Content-Type" = "application/x-www-form-urlencoded;charset=UTF-8"),
    body = "grant_type=client_credentials")
  
  # httr::status_code(login_req) # if it is sucessful it will return 200
  
  # Retrieve the content of the request and convert the response to the JSON object
  token_response <- toJSON(content(login_req), auto_unbox = TRUE)
  
  # Print the prettified response if desired (this will show the token)
  # prettify(token_response)
  
  return(paste("Bearer", fromJSON(token_response)$token))
  
}

available_datasets <- function() {
  
  prod_req <- GET(paste0(API_URL, "product"))
  
  # Retrieve the content of request and convert the info to JSON object
  all_prods <- toJSON(content(prod_req), auto_unbox = TRUE)
  
  # Print the prettified product response
  prettify(all_prods)
  
  #Divides information from each product.
  divided_products <- split(fromJSON(all_prods), seq(nrow(fromJSON(all_prods))))
  
  # Create a list indexed by the product name and version
  products <- setNames(divided_products, fromJSON(all_prods)$ProductAndVersion)
  
  # Print no. products available in AppEEARS
  sprintf("AppEEARS currently supports %i products.", length(products))
  
  return(
  for (p in products){
    print(paste0(p$ProductAndVersion," is ",p$Description," from ",p$Source))
  }
  )
}

#### Select products to download ####

datasets <- function(dataset) {
  # Make list of desired products
  desired_products <- c(dataset)
  desired_products
  
  ### 2b. Search and Explore Available Layers
  
  # Request layers for the 1st product in the list
  data_req <- GET(paste0(API_URL,"product/", dataset))

  
  # Retrieve content of the request and convert the content to JSON object
  # The response will contain the layer names and attributes for the product.
  data_response <- toJSON(content(data_req), auto_unbox = TRUE)

  # Print the prettified response
  prettify(data_response)
  
  # Return list of layers
  return(names(fromJSON(data_response)))
  
}


#### Select bands to download ####

desired_data <- function(bands, data){

  # Create a vector of desired layers
  desired_layers <- bands
  # Create a vector of products including the desired layers
  desired_prods <- rep(data, length(desired_layers))
      
      
  # Create a data frame including the desired data products and layers
  return(data.frame(product = desired_prods, layer = desired_layers))
}


#### Set up ROI ####

roi <- function(shp) {
  # convert to json 
  roi_json <- geojsonio::geojson_json(sf::st_as_sf(shp))
  
  # read in json as a list
  return(geojsonR::FROM_GeoJson(roi_json))
}

projection <- function(){
  
  # Set projection
  # Request the projection info from API_URL
  proj_req <- GET(paste0(API_URL, "spatial/proj"))
  
  # Retrieve content of the request and convert to JSON object
  proj_response <- toJSON(content(proj_req), auto_unbox = TRUE)
  
  # List the available projections
  projs <- fromJSON(proj_response)
  
  # "geographic" = WGS 1984
  return(projs[projs$Name=="geographic",])
  
}

#### Creaete the JSON to submit the request ####
createJSON <- function() {
  # Create a dataframe including the date range for the request.
  date <- data.frame(startDate = startDate, endDate = endDate)
  
  # If you set the recurring to TRUE
  #date <- data.frame(startDate = startDate, endDate = endDate , recurring = recurring)
  #date$yearRange <- list(c(fromYear,toYear))
  
  # Next, create a list including the projection and add the output format information.
  out <- list(projection)
  names(out) <- c("projection")
  out$format$type <- outFormat
  
  # Change the GeoJSON format for successful task submission.
  roi_json$features[[1]]$geometry$coordinates <- list(roi_json$features[[1]]$geometry$coordinates)
  
  # Next, compile dataframes and lists to create a nested dataframe. 
  # Create a list of dataframes and assign names
  task_info <- list(date, layers, out, roi_json)
  names(task_info) <- c("dates", "layers", "output", "geo")
  
  # Create the nested list and assing names
  task <- list(task_info, taskName, taskType)
  names(task) <- c("params", "task_name", "task_type") 
  
  # Convert datafram to json string 
  task_json <- jsonlite::toJSON(task, auto_unbox = TRUE, digits = 10)
  
  return(prettify(task_json))
  
}

submit_request <- function(task_json) {
  response <- POST(
    paste0(API_URL, "task"),
    body = task_json,
    encode = "json",
    httr::add_headers(Authorization = token, "Content-Type" = "application/json"))
  
  httr::status_code(response)
  
  # Retrieve content of the request
  task_response <- jsonlite::toJSON(content(response), auto_unbox = TRUE)
  
  # Print the response
  return(prettify(task_response))
  
}


# Download data

download_data <- function(task_id, outDir) {
response <- GET(paste0(API_URL, "bundle/", task_id), add_headers(Authorization = token))
bundle_response <- prettify(toJSON(content(response), auto_unbox = TRUE))


### 4b. Download Files in a Request (Automation) 

# Set output directory 

bundle <- fromJSON(bundle_response)$files
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


}