# March 20, 2025
# HLS pre-processing

setwd("/Users/hstouter/Desktop/NASA_LCLUC/RF_datasets/AppEEARS_download/")



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

## 4. Download a Request

# task_id <- fromJSON(task_response)[[1]]
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

