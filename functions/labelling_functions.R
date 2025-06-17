# functions RF

# clean labels 

clean_labels <- function(label, class) {
  
  label <- lapply(label, st_read)
  
  # Keep only the geometry column
  label <- lapply(label, function(sf) {
    sf <- st_as_sf(st_geometry(sf))  # Create an sf object with only geometry
  })
  
  # Combine all sf objects into one
  label <- do.call(rbind, label)
  
  # Add a new column 'class' with the value "label"
  label$class <- rep(class, nrow(label))
  
  # Remove duplicate rows
  label <- label %>% distinct()
  
  return(label)
}

