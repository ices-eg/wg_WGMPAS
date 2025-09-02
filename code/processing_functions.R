#functions

chunk_and_process <- function(sf_data, zone, chunk_size = 500) {
  # Split into manageable chunks
  n_features <- nrow(sf_data)
  chunks <- split(1:n_features, ceiling(seq_along(1:n_features)/chunk_size))
  
  # Process each chunk in parallel using future_map
  results <- future_map(chunks, function(idx) {
    chunk <- sf_data[idx, ]
    
    # Only fix invalid geometries in the chunk if needed
    invalid_geoms <- !st_is_valid(chunk)
    if(any(invalid_geoms)) {
      chunk[invalid_geoms,] <- st_make_valid(chunk[invalid_geoms,])
    }
    
    # Perform intersection test
    intersects <- st_intersects(chunk, zone, sparse = TRUE)
    
    # Return only the intersecting features
    chunk[lengths(intersects) > 0, ]
  }, .progress = TRUE)
  
  # Combine results
  do.call(rbind, results)
}

process_offshore_mpas_efficient <- function(mpas, europe, nm_12 = 12, nm_200 = 200) {
  message("Starting processing...")
  
  # Create buffers
  message("Creating buffers...")
  coastal_buffer <- st_buffer(europe, nm_12 * 1852)
  eez_buffer <- st_buffer(europe, nm_200 * 1852)
  
  # Process in smaller chunks with spatial indexing
  chunk_size <- 100  # Smaller chunk size
  n_features <- nrow(mpas)
  n_chunks <- ceiling(n_features/chunk_size)
  
  offshore_list <- list()
  
  message("Processing chunks...")
  for(i in 1:n_chunks) {
    # Create chunk indices
    start_idx <- ((i-1) * chunk_size) + 1
    end_idx <- min(i * chunk_size, n_features)
    
    # Process chunk
    chunk <- mpas[start_idx:end_idx, ]
    
    # Check validity only if needed
    if(!all(st_is_valid(chunk))) {
      chunk <- st_make_valid(chunk)
    }
    
    # First check if intersects with EEZ (larger area)
    eez_intersects <- st_intersects(chunk, eez_buffer, sparse = TRUE)
    potential_offshore <- chunk[lengths(eez_intersects) > 0, ]
    
    if(nrow(potential_offshore) > 0) {
      # Then check if these intersect with coastal zone
      coastal_intersects <- st_intersects(potential_offshore, coastal_buffer, sparse = TRUE)
      # Keep only those that don't intersect with coastal zone
      offshore_mpas <- potential_offshore[lengths(coastal_intersects) == 0, ]
      
      if(nrow(offshore_mpas) > 0) {
        offshore_list[[i]] <- offshore_mpas
      }
    }
    
    # Progress message
    if(i %% 10 == 0) {
      message(sprintf("Processed chunk %d of %d", i, n_chunks))
    }
  }
  
  # Combine results
  message("Combining results...")
  if(length(offshore_list) > 0) {
    result <- do.call(rbind, offshore_list)
  } else {
    result <- mpas[0, ]  # Return empty dataframe with same structure
  }
  
  return(result)
}

create_csquares_grid <- function(bbox, cellsize = 0.05) {
  # Define the CRS
  target_crs <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
  
  # Round the bbox limits to align with the cellsize grid
  xmin <- floor(bbox["xmin"] / cellsize) * cellsize
  xmax <- ceiling(bbox["xmax"] / cellsize) * cellsize
  ymin <- floor(bbox["ymin"] / cellsize) * cellsize
  ymax <- ceiling(bbox["ymax"] / cellsize) * cellsize
  
  # Create the grid with CRS
  grid <- st_make_grid(
    cellsize = cellsize,
    offset = c(xmin, ymin),
    n = c(
      ceiling((xmax - xmin)/cellsize),
      ceiling((ymax - ymin)/cellsize)
    )
  ) %>%
    st_set_crs(target_crs)  # Set the CRS
  
  # Convert to sf dataframe and add coordinates
  grid_sf <- st_sf(geometry = grid) %>%
    mutate(
      centroid = st_centroid(geometry),
      lon = st_coordinates(centroid)[,1],
      lat = st_coordinates(centroid)[,2]
    ) %>%
    # Calculate approximate area in km2
    mutate(
      area_km2 = (111.32 * cellsize) * (111.32 * cellsize * cos(lat * pi/180))
    )
  
  # Remove the centroid column
  grid_sf$centroid <- NULL
  
  return(grid_sf)
}
