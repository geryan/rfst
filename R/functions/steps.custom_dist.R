steps.custom_dist <- function (disturbance_layers, disturbance_function) {
  
  dist_fun <- function (landscape, timestep) {
    
    if (raster::nlayers(landscape$suitability) > 1) {
      original_habitat <- landscape$suitability[[timestep]]
    } else {
      original_habitat <- landscape$suitability
    }
    
    if (raster::nlayers(landscape[[disturbance_layers]]) < timestep ) {
      stop("The number of disturbance layers must match the number of timesteps in the simulation")
    }
    
    # process disturbance layer
    dist_layer <- landscape[[disturbance_layers]][[timestep]]
    idx <- which(!is.na(dist_layer[]))
    
    mod_values <- disturbance_function(x = dist_layer[idx])
    mod_values_rescaled <- (mod_values - min(mod_values)) / (max(mod_values) - min(mod_values))
    
    modified_habitat <- original_habitat
    modified_habitat[idx] <- original_habitat[idx] * mod_values_rescaled
    
    names(modified_habitat) <- paste0("Habitat_", timestep)
    
    if (raster::nlayers(landscape$suitability) > 1) {
      landscape$suitability[[timestep]] <- modified_habitat
    } else {
      landscape$suitability <- modified_habitat
    }
    
    landscape
    
  }
}