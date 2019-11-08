psr_raster <- function(
  sims,
  stage = 0,
  rep = 1,
  timesteps = NULL
){
  
  if(length(rep) > 1){ stop("I can only plot rasters from one replicate at a time")}
  
  if(length(stage) > 1){ stop("I can only plot rasters from one stage at a time")}
  
  ntimesteps <- length(sims[[rep]])
  
  if(is.null(timesteps)){
    
    if(ntimesteps < 5){
      timesteps <- 1:ntimesteps
    }
    
    timesteps <- pretty(1:ntimesteps, n = 3)
    
    if(timesteps[1] < 1){
      timesteps[1] <- 1
    }
    
    if(timesteps[length(timesteps)] > ntimesteps){
      timesteps[length(timesteps)] <- ntimesteps
    }
    
  }

  x <- sims[[rep]]
  x <- x[timesteps]
  
  panels <- c(3,3)
  
  if(stage == 0) {
    
    rasters_sum <- raster::stack(lapply(x, function (landscape) sum(landscape$population)))
    #rasters_sum[rasters_sum == 0] <- NA
    
    names(rasters_sum) <- paste0("Timestep_", timesteps)
    
    # Find maximum and minimum population value in raster cells for all timesteps for life-stage
    scale_max <- ceiling(max(raster::cellStats(rasters_sum, max)))
    scale_min <- floor(min(raster::cellStats(rasters_sum, min)))
    
    # Produce scale of values
    breaks <- seq(scale_min, scale_max, (scale_max-scale_min)/100)
    
    if(any(rasters_sum[] == 0, na.rm = TRUE)){
      colour_range <- c("#bfbfbfff", viridisLite::viridis(length(breaks)-1, option = "A"))
    } else {
      colour_range <- viridisLite::viridis(length(breaks)-1, option = "A")
    }
    
    graphics::par(mar=c(2, 0, 0, 0), mfrow=c(1,1))
    print(rasterVis::levelplot(rasters_sum,
                                 scales = list(draw = FALSE),
                                 margin = list(draw = FALSE),
                                 at = breaks,
                                 col.regions = colour_range,
                                 colorkey = list(space = "bottom",
                                                 width = 0.4),
                                 par.settings=list(layout.heights = list(xlab.key.padding = 1),
                                                   strip.background = list(col = "#ffffff")),
                                 xlab = "Number of individuals"))
    
    
    
  } else {
    
    rasters <- raster::stack(lapply(x, function (landscape) landscape$population[[stage]]))
    
    names(rasters) <- paste0("Timestep_", timesteps)
    
    # Find maximum and minimum population value in raster cells for all timesteps for life-stage
    scale_max <- ceiling(max(raster::cellStats(rasters, max)))
    scale_min <- floor(min(raster::cellStats(rasters, min)))
    
    # Produce scale of values
    breaks <- seq(scale_min, scale_max, (scale_max-scale_min)/100)
    
    if(any(rasters[] == 0, na.rm = TRUE)){
      colour_range <- c("#bfbfbfff", viridisLite::viridis(length(breaks)-1, option = "A"))
    } else {
      colour_range <- viridisLite::viridis(length(breaks)-1, option = "A")
    }
      
  print(rasterVis::levelplot(rasters,
                             scales = list(draw = FALSE),
                             margin = list(draw = FALSE),
                             at = breaks,
                             col.regions = colour_range,
                             colorkey = list(space = "bottom",
                                             width = 0.4),
                             par.settings=list(layout.heights = list(xlab.key.padding = 1),
                                               strip.background = list(col = "#ffffff")),
                             xlab = "Number of individuals"))
      
  
  
  }

}
