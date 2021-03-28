library(raster)
library(rasterVis)
library(ggplot2)
library(grid)
library(gridExtra)
library(viridis)
library(gplots)

# wherever "pva_pops" and "pva_k" folders live:
setwd("/home/casey/Research/Projects/Gerry")

# choose the baseline and comparison scenarios
baseline_file <- list.files('pva_pops', pattern = glob2rx('*TH00_rcp85_PB_01*grd*'), full.names = TRUE)
comp_scenario_file <- list.files('pva_pops', pattern = glob2rx('*TH30_rcp85_PB_01*grd*'), full.names = TRUE)

# create the comparison plots
plot_decade_rasters_comp(baseline_file, comp_scenario_file) # totals only
plot_decade_rasters_comp(baseline_file, comp_scenario_file, life_stages = TRUE) # all life stages (if curious)

# plot the rasters for a single scenario
plot_decade_rasters(baseline_file)

# utility functions
# for comparisons...
plot_decade_rasters_comp <- function(raster_file_A, raster_file_B, life_stages = FALSE, scenario = "Scenario X") {
  
  baseline <- restore_rasters(raster_file_A)
  comp_scenario <- restore_rasters(raster_file_B)
  
  nstages <- nlayers(baseline[[1]])
  
  if(life_stages == TRUE) {
    for (i in 1:nstages) {
      
      A <- stack(lapply(baseline, function(x) x[[i]]))
      
      B <- stack(lapply(comp_scenario, function(x) x[[i]]))
      
      png(paste0(substring(raster_file_A, 23, 48), "_stage_", i, "_comp.png"), width = 1200, height = 1500, res = 150)
      
      par(mar=c(0, 0, 0, 0))
      layout(matrix(c(0, 1, 1, 1, 1, 0, 0,
                      2, 8, 0, 9, 0, 20, 0,
                      3, 10, 0, 11, 0, 20, 0,
                      4, 12, 0, 13, 0, 20, 0,
                      5, 14, 0, 15, 0, 20, 0,
                      6, 16, 0, 17, 0, 20, 0,
                      7, 18, 0, 19, 0, 20, 0), nrow = 7, byrow = TRUE),
             widths = c(4, 3, 1, 3, 1.5, 1.5, 0.5),
             heights = c(1, 4, 4, 4, 4, 4, 4))
      
      plot(c(0, 1), c(0, 1), ann = FALSE, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(0.4, 0.5, paste0("Baseline                  ", scenario), cex = 2.0)
      plot(c(0, 1), c(0, 1), ann = FALSE, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(0.5, 0.5, "Initial", cex = 1.8)
      for (j in 1:5) {
        plot(c(0, 1), c(0, 1), ann = FALSE, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
        text(0.5, 0.5, paste0("Decade ", j), cex = 1.8)
      }
      for (p in 1:6) {
        image(A[[p]], axes = FALSE, col = c("lightgray", viridis(10)), asp = 1)
        image(B[[p]], axes = FALSE, col = c("lightgray", viridis(10)), asp = 1)
      }
      dims <- seq(1 , 2, (2 - 1) / 11)
      plot(NA, xlim = c(1, 2), ylim = c(1, 2), ann = FALSE, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      rect(1.4, head(dims, -1), 1.7, tail(dims, -1), col = c("lightgray", viridis(10)))
      mtext(0:10, side = 4, at = tail(dims, -1) - 0.05, las = 2, cex = 1.0)
      mtext("Population", side = 1, cex = 1.2, padj = -1.5)
      
      dev.off()
      
    }
  }else{
    A_tot <- stack(lapply(baseline, function(x) sum(x)))
    B_tot <- stack(lapply(comp_scenario, function(x) sum(x)))
    
    png(paste0(substring(raster_file_A, 23, 48), "_total.png"), width = 1200, height = 1500, res = 150)
    
    par(mar=c(0, 0, 0, 0))
    layout(matrix(c(0, 1, 1, 1, 1, 0, 0,
                    2, 8, 0, 9, 0, 20, 0,
                    3, 10, 0, 11, 0, 20, 0,
                    4, 12, 0, 13, 0, 20, 0,
                    5, 14, 0, 15, 0, 20, 0,
                    6, 16, 0, 17, 0, 20, 0,
                    7, 18, 0, 19, 0, 20, 0), nrow = 7, byrow = TRUE),
           widths = c(4, 3, 1, 3, 1.5, 1.5, 0.5),
           heights = c(1, 4, 4, 4, 4, 4, 4))
    
    plot(c(0, 1), c(0, 1), ann = FALSE, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(0.4, 0.5, paste0("Baseline                  ", scenario), cex = 2.0)
    plot(c(0, 1), c(0, 1), ann = FALSE, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(0.5, 0.5, "Initial", cex = 1.8)
    for (j in 1:5) {
      plot(c(0, 1), c(0, 1), ann = FALSE, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(0.5, 0.5, paste0("Decade ", j), cex = 1.8)
    }
    for (p in 1:6) {
      image(A_tot[[p]], axes = FALSE, col = c("lightgray", viridis(10)), asp = 1)
      image(B_tot[[p]], axes = FALSE, col = c("lightgray", viridis(10)), asp = 1)
    }
    dims <- seq(1 , 2, (2 - 1) / 11)
    plot(NA, xlim = c(1, 2), ylim = c(1, 2), ann = FALSE, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    rect(1.4, head(dims, -1), 1.7, tail(dims, -1), col = c("lightgray", viridis(10)))
    mtext(0:10, side = 4, at = tail(dims, -1) - 0.05, las = 2, cex = 1.0)
    mtext("Population", side = 1, cex = 1.2, padj = -1.5)
    
    dev.off()
  }
  
}

# for single scenarios...
plot_decade_rasters <- function(raster_file, life_stages = FALSE) {
  
  r <- restore_rasters(raster_file)
  
  
  
  nstages <- nlayers(r[[1]])
  
  layer_names <- c("Initial Population",
                   paste0("Population - Decade ", 1:5))
  
  if(life_stages == TRUE) {
    for (i in 1:nstages) {
      
      r_ls <- stack(lapply(r, function(x) x[[i]]))
      
      vals <- unique(na.omit(sapply(r_ls, FUN = function(x) x[])))
      
      cols <- c("lightgray", viridis(max(vals)))
      
      png(paste0(substring(raster_file, 18, 48), "_stage_", i, ".png"), width = 1200, height = 1500, res = 150)
      
      par(mar=c(1,1,1,1))
      p <- levelplot(r_ls,
                     layout = c(2, 3),
                     margin = FALSE,
                     auto.key = FALSE,
                     scales = list(draw = FALSE),
                     par.settings = list(axis.line = list(col = "transparent"),
                                         strip.background = list(col = 'transparent'), 
                                         strip.border = list(col = 'transparent')),
                     names.attr = layer_names,
                     col.regions = cols,
                     colorkey = list(col = cols,
                                     at = do.breaks(range(vals), max(vals)),
                                     space = "bottom"))
      print(p)
      
      dev.off()
      
    }
  }else{
    r_tot <- stack(lapply(r, function(x) sum(x)))
    
    vals <- unique(na.omit(sapply(r_tot, FUN = function(x) x[])))
    
    cols <- c("lightgray", viridis(max(vals)))
    
    png(paste0(substring(raster_file, 18, 48), "_total.png"), width = 1200, height = 1500, res = 150)
    
    par(mar=c(1,1,1,1))
    p <- levelplot(r_tot,
                   layout = c(2, 3),
                   margin = FALSE,
                   auto.key = FALSE,
                   scales = list(draw = FALSE),
                   par.settings = list(axis.line = list(col = "transparent"),
                                       strip.background = list(col = 'transparent'), 
                                       strip.border = list(col = 'transparent')),
                   names.attr = layer_names,
                   col.regions = cols,
                   colorkey = list(col = cols,
                                   at = do.breaks(range(vals), max(vals)),
                                   space = "bottom"))
    print(p)
    
    dev.off()
    
  }
}

# extract the rasters
restore_rasters <- function(raster_file) {
  z <- brick(raster_file)
  
  nstages <- nlayers(z) / 6
  
  y <- vector(
    mode = "list",
    length = 6
  )
  
  for(i in 1:6){
    y[[i]] <- z[[(i - 1) * nstages + (1:nstages)]]
  }
  
  return(y)
}
