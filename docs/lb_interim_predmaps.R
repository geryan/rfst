library(animation)
library(raster)
library(rasterVis)

saveGIF({
  for (i in 1:51){
    levelplot(preds_lb$predmaps[[1]][[i]], margin = FALSE, at = seq(0, 1, length.out = 100), main = paste0("Year ", i-1))
  }
}, interval = 0.2, movie.name = "sdm_lb_s1_01.gif")

saveGIF({
  for (i in 1:51){
    levelplot(preds_lb$predmaps[[2]][[i]], margin = FALSE, at = seq(0, 1, length.out = 100), main = paste0("Year ", i-1))
  }
}, interval = 0.2, movie.name = "sdm_lb_s4_01.gif")

saveGIF({
  for (i in 1:51){
    levelplot(preds_lb$predmaps[[3]][[i]], margin = FALSE, at = seq(0, 1, length.out = 100), main = paste0("Year ", i-1))
  }
}, interval = 0.2, movie.name = "sdm_lb_s8_01.gif")