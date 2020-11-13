get.mpc.threshold <- function(
  x,
  y,
  sp,
  probs = 0.1
){
  
  result <- x[[1]] %>%
    raster::extract(
      y = y$pa_dat[[which(y$sp == sp)]] %>%
        dplyr::filter(PA == 1)
    ) %>%
    quantile(probs = probs) %>%
    signif(digits = 3)
  
  return(result)
  
}