grplot <- function(a){
  
  layer <- eval(parse(text = names(a)))
  
  z <- ggplot() +
    geom_raster(data = as.data.frame(a,
                                     xy = TRUE),
                aes(x = x,
                    y = y,
                    fill = layer),
                na.rm = TRUE) +
    coord_fixed() +
    theme_minimal() +
    scale_fill_viridis_c() +
    theme(axis.title = element_blank())
  
  return(z)
}