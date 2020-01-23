interpolate.climdat <- function(
  initras,
  futras,
  ntimesteps,
  year0,
  proj_mask,
  out_path,
  climate_model,
  rcp,
  climate_variable,
  season,
  filename = NA
){
  
  
  projection_years <- futras %>%
    names %>%
    sub(
      pattern = "X",
      replacement = "",
      x = .
    ) %>%
    as.numeric
  
  
  if(ntimesteps > (max(projection_years) - year0)){
    stop("Projections go beyond ntimesteps")
  }
  
  
  ns <- c(year0, projection_years)
  
  
  zz <- c(year0:(year0 + ntimesteps))
  
  allras <- stack(initras, futras)
  
  z <- rep(list(proj_mask), (ntimesteps + 1))
  
  for (i in 1:(ntimesteps + 1)){
    
    set1 <- zz[i] - ns
    
    index.nearest <- which.min(abs(set1))
    
    set2 <- zz[i] - ns[-index.nearest]
    
    index.set2 <- which.min(abs(set2))
    
    value.set2 <- set2[index.set2]
    
    index.2nd <- which(set1 == value.set2)
    
    dist.nearest <- min(abs(set1))
    dist.2nd <- min(abs(set2))
    
    
    if(dist.nearest == 0){
      
      z[[i]][] <- getValues(allras[[index.nearest]])
      
    } else {
      
      v1 <- getValues(allras[[index.nearest]])

      v2 <- getValues(allras[[index.2nd]])
      
      w <- dist.nearest + dist.2nd
      
      v <- v1*(w - dist.nearest)/w + v2*(w - dist.2nd)/w
      
      
      z[[i]][] <- v
      
    }
  }
  
  brickz <- brick(z)
  
  if(is.na(filename)){
    filename <- sprintf(
      "%s/interpolated_%s_%s_%s_%s.grd",
      out_path,
      climate_model,
      rcp,
      climate_variable,
      season
    )
  }
  
  
  layernames <- sprintf(
    "%s_%s_%s",
    climate_variable,
    season,
    zz
  )
  
  result <- rst.op(
    input1 = brickz,
    op = "writeBrick",
    proj_mask = proj_mask,
    filename = filename,
    layernames = layernames
  )
  
  return(result)
  
}