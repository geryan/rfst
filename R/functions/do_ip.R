do_ip <- function(
  hab,
  index = 1,
  ip_raster = FALSE,
  mod_habitat = FALSE
){
  
  
  habitat_map <- hab$hab_map[[index]]
  
  if(mod_habitat){
    if(!is.na(hab$habfun[[index]])){
      habitat_map <- modify_habitat(
        habitat_map,
        disturbance_map = hab$dist_map[[index]],
        habfun = hab$habfun[[index]]
      )
    }
  }
  

  initial_population <- initpop5(
    hs = habitat_map[[1]],
    popsize = hab$popsize[index],
    cc = hab$cc[index],
    ss = hab$ss[[index]],
    pp = hab$pp[[index]],
    z = hab$z[[index]]
  )
  
  
  
  if(ip_raster){
    result <- initial_population
  } else{
    result <- initial_population %>%
      getValues %>%
      sum(na.rm = TRUE)
  }
  
  
  return(result)
  
  
}