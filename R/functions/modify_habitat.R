modify_habitat <- function(
  habitat_map,
  disturbance_map,
  habfun
){
  
  habitat_map2 <- habitat_map
  
  hab_vals <- getValues(habitat_map2)
  
  dist_vals <- getValues(disturbance_map)
  
  mod_vals <- habfun(dist_vals)
  
  mod_hab_vals <- mod_vals * hab_vals
  
  habitat_map2[] <- mod_hab_vals
  
  return(habitat_map2)
  
}