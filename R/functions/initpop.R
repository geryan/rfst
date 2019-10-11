initpop <- function(
  hs,
  cc,
  threshold = 0.7,
  tm,
  popsize,
  proj_mask,
  out_path,
  scn_id,
  varset,
  species,
  rf = "round"
){
  
  ss <- get.stable.states(tm)
  
  maxpop <- sum(cc - round(cc * dlogis(getValues(hs), scale = 0.25)), na.rm = TRUE)

  if(missing(popsize)){
    popsize <- maxpop
  }

  if(popsize > maxpop){
    popsize <- maxpop
  }
  
  roundfun <- ifelse(rf == "round", round, ceiling)
  
  Newborn <- rst.op(input1 = hs,
                    op = "pop",
                    cc = roundfun(cc*ss[1]),
                    threshold = threshold,
                    proj_mask = proj_mask,
                    filename = sprintf(
                      "%s/pop0_n_%s_%s_%s.grd",
                      out_path,
                      scn_id,
                      varset,
                      species
                    ),
                    layernames = "Newborn",
                    popsize = round(popsize*ss[1]))
  
  Juvenile <- rst.op(input1 = hs,
                     op = "pop",
                     cc = roundfun(cc*ss[2]),
                     threshold = threshold,
                     proj_mask = proj_mask,
                     filename = sprintf(
                       "%s/pop0_j_%s_%s_%s.grd",
                       out_path,
                       scn_id,
                       varset,
                       species
                     ),
                     layernames = "Juvenile",
                     popsize = round(popsize*ss[2]))
  
  
  Adult <- rst.op(input1 = hs,
                  op = "pop",
                  cc = roundfun(cc*ss[3]),
                  threshold = threshold,
                  proj_mask = proj_mask,
                  filename = sprintf(
                    "%s/pop0_a_%s_%s_%s.grd",
                    out_path,
                    scn_id,
                    varset,
                    species
                  ),
                  layernames = "Adult",
                  popsize = round(popsize*ss[3]))
  
  
  result <- stack(Newborn, Juvenile, Adult)
  
  return(result)
}