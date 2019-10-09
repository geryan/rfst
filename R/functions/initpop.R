initpop <- function(
  hs,
  cc,
  tm,
  popsize,
  proj_mask,
  out_path,
  scn_id,
  varset,
  species
){
  
  ss <- get.stable.states(tm)
  
  maxpop <- sum(getValues(cc), na.rm = TRUE)
  
  if(missing(popsize)){
    popsize <- maxpop
  }
  
  if(popsize > maxpop){
    popsize <- maxpop
  }
  
  Newborn <- rst.op(input1 = hs,
                    input2 = round(cc*ss[1]),
                    op = "pop",
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
                     input2 = round(cc*ss[1]),
                     op = "pop",
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
                  input2 = round(cc*ss[1]),
                  op = "pop",
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