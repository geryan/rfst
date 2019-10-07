mapcc <- function(
  inputlist,
  scn_id,
  proj_mask,
  out_path = "output/pva_vars/",
  sp_id = "lb",
  size
){
  
  cc <- rst.op(
    input1 = inputlist[[1]],
    op = "cc",
    proj_mask = proj_mask,
    filename = sprintf(
      "%s/cc_%s_%s.grd",
      out_path,
      sp_id,
      scn_id
    ),
    layernames = "carryingCapacity",
    size = size
  )
  
  return(cc)
  
}