# 03 LANDIS variables

load(file = "output/RData/00_comp_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")

source(file = "R/functions/get.landis.vars.R")


for (i in 1:length(scn_list)){
  for (j in 1:length(rep_list)){
    
    
    tx <- get.landis.vars(
      scn_path = sprintf("~/s%s_%s/", scn_list[i], rep_list[j]),
      proj_path = proj_path,
      out_path = "output/habitat_vars",
      scn_id = sprintf("%s_%s", scn_list[i], rep_list[j]),
      proj_mask = ch_mask,
      timesteps = ntimesteps,
      cores = ncores
    )
    
    assign(
      x = sprintf("lv_%s_%s", scn_list[i], rep_list[j]),
      value = tx
    )
    
    
  }
}


save(
  list = ls()[grep("lv_", ls())],
  file = "output/RData/03_LANDIS_variables.RData"
)


