# 07 combine variables

library(dplyr)

load(file = "output/RData/00_comp_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")
load(file = "output/RData/03_LANDIS_variables.RData")
load(file = "output/RData/04_disturbance_variables.RData")
load(file = "output/RData/05_geophys_vars.RData")
load(file = "output/RData/06_climate_variables.RData")

source.functions("R/functions")

for (i in 1:length(scn_list)){
  for (j in 1:length(rep_list)){
    
    
    tx <- mapply(
      FUN = stack,
      get(
        sprintf(
          "lv_%s_%s",
          scn_list[i],
          rep_list[j])),
      get(
        sprintf(
          "dv_%s_%s",
          scn_list[i],
          rep_list[j])),
      gv,
      clim_vars_4.5
    )
    

    assign(
      x = sprintf("vars_%s_%s", scn_list[i], rep_list[j]),
      value = tx
    )
    
    
  }
}

save(
  list = ls()[grep("vars_", ls())],
  file = "output/RData/07_combined_variables.RData"
)