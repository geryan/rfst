# 04 Disturbance variables

library(raster)

load(file = "output/RData/00_comp_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")
load(file = "output/RData/03_LANDIS_variables.RData")

source(file = "R/functions/get.disturbance.R")
source(file = "R/functions/rst.op.R")

registerDoMC(cores = ncores)

dvs <- foreach (i = 1:length(scn_list)) %:%
  foreach (j = 1:length(rep_list)) %dopar% {
    
    
    lh <- get.disturbance(
      disturbances = lapply(get(sprintf("lv_%s_%s",
                                        scn_list[i],
                                        rep_list[j])),
                            FUN = function(x){x[["firesev"]]}),
      disturbance_history = ch_logging_history,
      out_path = "output/landscape_vars",
      scn_id = sprintf("%s_%s", scn_list[i], rep_list[j]),
      proj_mask = ch_mask,
      layernames = c("lohi", "tsl"),
      timesteps = ntimesteps,
      year0 = year0
    )
    
    fh <- get.disturbance(
      disturbances = lapply(get(sprintf("lv_%s_%s",
                                        scn_list[i],
                                        rep_list[j])),
                            FUN = function(x){x[["harvest"]]}),
      disturbance_history = ch_fire_history,
      out_path = "output/landscape_vars",
      scn_id = sprintf("%s_%s", scn_list[i], rep_list[j]),
      proj_mask = ch_mask,
      layernames = c("fihi", "tsf"),
      timesteps = ntimesteps,
      year0 = year0
    )
    
    
    dv <- mapply(lh, fh, FUN = stack)
    
  }


for (i in 1:length(scn_list)) {
  for (j in 1:length(rep_list)){
    
    assign(
      x = sprintf("dv_%s_%s", scn_list[i], rep_list[j]),
      value = dvs[[i]][[j]]
    )
    
  }
}

save(
  list = ls()[grep("dv_", ls())],
  file = "output/RData/04_disturbance_variables.RData"
)
