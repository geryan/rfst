# 04 Disturbance variables

library(raster)
# library(doMC)
# library(foreach)
library(dplyr)
library(magrittr)
library(tibble)
library(future)
library(future.apply)


load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")
load(file = "output/RData/03_LANDIS_variables.RData")

source.functions("R/functions")

# -------------------------------------------------

if(dim(landis_variables)[1] < ncores){
  ncores <- dim(landis_variables)[1]
}


plan(multisession, workers = ncores)

dvs <- landis_variables %$%
  future_mapply(
  #mapply(
    FUN = function(
      x,
      fire_history,
      logging_history,
      out_path,
      scn_id,
      proj_mask,
      timesteps,
      year0
    ){
      
      firesev <- lapply(
        X = x,
        FUN = function(y){
          y[["firesev"]]
        }
      )
      
      harv <- lapply(
        X = x,
        FUN = function(z){
          z[["harvest"]]
        }
      )
      
      result <- get.dist(
        fire_history = fire_history,
        logging_history = logging_history,
        fs = firesev,
        ha = harv,
        out_path = out_path,
        scn_id = scn_id,
        proj_mask = proj_mask,
        timesteps = timesteps,
        year0 = year0
      )
      
      return(result)
    },
    x = landis_vars,
    scn_id = scn_id,
    MoreArgs = list(
      fire_history = ch_fire_history,
      logging_history = ch_logging_history,
      out_path = "output/landscape_vars",
      proj_mask = ch_mask,
      timesteps = ntimesteps,
      year0 = year0
    ),
    SIMPLIFY = FALSE
  )

plan(sequential)


disturbance_variables <- landis_variables %>%
  bind_cols(
    tibble(
      dist_vars = dvs
    )
  )


save(
  disturbance_variables,
  file = "output/RData/04_disturbance_variables.RData"
)  

# 
# registerDoMC(cores = ncores)
# 
# dvs <- foreach (i = 1:length(scn_list)) %:%
#   foreach (j = 1:length(rep_list)) %dopar% {
#     
#     dv <- get.dist(
#       fire_history = ch_fire_history,
#       logging_history = ch_logging_history,
#       fs = lapply(
#         get(
#           sprintf(
#             "lv_%s_%s",
#             scn_list[i],
#             rep_list[j])),
#         FUN = function(x){x[["firesev"]]}),
#       ha = lapply(
#         get(
#           sprintf(
#             "lv_%s_%s",
#             scn_list[i],
#             rep_list[j])),
#         FUN = function(x){x[["harvest"]]}),
#       out_path = "output/landscape_vars",
#       scn_id = sprintf(
#         "%s_%s",
#         scn_list[i],
#         rep_list[j]),
#       proj_mask = ch_mask,
#       timesteps = ntimesteps,
#       year0 = year0
#     )
#     
#   }
# 
# 
# for (i in 1:length(scn_list)) {
#   for (j in 1:length(rep_list)){
#     
#     assign(
#       x = sprintf("dv_%s_%s",
#                   scn_list[i],
#                   rep_list[j]),
#       value = dvs[[i]][[j]]
#     )
#     
#   }
# }
# 
# save(
#   list = ls()[grep("dv_", ls())],
#   file = "output/RData/04_disturbance_variables.RData"
# )
