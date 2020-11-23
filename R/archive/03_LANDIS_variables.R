# 03 LANDIS variables

library(dplyr)
library(magrittr)
library(tibble)


load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")

source.functions("R/functions")

# -------------------------------------------------

landis_vars <- scn_table %$%
  mapply(
    FUN = function(
      scnid,
      landis_path,
      proj_path,
      proj_mask,
      ntimesteps,
      ncores,
      ht
    ){
      
      print(scnid)
      
      result <- get.landis.vars(
        scn_path = sprintf(
          "%s/%s",
          landis_path,
          scnid
        ),
        proj_path = proj_path,
        out_path = "output/habitat_vars",
        scn_id = scnid,
        proj_mask = ch_mask,
        timesteps = ntimesteps,
        cores = ncores,
        harvest_timber = ht
      )
      
      return(result)
    },
    scnid = scn_id,
    ht = harvest_timber,
    MoreArgs = list(
      proj_path = proj_path,
      proj_mask = ch_mask,
      ntimesteps = ntimesteps,
      ncores = ncores
    ),
    SIMPLIFY = FALSE
  )


landis_variables <- scn_table %>%
  bind_cols(
    tibble(
      landis_vars = landis_vars
    )
  )

save(
  landis_vars,
  landis_variables,
  file = "output/RData/03_LANDIS_variables.RData"
 )
 
# for (i in 1:length(scn_list)){
#   for (j in 1:length(rep_list)){
#     
#     
#     tx <- get.landis.vars(
#       scn_path = sprintf("~/s%s_%s/", scn_list[i], rep_list[j]),
#       proj_path = proj_path,
#       out_path = "output/habitat_vars",
#       scn_id = sprintf("%s_%s", scn_list[i], rep_list[j]),
#       proj_mask = ch_mask,
#       timesteps = ntimesteps,
#       cores = ncores
#     )
#     
#     assign(
#       x = sprintf("lv_%s_%s", scn_list[i], rep_list[j]),
#       value = tx
#     )
#     
#     
#   }
# }
# 
# 
# save(
#   list = ls()[grep("lv_", ls())],
#   file = "output/RData/03_LANDIS_variables.RData"
# )

