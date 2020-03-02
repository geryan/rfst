library(dplyr)
library(magrittr)
library(tibble)


load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")

source.functions("R/functions")


# bb <- get.landis.vars(
#   scn_path = paste0("~/", scn_table$scn_id[2]),
#   proj_path = proj_path,
#   out_path = "output/habitat_vars/",
#   scn_id = scn_table$scn_id[2],
#   proj_mask = ch_mask,
#   timesteps = ntimesteps,
#   cores = 20,
#   harvest_timber = scn_table$harvest_timber[2]
# )



landis_vars <- scn_table[2,] %$%
  mapply(
    FUN = function(
      scnid,
      proj_path,
      proj_mask,
      ntimesteps,
      ncores
    ){
      
      print(scnid)
      
      result <- get.landis.vars(
        scn_path = sprintf(
          "~/%s",
          scnid
        ),
        proj_path = proj_path,
        out_path = "output/habitat_vars",
        scn_id = scnid,
        proj_mask = ch_mask,
        timesteps = ntimesteps,
        cores = ncores
      )
      
      return(result)
    },
    scnid = scn_id,
    MoreArgs = list(
      proj_path = proj_path,
      proj_mask = ch_mask,
      ntimesteps = ntimesteps,
      ncores = ncores
    ),
    SIMPLIFY = FALSE
  )
