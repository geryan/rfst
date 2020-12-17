
source("R/spartan/spartan_settings.R")

library(raster)
library(dplyr)
library(sf)
library(lubridate)
library(purrr)
library(tibble)
library(tidyr)


load(file = "output/RData/00_controls_eg.RData")
load(file = "output/RData/01_landscape_variables_eg.RData")
load(file = "output/RData/07_combined_variables_eg.RData")
load(file = "output/RData/09_fit_distribution_models_eg.RData")

source.functions("R/functions")


a_list <- list.files("/data/gpfs/projects/punim1340/rfst_eg/output/spartan_RData/habitat_pred_aggregated/")

p_list <- list.files("/data/gpfs/projects/punim1340/rfst_eg/output/spartan_RData/habitat_pred/")



a_id <- sub(
  pattern = "agg_",
  replacement = "",
  x = a_list
) %>%
  sub(
    pattern = ".Rds",
    replacement = "",
    x = .
  )

a_sp <- sub(
  pattern = ".*_",
  replacement = "",
  x = a_id
)


a_ycsn <- strsplit(
  x = a_id,
  split = "_"
) %>%
  lapply(
    FUN = function(x){
      x[1:6]
    }
  ) %>%
  sapply(
    FUN = paste,
    collapse = "_"
  )


a_maps <- lapply(
  X = a_list,
  FUN = function(x){
    z <- readRDS(
      file = sprintf(
        "/data/gpfs/projects/punim1340/rfst_eg/output/spartan_RData/habitat_pred_aggregated/%s",
        x
      )
    )
    
    return(z)
    
  }
)


aggs <- tibble(
  ycscnid = a_ycsn,
  sp =  a_sp,
  aggmaps = a_maps
)


###########


p_id <- sub(
  pattern = "pred_",
  replacement = "",
  x = p_list
) %>%
  sub(
    pattern = ".Rds",
    replacement = "",
    x = .
  )

p_sp <- sub(
  pattern = ".*_",
  replacement = "",
  x = p_id
)


p_ycsn <- strsplit(
  x = p_id,
  split = "_"
) %>%
  lapply(
    FUN = function(x){
      x[1:6]
    }
  ) %>%
  sapply(
    FUN = paste,
    collapse = "_"
  )


p_maps <- lapply(
  X = p_list,
  FUN = function(x){
    z <- readRDS(
      file = sprintf(
        "/data/gpfs/projects/punim1340/rfst_eg/output/spartan_RData/habitat_pred/%s",
        x
      )
    )
    
    return(z)
    
  }
)



preds <- tibble(
  ycscnid = p_ycsn,
  sp =  p_sp,
  predmaps = p_maps
)

######


vs <- var_set_eg %>%
  dplyr::select(
    scenario,
    scenario_replicate,
    rcp,
    climate_model,
    yearid,
    harvest_scenario,
    plan_burn,
    yscn_id,
    scn_id,
    cscnid,
    ycscnid
  )



pred_set <- full_join(
  x = vs,
  y = preds,
  by = "ycscnid"
)

agg_set <- full_join(
  x = vs,
  y = aggs,
  by = "ycscnid"
)


save(
  pred_set,
  agg_set,
  file = "output/RData/10_predict_SDMs_eg.RData"
) 


save(
  agg_set,
  file = "output/RData/10_predict_SDMs_agg_eg.RData"
) 