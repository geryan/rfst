
source("R/spartan/spartan_settings.R")

library(raster)
library(dplyr)
library(sf)
library(lubridate)
library(purrr)
library(tibble)
library(tidyr)


load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")
load(file = "output/RData/07_combined_variables.RData")
load(file = "output/RData/09_fit_distribution_models.RData")

source.functions("R/functions")


a_list <- list.files(
  path = "output/spartan_RData/habitat_pred_aggregated/",
  pattern = "agg_"
)

p_list <- list.files(
  path = "output/spartan_RData/habitat_pred/",
  pattern = "pred_"
)



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


a_csn <- strsplit(
  x = a_id,
  split = "_"
) %>%
  lapply(
    FUN = function(x){
      x[1:5]
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
        "output/spartan_RData/habitat_pred_aggregated/%s",
        x
      )
    )
    
    return(z)
    
  }
)


aggs <- tibble(
  cscnid = a_csn,
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


p_csn <- strsplit(
  x = p_id,
  split = "_"
) %>%
  lapply(
    FUN = function(x){
      x[1:5]
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
        "output/spartan_RData/habitat_pred/%s",
        x
      )
    )
    
    return(z)
    
  }
)



preds <- tibble(
  cscnid = p_csn,
  sp =  p_sp,
  predmaps = p_maps
)

######


vs <- var_set %>%
  dplyr::select(
    scenario,
    scenario_replicate,
    rcp,
    climate_model,
    harvest_scenario,
    plan_burn,
    scn_id,
    cscnid
  )



pred_set <- full_join(
  x = vs,
  y = preds,
  by = "cscnid"
)

agg_set <- full_join(
  x = vs,
  y = aggs,
  by = "cscnid"
)


# pred_set <- pred_set %>%
#   filter(sp != "pevo") %>%
#   bind_rows(pred_set3)
# 
# agg_set <- agg_set %>%
#   filter(sp != "pevo") %>%
#   bind_rows(agg_set3)

save(
  pred_set,
  agg_set,
  file = "output/RData/10_predict_SDMs.RData"
) 