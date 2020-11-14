# 17 predict SDMs for metapopulation capacity models 

source("R/spartan/spartan_settings.R")


library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(raster)
library(sp)
library(gbm)
library(dismo)
library(sf)

load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")
load(file = "output/RData/07a_varset_mpc.RData")
load(file = "output/RData/14.1_sp_occ_metapop.RData")

source.functions("R/functions")

p_list <- list.files("/data/gpfs/projects/punim0995/rfst/output/spartan_RData/mpc_pred/")

p_id <- sub(
  pattern = "pred_mpc_",
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
        "/data/gpfs/projects/punim0995/rfst/output/spartan_RData/mpc_pred/%s",
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


pred_mpc <- full_join(
  x = varset_mpc %>%
    dplyr::select(
      "scenario",
      "scenario_replicate",
      "rcp",
      "climate_model",
      "harvest_scenario",
      "plan_burn",
      "scn_id",
      "th",
      "rc",
      "pb",
      "scn_no",
      "cscnid"
    ),
  y = preds,
  by = "cscnid"
)


save(
  pred_mpc,
  file =  "/data/gpfs/projects/punim0995/rfst/output/RData/17_mpc_predict_sdm.RData"
)









