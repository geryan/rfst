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

load(file = "output/RData/00_controls_eg.RData")
load(file = "output/RData/01_landscape_variables_eg.RData")
load(file = "output/RData/07a_varset_mpc_eg.RData")
load(file = "output/RData/14.1_sp_occ_metapop_eg.RData")

source.functions("R/functions")

p_list <- list.files(
  path = "/data/gpfs/projects/punim1340/rfst_eg/output/spartan_RData/mpc_pred/",
  pattern = "pred_mpc"
)

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


p_ycscn <- strsplit(
  x = p_id,
  split = "_"
) %>%
  lapply(
    FUN = function(x){
      x[2:7]
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
        "/data/gpfs/projects/punim1340/rfst_eg/output/spartan_RData/mpc_pred/%s",
        x
      )
    )
    
    return(z)
    
  }
)

preds <- tibble(
  ycscnid = p_ycscn,
  sp =  p_sp,
  predmaps = p_maps
)


pred_mpc_eg <- full_join(
  x = varset_mpc_eg %>%
    dplyr::select(
      "scenario",
      "scenario_replicate",
      "rcp",
      "climate_model",
      "yearid",
      "harvest_scenario",
      "plan_burn",
      "yscn_id",
      "scn_id",
      "th",
      "rc",
      "pb",
      "scn_no",
      "cscnid",
      "ycscnid"
    ),
  y = preds,
  by = "ycscnid"
) %>% 
  filter(!is.na(sp))


save(
  pred_mpc_eg,
  file =  "/data/gpfs/projects/punim0995/rfst/output/RData/17_mpc_predict_sdm_eg.RData"
)









