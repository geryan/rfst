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
#library(sf)

load(file = "output/RData/00_controls_eg.RData")
load(file = "output/RData/01_landscape_variables_eg.RData")
load(file = "output/RData/07a_varset_mpc_eg.RData")
load(file = "output/RData/16_mpc_fit_sdm_eg.RData")

source.functions("R/functions")

command_args <- commandArgs(trailingOnly = TRUE)

i <- as.numeric(command_args[1])

j <- as.numeric(command_args[2])


# for some reason paths here and below need to be full otherwise script fails to find the directory.

pred_out_path <- "/data/scratch/projects/punim1340/habitat_pred_mpc/"

# file_list <- list.files("/data/scratch/projects/punim0995/habitat_pred_mpc/")

# if(
#   any(
#     file_list == sprintf(
#       "brtpred_%s_%s_%s.grd",
#       varset_mpc$cscnid[[i]],
#       "",
#       sdm_results_mpc_ch$sp[[j]]
#     )
#   )
# ){
#   
#   print("exists")
#   
#   # this if else allows it to skip ones that are already made and do ones that still need to be done
#   
# } else {
  
  pred <- brtpredict(
    variables = varset_mpc_eg$all_vars[[i]],
    model = sdm_results_mpc_eg$brt.fit[[j]],
    out_path = pred_out_path,
    scn_id = varset_mpc$ycscnid[[i]], # NB needs cscnid not scn_id if multiple climate scenarios with each scenario
    varset = "",
    species = sdm_results_mpc_eg$sp[[j]],
    initial = FALSE,
    pll = FALSE,
    ncores = 1
  )
  
  
  
  saveRDS(
    object = pred,
    file = sprintf(
      "%s/pred_mpc_eg_%s_%s.Rds",
      "/data/gpfs/projects/punim1340/rfst_eg/output/spartan_RData/mpc_pred",
      varset_mpc_eg$ycscnid[[i]], # NB needs cscnid not scn_id if multiple climate scenarios with each scenario
      sdm_results_mpc_eg$sp[[j]]
    )
  )
  
# }
  