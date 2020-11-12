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

load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")
load(file = "output/RData/07a_varset_mpc.RData")
#load(file = "output/RData/14.1_sp_occ_metapop.RData")
load(file = "output/RData/16_mpc_fit_sdm.RData")

source.functions("R/functions")

command_args <- commandArgs(trailingOnly = TRUE)

i <- as.numeric(command_args[1])

j <- as.numeric(command_args[2])


# for some reason paths here and below need to be full otherwise script fails to find the directory.

pred_out_path <- "/data/scratch/projects/punim0995/habitat_pred_mpc/"


pred <- brtpredict(
  variables = varset_mpc$all_vars[[i]],
  model = sdm_results_mpc_ch$brt.fit[[j]],
  out_path = pred_out_path,
  scn_id = varset_mpc$cscnid[[i]], # NB needs cscnid not scn_id if multiple climate scenarios with each scenario
  varset = "",
  species = sdm_results_mpc_ch$sp[[j]],
  initial = FALSE,
  pll = FALSE,
  ncores = 1
)



saveRDS(
  object = pred,
  file = sprintf(
    "%s/pred_mpc_%s_%s.Rds",
    "/data/gpfs/projects/punim0995/rfst/output/spartan_RData/mpc_pred",
    varset_mpc$cscnid[[i]], # NB needs cscnid not scn_id if multiple climate scenarios with each scenario
    sdm_results_mpc_ch$sp[[j]]
  )
)
