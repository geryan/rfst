

source("R/spartan/spartan_settings.R")


library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(raster)
library(sp)
library(gbm)
library(dismo)


load(file = "output/RData/00_controls_eg.RData")
load(file = "output/RData/01_landscape_variables_eg.RData")
load(file = "output/RData/07_combined_variables_eg.RData")
load(file = "output/RData/09_fit_distribution_models_eg.RData")

source.functions("R/functions")

command_args <- commandArgs(trailingOnly = TRUE)

i <- as.numeric(command_args[1])

j <- as.numeric(command_args[2])


# for some reason paths here and below need to be full otherwise script fails to find the directory.

pred_out_path <- "/data/scratch/projects/punim1340/habitat_pred"

agg_out_path <- "/data/gpfs/projects/punim1340/rfst_eg/output/habitat_pred_aggregated"

##############


pred <- brtpredict(
  variables = var_set_eg$all_vars[[i]],
  model = sdm_results_eg$brt.fit[[j]],
  out_path = pred_out_path,
  scn_id = var_set_eg$ycscnid[[i]], # NB needs ycscnid not scn_id if multiple climate scenarios or starting landscapes
  varset = "",
  species = sdm_results_eg$sp[[j]],
  initial = FALSE,
  pll = FALSE,
  ncores = 1
)

saveRDS(
  object = pred,
  file = sprintf(
    "%s/pred_%s_%s.Rds",
    "/data/gpfs/projects/punim1340/rfst_eg/output/spartan_RData/habitat_pred",
    var_set_eg$ycscnid[[i]], # NB needs ycscnid not scn_id if multiple climate scenarios or starting landscapes
  )
)

agg <- maggregate(
  x = pred,
  fact = 2,
  out_path = agg_out_path,
  aggname = "aggpred",
  scn_id = var_set_eg$ycscnid[[i]], # NB needs ycscnid not scn_id if multiple climate scenarios or starting landscapes
  varset = "",
  species = sdm_results_eg$sp[[j]]
)

saveRDS(
  object = agg,
  file = sprintf(
    "%s/agg_%s_%s.Rds",
    "/data/gpfs/projects/punim1340/rfst_eg/output/spartan_RData/habitat_pred_aggregated",
    var_set_eg$ycscnid[[i]], # NB needs ycscnid not scn_id if multiple climate scenarios or starting landscapes
    sdm_results_eg$sp[[j]]
  )
)

