

source("R/spartan/spartan_settings.R")


library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(future)
library(future.apply)
library(raster)
library(sp)
library(gbm)
library(dismo)


load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")
load(file = "output/RData/07_combined_variables.RData")
load(file = "output/RData/09_fit_distribution_models.RData")

source.functions("R/functions")

command_args <- commandArgs(trailingOnly = TRUE)

i <- as.numeric(command_args[1])

j <- as.numeric(command_args[2])

pred_out_path <- "/data/scratch/projects/punim0995/habitat_pred/"

agg_out_path <- "/output/habitat_pred_aggregated/"

##############


pred <- brtpredict(
  variables = var_set$all_vars[[i]],
  model = sdm_results$brt.fit[[j]],
  out_path = pred_out_path,
  scn_id = var_set$cscnid[[i]], # NB needs cscnid not scn_id if multiple climate scenarios with each scenario
  varset = "",
  species = sdm_results$sp[[j]],
  initial = FALSE,
  pll = FALSE,
  ncores = 1
)

saveRDS(
  object = pred,
  file = sprintf(
    "%s/pred_%s_%s.Rds",
    "/data/gpfs/projects/punim0995/rfst/output/spartan_RData/habitat_pred",
    var_set$cscnid[[i]], # NB needs cscnid not scn_id if multiple climate scenarios with each scenario
    sdm_results$sp[[j]]
  )
)

agg <- maggregate(
  x = pred,
  fact = 10,
  out_path = agg_out_path,
  aggname = "aggpred",
  scn_id = var_set$cscnid[[i]], # NB needs cscnid not scn_id if multiple climate scenarios with each scenario
  varset = "",
  species = sdm_results$sp[[j]]
)

saveRDS(
  object = agg,
  file = sprintf(
    "%s/agg_%s_%s.Rds",
    "/data/gpfs/projects/punim0995/rfst/output/spartan_RData/habitat_pred_aggregated",
    var_set$cscnid[[i]], # NB needs cscnid not scn_id if multiple climate scenarios with each scenario
    sdm_results$sp[[j]]
  )
)

