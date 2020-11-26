# 10.1 aggregate sdm

source("R/spartan/spartan_settings.R")


library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(raster)
library(sp)
library(sf)
library(magrittr)

load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")
load(file = "output/RData/10_predict_SDMs.RData")


source.functions("R/functions")

command_args <- commandArgs(trailingOnly = TRUE)

i <- as.numeric(command_args[1])


pred <- pred_set$predmaps[[i]]

agg_out_path <- "/data/gpfs/projects/punim0995/rfst/output/habitat_pred_aggregated/"


agg <- maggregate(
  x = pred,
  fact = 5,
  out_path = agg_out_path,
  aggname = "aggpred5",
  scn_id = pred_set$cscnid[[i]], # NB needs cscnid not scn_id if multiple climate scenarios with each scenario
  varset = "",
  species = pred_set$sp[[i]]
)


aggtable <- bind_cols(
  pred_set[i,] %>%
    dplyr::select(-predmaps),
  tibble(aggmap5 = list(agg))
)


saveRDS(
  object = aggtable,
  file = sprintf(
    "%s/agg5_%s_%s.Rds",
    "/data/gpfs/projects/punim0995/rfst/output/spartan_RData/habitat_pred_aggregated",
    pred_set$cscnid[[i]], # NB needs cscnid not scn_id if multiple climate scenarios with each scenario
    pred_set$sp[[i]]
  )
)

