# 18 metapopulation capacity models data list

source("R/spartan/spartan_settings.R")

library(magrittr)
library(tibble)
library(dplyr)
library(purrr)
library(tidyr)
library(raster)
library(sp)
library(sf)
library(igraph)
library(metacapa)

load(file = "output/RData/00_controls_eg.RData")
load(file = "output/RData/01_landscape_variables_eg.RData")
load(file = "output/RData/18.0_mpc_list_eg.RData")

source.functions("R/functions")


command_args <- commandArgs(trailingOnly = TRUE)

i <- as.numeric(command_args[1])

agg_map <- raster::aggregate(
  x = mpc_dat_eg$predmaps[[i]],
  fact = 2,
  na.rm = TRUE
)


dispfun <- dispersal_negexp(1/mpc_dat_eg$ddist[i])

patch <- patmat(
  x = agg_map,
  threshold = mpc_dat_eg$threshold_max_sss[i],
  write = FALSE
)

mpc <- metacapstack(
  x = patch,
  f = dispfun,
  year0 = year0
) %>%
  rename(
    mpc = metapopulation_capacity
  )


mpc_results <- bind_cols(
  mpc_dat_eg[i,] %>%
    dplyr::select(-predmaps),
  mpc
)

saveRDS(
  object = mpc_results,
  file = sprintf(
    "%s/mpc_eg_%s_%s.Rds",
    "/data/gpfs/projects/punim1340/rfst_eg/output/spartan_RData/mpc",
    mpc_dat_eg$ycscnid[i], # NB needs cscnid not scn_id if multiple climate scenarios with each scenario
    mpc_dat_eg$sp[i]
  )
)

