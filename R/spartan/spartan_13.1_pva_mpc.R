# 13 metapopulation capacity models data list for PVA species

source("R/spartan/spartan_settings.R")


library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(raster)
library(sp)
library(sf)
library(magrittr)
library(igraph)
library(metacapa)

load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")
load(file = "output/RData/13.0_pva_mpc_list.RData")

source.functions("R/functions")


command_args <- commandArgs(trailingOnly = TRUE)

i <- as.numeric(command_args[1])


agg_map <- mpc_dat_pva$aggmaps[[i]]

dispfun <- dispersal_negexp(1/mpc_dat_pva$ddist[i])

patch <- patmat(
  x = agg_map,
  threshold = mpc_dat_pva$threshold_max_sss[i],
  write = FALSE
)

mpc <- metacapstack(
  x = patch,
  f = dispfun,
  year0 = year0
) %>%
  rename(
    mpc_90 = metapopulation_capacity
  )


mpc_results_pva <- bind_cols(
  mpc_dat_pva[i,],
  mpc
)

saveRDS(
  object = mpc_results_pva,
  file = sprintf(
    "%s/mpc_pva_%s_%s.Rds",
    "/data/gpfs/projects/punim0995/rfst/output/spartan_RData/mpc_pva",
    mpc_dat_pva$cscnid[i], # NB needs cscnid not scn_id if multiple climate scenarios with each scenario
    mpc_dat_pva$sp[i]
  )
)

