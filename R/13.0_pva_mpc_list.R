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

load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")
load(file = "output/RData/02_species_occurrences.RData")
load(file = "output/RData/10_predict_SDMs.RData")

source.functions("R/functions")



threshold_max_sss <- pred_set[1:6,] %$%
  mapply(
    x = predmaps,
    sp = sp,
    FUN = get.threshold.max.sss,
    MoreArgs = list(
      y = pa_data
    )
  )

thresholds <- tibble(
  sp = pred_set$sp[1:6],
  threshold_max_sss = threshold_max_sss
)


dispersal_dist <- tribble(
  ~sp,    ~ddist,
  "gyle", 3000,
  "peau", 10000,
  "pevo", 2000,
  "smle", 1000,
  "tyte", 10000,
  "vava", 3000 
)

mpc_dat_pva <- agg_set %>%
  full_join(
    y = thresholds,
    by = "sp"
  ) %>%
  full_join(
    y = dispersal_dist,
    by = "sp"
  )


save(
  mpc_dat_pva,
  file = "/data/gpfs/projects/punim0995/rfst/output/RData/13.0_pva_mpc_list.RData"
)
