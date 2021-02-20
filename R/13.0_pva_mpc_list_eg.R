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

load(file = "output/RData/00_controls_eg.RData")
load(file = "output/RData/01_landscape_variables_eg.RData")
load(file = "output/RData/02.1_species_occurrence_eg.RData")
load(file = "output/RData/10_predict_SDMs_eg.RData")

source.functions("R/functions")



threshold_max_sss <- pred_set_eg[1:5,] %$%
  mapply(
    x = predmaps,
    sp = sp,
    FUN = get.threshold.max.sss,
    MoreArgs = list(
      y = pa_data_eg
    )
  )

thresholds <- tibble(
  sp = pred_set_eg$sp[1:5],
  threshold_max_sss = threshold_max_sss
)


dispersal_dist <- tribble(
  ~sp,    ~ddist,
  "polo", 2000,
  "peau", 10000,
  "pevo", 2000,
  "smle", 1000,
  "tyte", 100000,
  "vava", 3000 
)

mpc_dat_pva_eg <- agg_set_eg %>%
  left_join(
    y = thresholds,
    by = "sp"
  ) %>%
  left_join(
    y = dispersal_dist,
    by = "sp"
  )


save(
  mpc_dat_pva_eg,
  file = "/data/gpfs/projects/punim0995/rfst/output/RData/13.0_pva_mpc_list_eg.RData"
)
