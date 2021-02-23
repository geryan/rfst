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


load(file = "output/RData/00_controls_eg.RData")
load(file = "output/RData/01_landscape_variables_eg.RData")
load(file = "output/RData/14.1_sp_occ_metapop_eg.RData")
load(file= "output/RData/17_mpc_predict_sdm_eg.RData")

source.functions("R/functions")



threshold_max_sss <- pred_mpc_eg[1:18,] %$%
  mapply(
    x = predmaps,
    sp = sp,
    FUN = get.threshold.max.sss,
    MoreArgs = list(
      y = pa_data_eg_model
    )
  )

thresholds <- tibble(
  sp = pred_mpc_eg$sp[1:18],
  threshold_max_sss = threshold_max_sss
)


dispersal_dist <- tribble(
  ~sp, ~ddist,
  "liau", 1000,
  "lili", 400,
  "psde", 400,
  "acno", 10000,
  "cala", 10000,
  "cipu", 2000,
  "clpi", 1200,
  "lois", 10000,
  "mecu", 3000,
  "nist", 10000,
  "pero", 2000,
  "pysa", 1400,
  "tyno", 10000,
  "dama", 10000,
  "isob", 500,
  "potr", 300,
  "euma", 100,
  "mosp", 700
)

mpc_dat_eg <- pred_mpc_eg %>%
  full_join(
    y = thresholds,
    by = "sp"
  ) %>%
  full_join(
    y = dispersal_dist,
    by = "sp"
  )


save(
  mpc_dat_eg,
  file = "/data/gpfs/projects/punim0995/rfst/output/RData/18.0_mpc_list_eg.RData"
)
