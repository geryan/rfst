## 11.0.1_hab_set

source("R/spartan/spartan_settings.R")

library(magrittr)
library(tibble)
library(tidyr)
library(ggplot2)
library(dplyr)
library(purrr)
library(raster)
library(sp)
library(steps)


load(file = "output/RData/00_controls_eg.RData")
load(file = "output/RData/01_landscape_variables_eg.RData")
load(file = "output/RData/04.1_mortality_aggregated4_eg.RData")
load(file = "output/RData/04.3_tsl_aggregated_eg.RData")
load(file = "output/RData/10_predict_SDMs_agg_eg.RData")
load(file = "output/RData/11.0_pva_species_dat_eg.RData")

source.functions("R/functions")

hab_set_eg <- agg_set_eg %>%
  left_join(
    y = species_dat_pva_eg,
    by = "sp"
  ) %>%
  mutate(
    hab_map = aggmaps
  ) %>%
  full_join(
    y = mort_agg4_eg,
    by = "yscn_id"
  ) %>%
  mutate(
    mort_map = mort_agg
  ) %>%
  full_join(
    y = tsl_agg_eg,
    by = "yscn_id"
  ) %>%
  mutate(
    dist_map = tsl_agg
  ) %>%
  dplyr::select(-aggmaps, -mort_agg, - tsl_agg)


save(
  hab_set_eg,
  file = "output/RData/11.0.1_hab_set_eg.RData"
)

