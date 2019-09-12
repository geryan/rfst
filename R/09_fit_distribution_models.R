# 09 fit distribution models

library(raster)
library(dplyr)
library(sf)
library(lubridate)
library(purrr)
library(tibble)
library(future)
library(furrr)

load(file = "output/RData/00_comp_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")
load(file = "output/RData/02_species_occurrences.RData")
load(file = "output/RData/07_combined_variables.RData")

source.functions("R/functions")

# LBP -----------------

plan(multisession, workers = 8)

brt_lb <- tibble(year.from = c("80", "09"),
                 bg = c(TRUE, FALSE),
                 varset = list(vn_lb_1, vn_lb_2)) %>%
  expand(year.from, bg, varset) %>%
  rowwise %>%
  mutate(pa.data = list(get(sprintf("pa_lb_%S%s", year.from, ifelse(bg, "", "x"))))) %>%
  ungroup %>%
  mutate(model.data = future_map(.x = pa.data,
                          .f = get.model.data,
                          y = vars_1_01,
                          na.omit = FALSE),
         model.data = future_map2(.x = model.data,
                           .y = varset,
                           .f = clean.model.data)) %>%
  mutate(brt.fit = future_map(.x = model.data,
                       .f = gbmstep,
                       learning.rate = 0.01,
                       bag.fraction = 0.75)) %>%
  mutate(auc = future_map(.x = brt.fit,
                   .f = brt_auc))

save(
  brt_lb,
  file = "output/RData/09_fit_distribution_models.RData"
)