# 09 fit distribution models

library(raster)
library(dplyr)
library(sf)
library(lubridate)
library(purrr)
library(tibble)
library(future)
library(furrr)
library(tidyr)

load(file = "output/RData/00_comp_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")
load(file = "output/RData/02_species_occurrences.RData")
load(file = "output/RData/07_combined_variables.RData")

source.functions("R/functions")

# -------------------

vn_lb_1 <- c("lbm_prop", "lbm_biom", "prop_old_150", "prop_oge_3h", "biom_oge_3h", "prec01", "prec07", "tmax01", "tmin07", "lvdaw", "lvdma", "lvdmi", "lvdsw", "ahr", "tho", "max_age")
vn_lb_2 <- c("prec01", "prec07", "tmax01", "tmin07", "lvdaw", "lvdma", "lvdmi", "lvdsw", "ahr", "tho", "max_age")

vn_gg_1 <- c( "ggf_prop", "ggf_biom", "ggd_prop_og", "ggd_biom_og","prop_old_150", "prop_oge_1k", "biom_oge_1k", "prec01", "prec07", "tmax01", "tmin07", "lvdaw", "lvdma", "lvdmi", "lvdsw", "ahr", "tho", "max_age")
vn_gg_2 <- c("prec01", "prec07", "tmax01", "tmin07", "lvdaw", "lvdma", "lvdmi", "lvdsw", "ahr", "tho", "max_age")


plan(multisession, workers = 8)

# LBP -----------------


brt_lb <- tibble(year.from = c("80", "09"),
                 bg = c(TRUE, FALSE),
                 varset = list(vn_lb_1, vn_lb_2)) %>%
  expand(year.from, bg, varset) %>%
  rowwise %>%
  mutate(pa.data = list(get(sprintf("pa_lb_%s%s", year.from, ifelse(bg, "b", "x"))))) %>%
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
                   .f = brt_auc))%>%
  unnest(auc)

# GG ------------------

brt_gg <- tibble(year.from = c("80", "09"),
                 bg = c(TRUE, FALSE),
                 varset = list(vn_gg_1, vn_gg_2)) %>%
  expand(year.from, bg, varset) %>%
  rowwise %>%
  mutate(pa.data = list(get(sprintf("pa_gg_%s%s", year.from, ifelse(bg, "b", "x"))))) %>%
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
                          .f = brt_auc))%>%
  unnest(auc)

# -----------

save(
  brt_lb,
  brt_gg,
  file = "output/RData/09_fit_distribution_models.RData"
)