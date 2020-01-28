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
library(future.apply)

load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")
load(file = "output/RData/02_species_occurrences.RData")
load(file = "output/RData/07_combined_variables.RData")

source.functions("R/functions")

#plan(multisession, workers = 20)


varlist <- c(
  "date",         # 01
  "PA",           # 02
  "lbm_prop",     # 03
  "lbm_biom",     # 04
  "ac_prop",      # 05
  "ac_biom",      # 06
  "ggf_prop",     # 07
  "ggf_biom",     # 08
  "ggd_prop",     # 09
  "ggd_prop_og",  # 10
  "ggd_biom",     # 11
  "ggd_biom_og",  # 12
  "prop_bio_regn",# 13
  "prop_bio_targ",# 14
  "prop_old_150", # 15
  "prop_old_200", # 16
  "prop_oge",     # 17
  "prop_oge_3h",  # 18
  "prop_oge_1k",  # 19
  "biom_oge",     # 20
  "biom_oge_3h",  # 21
  "biom_oge_1k",  # 22
  "harvest",      # 23
  "firesev",      # 24
  "max_age",      # 25
  "pb",           # 26
  "fi",           # 27
  "lo",           # 28
  "fihi",         # 29
  "lohi",         # 30
  "tsf",          # 31
  "tsl",          # 32
  "mort",         # 33
  "prec_djf_2019",# 34
  "prec_jja_2019",# 35
  "tmax_djf_2019",# 36
  "tmin_jja_2019",# 37
  "lvdaw",        # 38
  "lvdma",        # 39
  "lvdmi",        # 40
  "lvdsw",        # 41
  "ahr",          # 42
  "tho"           # 43
)


if(!all(colnames(distribution_model_data$dist_mod_dat[[1]]) == varlist)){
  stop("Varlist definition and variable layers are different")
}

sp_sdm_vars <- tribble(
  ~sp, ~ sdm_vars,
  "gyle", varlist[c(3:8, 10, 12, 17:22, 34:43)],
  "pevo", varlist[c(3:8, 10, 12, 17:22, 34:43)],
  "peau", varlist[c(3:8, 10, 12, 17:22, 34:43)],
  "smle", varlist[c(3:8, 10, 12, 17:22, 34:43)],
  "tyte", varlist[c(3:8, 10, 12, 17:22, 34:43)],
  "vava", varlist[c(3:8, 10, 12, 17:22, 34:43)]
)

sdm_data <- full_join(
  x = distribution_model_data,
  y = sp_sdm_vars,
  by = "sp"
)

# LBP -----------------


sdm_gyle <- sdm_data %>%
  filter(sp == "gyle") %>%
  mutate(
    model_data = map2(
      .x = dist_mod_dat,
      .y = sdm_vars,
      .f = clean.model.data,
      na.omit = TRUE
    )
  ) %>%
  mutate(
    brt.fit = map(
      .x = model_data,
      .f = gbmstep,
      tree.complexity = 5,
      learning.rate = 0.005,
      step.size = 1,
      bag.fraction = 0.5,
      prev.stratify = FALSE,
      verbose = TRUE,
      max.trees = 2000
    )
  ) %>%
  mutate(auc = future_map(.x = brt.fit,
                          .f = brt_auc)) %>%
  unnest(auc) %>%
  mutate(trees = map(.x = brt.fit, .f = ~ .$gbm.call$best.trees)) %>%
  unnest(trees)



  
# -----------

save(
  brt_lb,
  brt_gg,
  file = "output/RData/09_fit_distribution_models.RData"
)