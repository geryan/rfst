# 16 fit distribution models pva

source("R/spartan/spartan_settings.R")

library(raster)
library(dplyr)
library(sf)
library(lubridate)
library(purrr)
library(tibble)
#library(future)
#library(furrr)
library(tidyr)
#library(future.apply)
library(dismo)
library(gbm)



load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")
load(file = "output/RData/15_distribution_model_data_mpc.RData")


source.functions("R/functions")

command_args <- commandArgs(trailingOnly = TRUE)

i <- as.numeric(command_args[1])


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
  "tho",          # 43
  "lon",          # 44
  "lat"           # 45
)




if(!all(colnames(distribution_model_data_mpc$dist_mod_dat[[1]]) == varlist)){
  stop("Varlist definition and variable layers are different")
}

# sdm_pars <- tibble::tribble(
#      ~sp,   ~lr,
#   "grba", 0.010,
# )

sdm_data <- distribution_model_data_mpc[i,] %>%
  mutate(
    sdm_vars = case_when(
      sp == "grba" ~ varlist[c(6, 15, 17, 20, 25, 34:45)] %>% list,
      TRUE ~ varlist[c(3:8, 10, 12, 17:22, 25, 34:45)] %>% list
    ),
    lr = case_when(
      sp == "grba" ~ 0.005,
      sp == "acno" ~ 0.0005,
      TRUE ~ 0.001
    )
  )

# sdm_data <- left_join(
#   x = sdm_data,
#   y = sdm_pars,
#   by = "sp"
# )

sdm_fit <- sdm_data %>%
  mutate(
    model_data = map2(
      .x = dist_mod_dat,
      .y = sdm_vars,
      .f = clean.model.data,
      na.omit = TRUE
    )
  ) %>%
  mutate(
    brt.fit = pmap(
      .l = list(
        data = model_data,
        learning.rate = lr
      ),
      .f = gbmstep,
      tree.complexity = 5,
      #learning.rate = 0.001,
      #step.size = 1,
      bag.fraction = 0.5,
      prev.stratify = TRUE,
      verbose = FALSE,
      max.trees = 5000
    )
  ) %>%
  mutate(auc = map(.x = brt.fit,
                          .f = brt_auc)) %>%
  unnest(auc) %>%
  mutate(trees = map(.x = brt.fit, .f = ~ .$gbm.call$best.trees)) %>%
  unnest(trees)


# sdm_fit
# 
# predi <- brtpredict(
#   variables = varset_mpc$all_vars[[1]],
#   model = sdm_fit$brt.fit[[1]],
#   scn_id = "TH19_rcp45_PB_01_ACCESS1-0_init",
#   varset = "",
#   species = "test",
#   initial = TRUE
# )
# 
# plot(predi)
# 
# predf <- brtpredict(
#   variables = varset_mpc$all_vars[[1]][c(1, 10, 50)],
#   model = sdm_fit$brt.fit[[1]],
#   out_path = "/data/gpfs/projects/punim0995/rfst/output/",
#   scn_id = "TH19_rcp45_PB_01_ACCESS1-0",
#   varset = "",
#   species = "test",
#   initial = FALSE,
#   pll = FALSE,
#   ncores = 1
# )
# 
# plot(predf)


saveRDS(
  object = sdm_fit,
  file = sprintf(
    "%s/sdm_fit_ch_%s.Rds",
    "/data/gpfs/projects/punim0995/rfst/output/spartan_RData/sdm_fit_mpc/ch",
    distribution_model_data_mpc$sp[i]
  )
)