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



load(file = "output/RData/00_controls_eg.RData")
load(file = "output/RData/01_landscape_variables_eg.RData")
load(file = "output/RData/15_distribution_model_data_mpc_eg.RData")


source.functions("R/functions")

command_args <- commandArgs(trailingOnly = TRUE)

i <- as.numeric(command_args[1])

varlist <- c(
  "date",         # 01
  "PA",           # 02
  "prop_bio_regn",# 03
  "prop_old_150", # 04
  "prop_old_200", # 05
  "prop_oge",     # 06
  "prop_oge_3h",  # 07
  "prop_oge_1k",  # 08
  "biom_oge",     # 09
  "biom_oge_3h",  # 10
  "biom_oge_1k",  # 11
  "harvest",      # 12
  "firesev",      # 13
  "max_age",      # 14
  "pb",           # 15
  "fi",           # 16
  "lo",           # 17
  "fihi",         # 18
  "lohi",         # 19
  "tsf",          # 20
  "tsl",          # 21
  "mort",         # 22
  "prec_djf_2019",# 23
  "prec_jja_2019",# 24
  "tmax_djf_2019",# 25
  "tmin_jja_2019",# 26
  "lvdaw",        # 27
  "lvdma",        # 28
  "lvdmi",        # 29
  "lvdsw",        # 30
  "ahr",          # 31
  "tho",          # 32
  "lon",          # 33
  "lat"           # 34
)



if(!all(colnames(distribution_model_data_mpc_eg$dist_mod_dat[[1]]) == varlist)){
  stop("Varlist definition and variable layers are different")
}

# sdm_pars <- tibble::tribble(
#      ~sp,   ~lr,
#   "grba", 0.010,
# )

sdm_data <- distribution_model_data_mpc_eg[i,] %>%
  mutate(
    sdm_vars = case_when(
      #sp == "grba" ~ varlist[c(6, 15, 17, 20, 25, 34:45)] %>% list,
      TRUE ~ varlist[c(6:11, 14, 23:34)] %>% list
    ),
    lr = case_when(
      sp == "cala" ~ 0.002,
      sp == "cipu" ~ 0.005,
      sp == "clpi" ~ 0.002,
      sp == "dama" ~ 0.003,
      sp == "euma" ~ 0.004,
      sp == "isob" ~ 0.008,
      sp == "liau" ~ 0.002,
      sp == "lili" ~ 0.0002,
      sp == "mecu" ~ 0.0025,
      sp == "mosp" ~ 0.008,
      sp == "nist" ~ 0.005,
      sp == "potr" ~ 0.008,
      sp == "psde" ~ 0.006,
      sp == "pysa" ~ 0.002,
      sp == "tyno" ~ 0.005,
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
    "%s/sdm_fit_eg_%s.Rds",
    "/data/gpfs/projects/punim0995/rfst/output/spartan_RData/sdm_fit_mpc/eg",
    distribution_model_data_mpc_eg$sp[i]
  )
)