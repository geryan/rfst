# 09 fit distribution models

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
load(file = "output/RData/02.1_species_occurrence_eg.RData")
load(file = "output/RData/07_combined_variables_eg.RData")
load(file = "output/RData/08_distribution_model_data_eg.RData")

source.functions("R/functions")

#plan(multisession)


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




if(!all(colnames(distribution_model_data_eg$dist_mod_dat[[1]]) == varlist)){
  stop("Varlist definition and variable layers are different")
}

sp_sdm_vars <- tribble(
  ~sp, ~ sdm_vars,
  #"polo", varlist[c(3:5, 7, 10, 14, 23:34)],
  "pevo", varlist[c(3:5, 7, 10, 14, 23:29, 31:32)]#,
  #"peau", varlist[c(3:5, 7, 10, 14, 23:34)],
  #"smle", varlist[c(3:5, 7, 10, 14, 23:34)],
  #"tyte", varlist[c(3:5, 7, 10, 14, 23:34)],
  #"vava", varlist[c(3:5, 7, 10, 14, 23:34)]
)

sdm_data <- full_join(
  x = distribution_model_data_eg,
  y = sp_sdm_vars,
  by = "sp"
) %>%
  mutate(
    weights = map(
      .x = pa_dat,
      .f = function(x){
        z <- x %>%
          group_by(PA) %>%
          mutate(
            npa = n()
          ) %>%
          ungroup %>%
          mutate(
            sump = length(which(PA == 1))
          ) %>%
          mutate(
            weight = sump/npa
          )
        
        result <- z$weight
        
        return(result)
      }
    )
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
      learning.rate = 0.03,
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

# GG  -----------------
sdm_pevo <- sdm_data %>%
  filter(sp == "pevo") %>%
  mutate(
    model_data = map2(
      .x = dist_mod_dat,
      .y = sdm_vars,
      .f = clean.model.data,
      na.omit = TRUE
    )#,
    # model_data = map(
    #   .x = model_data,
    #   .f = function(x){
    #     
    #     z <- x %>%
    #       group_by(PA) %>%
    #       mutate(
    #         npa = n()
    #       ) %>%
    #       ungroup %>%
    #       mutate(
    #         sump = length(which(PA == 1))
    #       ) %>%
    #       mutate(
    #         site.weights = sump/npa
    #       ) %>%
    #       dplyr::select(
    #         -npa,
    #         -sump
    #       )
    #     
    #     return(z) 
    #   
    #   }
    # )
  ) %>%
  mutate(
    brt.fit = map(
      .x = model_data,
      .f = gbmstep,
      tree.complexity = 4,
      learning.rate = 0.01,
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



# YBG -----------------
sdm_peau <- sdm_data %>%
  filter(sp == "peau") %>%
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
      learning.rate = 0.02,
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

# WFD -----------------
sdm_smle <- sdm_data %>%
  filter(sp == "smle") %>%
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
      tree.complexity = 7,
      learning.rate = 0.002,
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

# SO  -----------------
sdm_tyte <- sdm_data %>%
  filter(sp == "tyte") %>%
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
      tree.complexity = 7,
      learning.rate = 0.005,
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

# LM  -----------------
sdm_vava <- sdm_data %>%
  filter(sp == "vava") %>%
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
      tree.complexity = 7,
      learning.rate = 0.003,
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

# -----------

sdm_results <- bind_rows(
  #sdm_gyle,
  sdm_pevo#,
  #sdm_peau,
  #sdm_smle,
  #sdm_tyte,
  #sdm_vava
)

# -----------
save(
  sdm_results,
  file = "output/RData/09_fit_distribution_models_eg.RData"
)