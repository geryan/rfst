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

load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")
load(file = "output/RData/04.1_mortality_aggregated5.RData")
load(file = "output/RData/04.1_mortality_aggregated.RData")
load(file = "output/RData/04.3_tsl_aggregated.RData")
load(file = "output/RData/10.1_aggregate_sdm.RData")
load(file = "output/RData/10_predict_SDMs_agg.RData")
load(file = "output/RData/11.0_pva_species_dat.RData")

source.functions("R/functions")

hab_set <- full_join(
  agg_set,
  agg5_ch,
  by = colnames(agg_set)[1:9]
) %>%
  full_join(
    y = species_dat_pva,
    by = "sp"
  ) %>%
  mutate(
    hab_map = pmap(
      .l = list(
        scale,
        aggmaps,
        aggmap5
      ),
      .f = function(
        scale,
        aggmaps,
        aggmap5
      ){
        if(scale == 1000){
          return(aggmaps)
        } else if(scale == 500){
          return(aggmap5)
        }
      }
    )
  ) %>%
  full_join(
    y = mort_agg_ch,
    by = "scn_id"
  ) %>%
  full_join(
    y = mort_agg5_ch,
    by = "scn_id"
  ) %>%
  mutate(
    mort_map = pmap(
      .l = list(
        scale,
        mort_agg.x,
        mort_agg.y
      ),
      .f = function(
        scale,
        mort_agg.x,
        mort_agg.y
      ){
        if(scale == 1000){
          return(mort_agg.x)
        } else if(scale == 500){
          return(mort_agg.y)
        }
      }
    )
  ) %>%
  full_join(
    y = tsl_agg_ch,
    by = "scn_id"
  ) %>%
  mutate(
    dist_map = pmap(
      .l = list(
        scale,
        tsl_agg,
        tsl_agg5
      ),
      .f = function(
        scale,
        tsl_agg,
        tsl_agg5
      ){
        if(scale == 1000){
          return(tsl_agg)
        } else if(scale == 500){
          return(tsl_agg5)
        }
      }
    )
  ) %>%
  dplyr::select(-aggmaps, - aggmap5, -mort_agg.x, -mort_agg.y, -tsl_agg, -tsl_agg5)


save(
  hab_set,
  file = "output/RData/11.0.1_hab_set.RData"
)

