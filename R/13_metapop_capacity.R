## 13 Metapopulation capacity models


library(dplyr)
library(purrr)
library(tibble)
library(magrittr)
library(raster)
library(sp)
library(sf)
library(metacapa)


load(file = "output/RData/00_comp_controls.RData")
load(file = "output/RData/02_species_occurrences.RData")
load(file = "output/RData/10.1_preds_agg.RData")

source.functions("R/functions")





### LB ---------------------------

dispersal_lb <- dispersal_negexp(1/3000)
  

threshold_lb <- preds_lb_agg$predmaps[[1]][[1]] %>%
  raster::extract(
    y = pa_lb_09b %>% filter(PA == 1)
  ) %>%
  quantile(probs = c(0.1))


metapc <- preds_lb_agg %>%
  select(-variables, -mort, -v_id) %>%
  