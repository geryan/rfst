## 13 Metapopulation capacity models


library(steps)
library(foreach)
library(doMC)
library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(magrittr)
library(future)
library(future.apply)
library(raster)
library(sp)


load(file = "output/RData/00_comp_controls.RData")
load(file = "output/RData/02_species_occurrences.RData")
load(file = "output/RData/10.1_preds_agg.RData")

source.functions("R/functions")


metacapstack <- function(x){
  
}


### LB ---------------------------

dispersal_lb <- dispersal_negexp(1/3000)
  

threshold_lb <- preds_lb_agg$predmaps[[1]][[1]] %>%
  raster::extract(
    y = pa_lb_09b %>% filter(PA == 1)
  ) %>%
  quantile(probs = c(0.1))


metapc <- preds_lb_agg %>%
  select(-variables, -mort, -v_id) %>%
  