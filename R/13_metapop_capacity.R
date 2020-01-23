## 13 Metapopulation capacity models


library(dplyr)
library(tibble)
library(magrittr)
library(tidyr)
library(raster)
library(sp)
library(sf)
library(metacapa)
library(future)
library(future.apply)

load(file = "output/RData/00_controls.RData")
load(file = "output/RData/02_species_occurrences.RData")
load(file = "output/RData/10.1_preds_agg.RData")

source.functions("R/functions")


### LB ---------------------------

dispersal_lb <- dispersal_negexp(1/3000)
  

threshold_lb <- preds_lb_agg$predmaps[[1]][[1]] %>%
  raster::extract(
    y = pa_lb_09b %>% filter(PA == 1)
  ) %>%
  quantile(probs = c(0.1)) %>%
  signif(digits = 3)


patch_matrix_lb <- preds_lb_agg %$%
  mapply(
    FUN = patmat,
    x = predmaps,
    scn_id = scn_id,
    MoreArgs = list(
      threshold = threshold_lb,
      out_path = "output/patch_matrix",
      varset = "",
      species = "lb"
    )
  )


plan(multisession, workers = ncores)

mpc_lb <- future_lapply(
    X = patch_matrix_lb,
    FUN = metacapstack,
    f = dispersal_lb,
    future.packages = c("raster", "sp", "rgeos", "metacapa", "tibble")
  ) 


plan(sequential)

metapc_lb  <- bind_cols(
  preds_lb_agg,
  tibble(
    patch_matrix = patch_matrix_lb,
    mpc = mpc_lb
  )
) %>%
  dplyr::select(-v_id, -variables, -predmaps, -mort) %>%
  mutate(
    scenario = case_when(scenario == 1 ~ "BAU",
                         scenario == 4 ~ "No harvest",
                         scenario == 8 ~ "Cease harvest Y30")
  )


metapc_long_lb <- metapc_lb %>%
dplyr::select(-scn_id, -patch_matrix) %>%
  unnest(cols = mpc)


### GG ---------------------------

dispersal_gg <- dispersal_negexp(1/5000)


threshold_gg <- preds_gg_agg$predmaps[[1]][[1]] %>%
  raster::extract(
    y = pa_gg_09b %>% filter(PA == 1)
  ) %>%
  quantile(probs = c(0.1)) %>%
  signif(digits = 3)


patch_matrix_gg <- preds_gg_agg %$%
  mapply(
    FUN = patmat,
    x = predmaps,
    scn_id = scn_id,
    MoreArgs = list(
      threshold = threshold_gg,
      out_path = "output/patch_matrix",
      varset = "",
      species = "gg"
    )
  )

plan(multisession, workers = ncores)

mpc_gg <- future_lapply(
  X = patch_matrix_gg,
  FUN = metacapstack,
  f = dispersal_gg,
  future.packages = c("raster", "sp", "rgeos", "metacapa", "tibble")
) 


plan(sequential)

metapc_gg  <- bind_cols(
  preds_gg_agg,
  tibble(
    patch_matrix = patch_matrix_gg,
    mpc = mpc_gg
  )
) %>%
  dplyr::select(-v_id, -variables, -predmaps, -mort) %>%
  mutate(
    scenario = case_when(scenario == 1 ~ "BAU",
                         scenario == 4 ~ "No harvest",
                         scenario == 8 ~ "Cease harvest Y30")
  )


metapc_long_gg <- metapc_gg %>%
  dplyr::select(-scn_id, -patch_matrix) %>%
  unnest(cols = mpc)

##### save  -------------------

save(
  metapc_lb,
  metapc_long_lb,
  metapc_gg,
  metapc_long_gg,
  file = "output/RData/13_metapopulation_capacity.RData"
)




