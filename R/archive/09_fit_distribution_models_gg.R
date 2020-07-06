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
plan(sequential)

# GG ------------------

brt_gg_bb <- tibble(year.from = c("09"),
                 bg = c(TRUE),
                 varset = list(vn_gg_1),
                 lr = c("0.010"),
                 bf = c("0.50")) %>%
  expand(year.from, bg, varset, lr, bf) %>%
  mutate(lrbf = paste0(lr, bf)) %>%
  rowwise %>%
  mutate(pa.data = list(get(sprintf("pa_gg_%s%s", year.from, ifelse(bg, "bb", "x"))))) %>%
  ungroup %>%
  mutate(model.data = future_map(.x = pa.data,
                                 .f = get.model.data,
                                 y = vars_1_01,
                                 na.omit = FALSE),
         model.data = future_map2(.x = model.data,
                                  .y = varset,
                                  .f = clean.model.data)) %>%
  mutate(brt.fit = future_map2(.x = model.data,
                               .y = lrbf,
                               .f = gbmstep_lrbf)) %>%
  mutate(auc = future_map(.x = brt.fit,
                          .f = brt_auc)) %>%
  unnest(auc) %>%
  mutate(trees = map(.x = brt.fit, .f = ~ .$gbm.call$best.trees)) %>%
  unnest(trees) %>%
  mutate(vs = map(.x = varset,
                  .f = length)) %>%
  unnest(vs) %>%
  mutate(varsetn = sprintf("%s%s_%s_%s%s",
                           year.from,
                           ifelse(bg, "b", "x"),
                           ifelse(vs == 16, 1, 2),
                           lr,
                           bf))


ip_gg <- future_mapply(
  FUN = brtpredict,
  model = brt_gg_bb$brt.fit,
  varset = brt_gg_bb$varsetn,
  MoreArgs = list(variables = vgg1_1_01,
                  scn_id = "s1_01",
                  species = "gg"),
  future.packages = c("gbm", "raster", "dismo"))



brt_gg_bb <- brt_gg_bb %>%
  mutate(init.pred = ip_gg)

  
# -----------

save(
  
  brt_gg_bb,
  file = "output/RData/09_fit_distribution_models_gg.RData"
)