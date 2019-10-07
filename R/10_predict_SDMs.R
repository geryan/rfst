## 10 predict SDMs



library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(future)
library(future.apply)
library(raster)
library(sp)


load(file = "output/RData/00_comp_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")
load(file = "output/RData/07_combined_variables.RData")
load(file = "output/RData/09_fit_distribution_models.RData")

source.functions("R/functions")


# Predict LBP ----------

preds_lb <- expand.grid(
  scenario = scn_list,
  rep = rep_list
) %>%
  as_tibble %>%
  mutate(
    scn_id = sprintf("s%s_%s", scenario, rep),
    v_id = sprintf("vlb1_%s_%s", scenario, rep)
  ) %>%
  rowwise %>%
  mutate(
    variables = map(
      .x = v_id,
      .f = get,
      envir = .GlobalEnv
    )
  )

plan(multisession, workers = ncores)

predmaps_lb <- future_mapply(
  FUN = brtpredict,
  variables = preds_lb$variables,
  scn_id = preds_lb$scn_id,
  MoreArgs = list(
    model = brt_lb$brt.fit[[11]],
    species = "lb",
    varset = "09b_1",
    ncores = ncores,
    initial = FALSE
  )
)


pms_lb <- tibble(predmaps = predmaps_lb)

preds_lb <- bind_cols(preds_lb, pms_lb)


# Predict GG -------------------

# preds_gg <- expand.grid(
#   scenario = scn_list,
#   rep = rep_list
# ) %>%
#   as_tibble %>%
#   mutate(
#     scn_id = sprintf("s%s_%s", scenario, rep),
#     v_id = sprintf("vgg1_%s_%s", scenario, rep)
#   ) %>%
#   rowwise %>%
#   mutate(
#     variables = map(
#       .x = v_id,
#       .f = get
#     )
#   )
# 
# predmaps_gg <- mapply(
#   FUN = brtpredict,
#   variables = preds_gg$variables,
#   scn_id = preds_gg$scn_id,
#   MoreArgs = list(
#     model = brt_gg$brt.fit[[11]],
#     species = "gg",
#     varset = "09b_1",
#     ncores = ncores,
#     initial = FALSE
#   )
# )
# 
# 
# preds_gg <- preds_gg %>%
#   mutate(predmaps = predmaps_gg)


save(
  preds_lb,
  #preds_gg,
  file = "output/RData/10_predict_SDMs.RData"
)
