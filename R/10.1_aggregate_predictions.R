# 10.1 aggregate preds

library(dplyr)
library(tibble)
library(magrittr)
library(purrr)
library(future)
library(future.apply)
library(raster)
library(sp)

load(file = "output/RData/00_comp_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")
load(file = "output/RData/04_disturbance_variables.RData")
load(file = "output/RData/09_fit_distribution_models.RData")
load(file = "output/RData/10_predict_SDMs.RData")

source.functions("R/functions")

plan(multisession, workers = ncores)

myenv <- environment()

# ------------------------

plba <- future_mapply(
  FUN = maggregate,
  x = preds_lb$predmaps,
  scn_id = preds_lb$scn_id,
  MoreArgs = list(
    fact = 10,
    out_path = "output/habitat_pred_aggregated",
    aggname = "aggpreds",
    varset = "",
    species = "lb"
  )
)

plba <- tibble(predmaps_agg = plba)

# ----------------------

mpred <- preds_lb %>%
  mutate(
    dv_id = sprintf("dv_%s_%s", scenario, rep)
  ) %>%
  rowwise %>%
  mutate(
    dvs = map(
      .x = dv_id,
      .f = get,
      envir = myenv
    )
  ) %>%
  ungroup %>%
  mutate(
    mort = map(
      .x = dvs,
      .f = function(y){lapply(y, FUN = function(x){x[["mort"]]})}
    )
  ) %>%
  mutate(
    mort = map(
      .x = mort,
      .f = stack
    )
  ) %>%
  dplyr::select(-v_id, -variables, -dv_id, -dvs)

mlba <- future_mapply(
  FUN = maggregate,
  x = mpred$mort,
  scn_id = mpred$scn_id,
  MoreArgs = list(
    fact = 10,
    out_path = "output/mort_aggregated",
    aggname = "mort",
    varset = "",
    species = "lb"
  )
)

mlba <- tibble(mort = mlba)

# ------------------

preds_lb_agg <- bind_cols(preds_lb, plba, mlba) %>%
  mutate(predmaps = predmaps_agg) %>%
  dplyr::select(-predmaps_agg)


# ---------------------
# ---------------------

# ------------------------

pgga <- future_mapply(
  FUN = maggregate,
  x = preds_gg$predmaps,
  scn_id = preds_gg$scn_id,
  MoreArgs = list(
    fact = 10,
    out_path = "output/habitat_pred_aggregated",
    aggname = "aggpreds",
    varset = "",
    species = "gg"
  )
)

pgga <- tibble(predmaps_agg = pgga)

# ----------------------

mpred <- preds_gg %>%
  mutate(
    dv_id = sprintf("dv_%s_%s", scenario, rep)
  ) %>%
  rowwise %>%
  mutate(
    dvs = map(
      .x = dv_id,
      .f = get,
      envir = myenv
    )
  ) %>%
  ungroup %>%
  mutate(
    mort = map(
      .x = dvs,
      .f = function(y){lapply(y, FUN = function(x){x[["mort"]]})}
    )
  ) %>%
  mutate(
    mort = map(
      .x = mort,
      .f = stack
    )
  ) %>%
  dplyr::select(-v_id, -variables, -dv_id, -dvs)

mgga <- future_mapply(
  FUN = maggregate,
  x = mpred$mort,
  scn_id = mpred$scn_id,
  MoreArgs = list(
    fact = 10,
    out_path = "output/mort_aggregated",
    aggname = "mort",
    varset = "",
    species = "gg"
  )
)

mgga <- tibble(mort = mgga)

# ------------------

preds_gg_agg <- bind_cols(preds_gg, pgga, mgga) %>%
  mutate(predmaps = predmaps_agg) %>%
  dplyr::select(-predmaps_agg)

# ---------------------
save(
  preds_lb_agg,
  preds_gg_agg,
  file = "output/RData/10.1_preds_agg.RData"
)

