# 11 run PVAs


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
load(file = "output/RData/04_disturbance_variables.RData")
load(file = "output/RData/09_fit_distribution_models.RData")
load(file = "output/RData/10_predict_SDMs.RData")

source.functions("R/functions")


set_lb <- preds_lb[1:3,] %>%
  mutate(
    dv_id = sprintf("dv_%s_%s", scenario, rep)
  ) %>%
  rowwise %>%
  mutate(
    dvs = map(
      .x = dv_id,
      .f = get
    )
  ) %>%
  ungroup %>%
  mutate(
    mort = map(
      .x = dvs,
      .f = function(y){lapply(y, FUN = function(x){x[["mort"]]})}
    ),
    hs = map(
      .x = predmaps,
      .f =  function(x){x[["sdm_0"]]}
    )
  ) %>%
  mutate(
    mort = map(
      .x = mort,
      .f = stack
    )
  ) %>%
  dplyr::select(-v_id, -variables, -dv_id, -dvs)%>%
  mutate(
    tm = tml,
    popsize = 5000,
    varset = "",
    species = "lb"
  ) %>%
  mply.initpop(
    cc = 6,
    proj_mask = ch_mask,
    out_path = "output/pva_vars"
  ) %>%
  mply.landscape %>%
  mply.population_dynamics(stoch = 0.1) %>%
  mply.simulation(
    ntimesteps = 5,
    nreplicates = 10,
    ncores = 10,
    proj_mask = ch_mask,
    out_path = "output/pva_pops/"
  )