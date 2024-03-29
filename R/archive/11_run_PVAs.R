# 11 run PVAs


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


load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")
load(file = "output/RData/04_disturbance_variables.RData")
load(file = "output/RData/10.1_preds_agg.RData")

source.functions("R/functions")

plan(strategy = multisession, workers = ncores)

# -----------------

tm_lb <- matrix(c(0.00, 0.50 * 0.75, 0.80 *0.75,
                  0.50,        0.00,       0.00,
                  0.00,        0.50,       0.80),
                nrow = 3,
                ncol = 3,
                byrow = TRUE,
                dimnames = list(c('Newborn','Juvenile','Adult'),
                                c('Newborn','Juvenile','Adult')))


ss_lb <- get.stable.states(tm_lb)

#nreplicates <- 100

npvas <- dim(preds_lb_agg)[1]

tml <- vector("list", npvas)

for (i in 1:npvas){
  tml[[i]] <- tm_lb
}

myenv <- environment()

# set ---------------------

set_lb <- preds_lb_agg %>%
  # mutate(
  #   dv_id = sprintf("dv_%s_%s", scenario, rep)
  # ) %>%
  # rowwise %>%
  # mutate(
  #   dvs = map(
  #     .x = dv_id,
  #     .f = get,
  #     envir = myenv
  #   )
  # ) %>%
  ungroup %>%
  mutate(
  #   mort = map(
  #     .x = dvs,
  #     .f = function(y){lapply(y, FUN = function(x){x[["mort"]]})}
  #   ),
    hs = map(
      .x = predmaps,
      .f =  function(x){x[["sdm_0"]]}
    )
  ) %>%
  # mutate(
  #   mort = map(
  #     .x = mort,
  #     .f = stack
  #   )
  # ) %>%
  # dplyr::select(-v_id, -variables, -dv_id, -dvs)%>%
  mutate(
    tm = tml,
    popsize = 3000,
    varset = "",
    species = "lb"
  ) %>%
  mply.initpop(
    cc = 245,
    proj_mask = ch_mask_agg,
    out_path = "output/pva_vars"
  ) %>%
  mply.landscape(
    ccfun = cc_245
  ) %>%
  mply.population_dynamics(stoch = 0.1)

# simset ----

simset_lb <- set_lb %>%
  mply.simulation(
    ntimesteps = ntimesteps,
    nreplicates = nreplicates,
    ncores = ncores,
    proj_mask = ch_mask_agg,
    out_path = "output/pva_pops/",
    save.sims = TRUE,
    ss.path = "output/pva_objects",
    save.pops = TRUE,
    sp.path = "output/pva_pops",
    pll.level = "m"
  ) %>%
  mutate(
    pva_sims = map(
      .x = pva_res,
      .f = ~ .x$simulations
    ),
    pva_pops = map(
      .x = pva_res,
      .f = ~ .x$pops
    )
  ) %>%
  dplyr::select(-pva_res)

# GG -----------------------------------------------------------------------------------

tm_gg <- matrix(c(0.00, 0.6 * 0.50, 0.85 *0.50,
                  0.50,        0.00,       0.00,
                  0.00,        0.6,       0.85),
                nrow = 3,
                ncol = 3,
                byrow = TRUE,
                dimnames = list(c('Newborn','Juvenile','Adult'),
                                c('Newborn','Juvenile','Adult')))

rmax(tm_gg)

ss_gg <- get.stable.states(tm_gg)


npvas <- dim(preds_gg_agg)[1]

tml <- vector("list", npvas)

for (i in 1:npvas){
  tml[[i]] <- tm_gg
}

myenv <- environment()

# set ---------------------

set_gg <- preds_gg_agg %>%
  # mutate(
  #   dv_id = sprintf("dv_%s_%s", scenario, rep)
  # ) %>%
  # rowwise %>%
  # mutate(
  #   dvs = map(
  #     .x = dv_id,
  #     .f = get,
  #     envir = myenv
  #   )
  # ) %>%
ungroup %>%
  mutate(
    #   mort = map(
    #     .x = dvs,
    #     .f = function(y){lapply(y, FUN = function(x){x[["mort"]]})}
    #   ),
    hs = map(
      .x = predmaps,
      .f =  function(x){x[["sdm_0"]]}
    )
  ) %>%
  # mutate(
  #   mort = map(
  #     .x = mort,
  #     .f = stack
  #   )
  # ) %>%
  # dplyr::select(-v_id, -variables, -dv_id, -dvs)%>%
  mutate(
    tm = tml,
    popsize = 5000,
    varset = "",
    species = "gg"
  ) %>%
  mply.initpop(
    cc = 60,
    proj_mask = ch_mask_agg,
    out_path = "output/pva_vars"
  ) %>%
  mply.landscape(
    ccfun = cc_60
  ) %>%
  mply.population_dynamics(stoch = 0.1)

# simset ----

simset_gg <- set_gg %>%
  mply.simulation(
    ntimesteps = ntimesteps,
    nreplicates = nreplicates,
    ncores = ncores,
    proj_mask = ch_mask_agg,
    out_path = "output/pva_pops/",
    save.sims = TRUE,
    ss.path = "output/pva_objects",
    save.pops = TRUE,
    sp.path = "output/pva_pops",
    pll.level = "m"
  ) %>%
  mutate(
    pva_sims = map(
      .x = pva_res,
      .f = ~ .x$simulations
    ),
    pva_pops = map(
      .x = pva_res,
      .f = ~ .x$pops
    )
  ) %>%
  dplyr::select(-pva_res)


save(
  tm_lb,
  simset_lb,
  tm_gg,
  simset_gg,
  file = "output/RData/11_pvas.RData"
)

