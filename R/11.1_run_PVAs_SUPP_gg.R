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


load(file = "output/RData/comp_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")
load(file = "output/RData/04_disturbance_variables.RData")
load(file = "output/RData/10.1_preds_agg.RData")

source.functions("R/functions")

plan(strategy = multisession, workers = ncores)

# GG -----------------------------------------------------------------------------------

tm_gg_1 <- matrix(c(0.00, 0.5 * 0.50, 0.85 *0.50,
                  0.50,        0.00,       0.00,
                  0.00,        0.50,       0.85),
                nrow = 3,
                ncol = 3,
                byrow = TRUE,
                dimnames = list(c('Newborn','Juvenile','Adult'),
                                c('Newborn','Juvenile','Adult')))



tm_gg_2 <- matrix(c(0.00, 0.85 * 0.50, 0.85 *0.50,
                    0.50,        0.00,       0.00,
                    0.00,        0.85,       0.85),
                  nrow = 3,
                  ncol = 3,
                  byrow = TRUE,
                  dimnames = list(c('Newborn','Juvenile','Adult'),
                                  c('Newborn','Juvenile','Adult')))





nreplicates <- 50


tml <- vector("list", 18)

for (i in 1:9){
  tml[[i]] <- tm_gg_1
}

for (i in 10:18){
  tml[[i]] <- tm_gg_2
}
myenv <- environment()

# set ---------------------

set_gg <- preds_gg_agg[rep(1:3,6),] %>%
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
    tmn = c(rep(1, 9),
            rep(2, 9)),
    popsize = rep(
      x = c(
        rep(2500, times = 3),
        rep(5000, times = 3),
        rep(10000, times = 3)
      ),
      times = 2
    ),
    varset = paste0("s_",tmn, "_", popsize),
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

simset_gg_supp <- set_gg %>%
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
  simset_gg_supp,
  file = "output/RData/11.1_pvas_gg.RData"
)

