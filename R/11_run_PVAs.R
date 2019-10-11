# 11 run PVAs


library(steps)
library(foreach)
library(doMC)
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

tm_lb <- matrix(c(0.00, 0.59 * 0.75, 0.79 *0.75,
                  0.59,        0.00,       0.00,
                  0.00,        0.59,       0.79),
                nrow = 3,
                ncol = 3,
                byrow = TRUE,
                dimnames = list(c('Newborn','Juvenile','Adult'),
                                c('Newborn','Juvenile','Adult')))


ss_lb <- get.stable.states(tm_lb)

# ss_lb
# 
# rmax(tm = tm_lb)

npvas <- dim(preds_lb)[1]

tml <- vector("list", npvas) # 3 because tibble is 3 rows long. change for longer one

for (i in 1:npvas){
  tml[[i]] <- tm_lb
}

tml


#ntimesteps <- 4
nreplicates <- 20
#ncores <- 20

simset_lb <- preds_lb %>%
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
    out_path = "output/pva_vars",
    ncores = ncores
  ) %>%
  mply.landscape(
    ccfun = cc_6
  ) %>%
  mply.population_dynamics(stoch = 0.1) %>%
  mply.simulation(
    ntimesteps = ntimesteps,
    nreplicates = nreplicates,
    ncores = ncores,
    proj_mask = ch_mask,
    out_path = "output/pva_pops/"
  ) %>%
  mutate(
    simpop = map(
      .x = pva,
      .f = gps,
      workers = ncores
    )
  )

save(
  simset_lb,
  file = "output/RData/11_pvas.RData"
)

