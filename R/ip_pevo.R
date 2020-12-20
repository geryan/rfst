## East Gippsland initial populations


source("R/spartan/spartan_settings.R")


library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(raster)
library(sp)
library(magrittr)
library(steps)

load(file = "output/RData/00_controls_eg.RData")
load(file = "output/RData/01_landscape_variables_eg.RData")
load(file = "output/RData/04.1_mortality_aggregated4_eg.RData")
load(file = "output/RData/04.3_tsl_aggregated_eg.RData")
load(file = "output/RData/10_predict_SDMs_agg_eg.RData")

source.functions("R/functions")



species_dat_pva <- tribble(
  ~sp,    ~popsize, ~cc, ~ccfun, ~stoch,  ~max_disp, ~habfun,
  "gyle", 10000,     60,  cc_60,  0.05,    2000,      habitat.downupfun,
  "pevo", 5000,     15,  cc_15,  0.05,    4000,      habitat.upfun,
  "peau", 2000,     3,    cc_3,  0.05,    20000,     habitat.upfun,
  "smle", 1000,     75,  cc_75,  0.10,    1000,      NA,
  "tyte", 1000,     2,    cc_2,  0.08,    10000,     habitat.downupfun,
  "vava", 5000,     25,  cc_25,  0.05,    5000,      NA
)


tm_pevo <- matrix(
  data = c(
    0.00, 0.00, 0.50,
    0.50, 0.00, 0.00,
    0.00, 0.8, 0.84
  ),
  nrow = 3,
  ncol = 3,
  byrow = TRUE,
  dimnames = list(
    c('Newborn','Juvenile','Adult'),
    c('Newborn','Juvenile','Adult')
  )
)


ss_pevo <- get.stable.states(tm_pevo)


i <- 1

j <- which(species_dat_pva$sp == agg_set_eg$sp[i])

k <- which(mort_agg4_eg$yscn_id == agg_set_eg$yscn_id[[i]])

l <- which(tsl_agg_eg$yscn_id == agg_set_eg$yscn_id[[i]])


#

habitat_map <- agg_set_eg$aggmaps[[i]]

mortality_map <- mort_agg4_eg$mort_agg[[k]]

disturbance_map <- tsl_agg_eg$tsl_agg[[l]]


#
habitat_map2 <- habitat_map

hab_vals <- getValues(habitat_map2)

dist_vals <- getValues(disturbance_map)

mod_vals <- habitat.upfun(dist_vals)

mod_hab_vals <- mod_vals * hab_vals

habitat_map2[] <- mod_hab_vals

#

habitat_map3 <- habitat_map

hab_vals <- getValues(habitat_map[[1]])

dist_vals <- getValues(disturbance_map)

mod_vals <- habitat.upfun(dist_vals)

mod_hab_vals <- mod_vals * hab_vals

habitat_map3[] <- mod_hab_vals

#


egf20vals <- which(getValues(eg_fire_history_20) == 2020)

eg_fire_20 <- eg_mask

eg_fire_20[egf20vals] <- 0

egf20 <- aggregate(eg_fire_20, fact = 2, fun = min)



initial_population_19_a <- initpop2(
  hs = habitat_map2[[1]],
  popsize = species_dat_pva$popsize[j],
  cc = species_dat_pva$cc[j],
  ss = ss_pevo,
  pp = 0.90
) %>%
  brick

initial_population_20_a <- initial_population_19_a * egf20 %>%
  brick


initial_population_19_b <- initpop2(
  hs = habitat_map[[1]],
  popsize = species_dat_pva$popsize[j],
  cc = species_dat_pva$cc[j],
  ss = ss_pevo,
  pp = 0.90
) %>%
  brick

initial_population_20_b <- initial_population_19_b * egf20 %>%
  brick


writeRaster(
  x = initial_population_19_a,
  filename = "/data/gpfs/projects/punim1340/rfst_eg/output/ip/ip_eg_19_a.grd",
  overwrite = TRUE
)

writeRaster(
  x = initial_population_19_b,
  filename = "/data/gpfs/projects/punim1340/rfst_eg/output/ip/ip_eg_19_b.grd",
  overwrite = TRUE
)
writeRaster(
  x = initial_population_20_a,
  filename = "/data/gpfs/projects/punim1340/rfst_eg/output/ip/ip_eg_20_a.grd",
  overwrite = TRUE
)

writeRaster(
  x = initial_population_20_b,
  filename = "/data/gpfs/projects/punim1340/rfst_eg/output/ip/ip_eg_20_b.grd",
  overwrite = TRUE
)

