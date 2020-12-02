## Run PVAs

source("R/spartan/spartan_settings.R")


library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(raster)
library(sp)
library(magrittr)
library(steps)

load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")
load(file = "output/RData/04.1_mortality_aggregated5.RData")
load(file = "output/RData/04.3_tsl_aggregated.RData")
load(file = "output/RData/10.1_aggregate_sdm.RData")

source.functions("R/functions")

#####

species_dat_pva <- tribble(
  ~sp,    ~popsize, ~cc, ~ccfun, ~stoch,  ~max_disp, ~habfun,
  "gyle", 10000,     60,  cc_60,  0.05,    2000,      habitat.downupfun,
  "pevo", 5000,     15,  cc_15,  0.05,    4000,      habitat.upfun,
  "peau", 2000,     3,    cc_3,  0.05,    20000,     habitat.upfun,
  "smle", 1000,     75,  cc_75,  0.10,    1000,      NA,
  "tyte", 1000,     2,    cc_2,  0.08,    10000,     habitat.downupfun,
  "vava", 5000,     25,  cc_25,  0.05,    5000,      NA
)

##### gyle

tm_gyle <- matrix(
  data = c(
    0.00, 0.00, 0.80,
    0.50, 0.00, 0.00,
    0.00, 0.60, 0.76
  ),
  nrow = 3,
  ncol = 3,
  byrow = TRUE,
  dimnames = list(
    c('Newborn','Juvenile','Adult'),
    c('Newborn','Juvenile','Adult')
  )
)

ss_gyle <- get.stable.states(tm_gyle)

rmax(tm_gyle)

rmax(altm(tm_gyle))

hm_gyle <- agg5_ch$aggmap5[[
  which(
    agg5_ch$sp == "gyle" &
      agg5_ch$cscnid == "TH00_rcp45_PB_01_ACCESS1-0"
  )
]][[1]]


sf_gyle <- logistic_sf(hm_gyle)

j_gyle <- which(species_dat_pva$sp == "gyle")

ip_gyle <- initpop2(
  hs = hm_gyle,
  popsize = species_dat_pva$popsize[j_gyle],
  cc = species_dat_pva$cc[j_gyle],
  ss = ss_gyle
)

ip_gyle <- ipredo

lsc_gyle <- landscape(
  population = ip_gyle,
  suitability = hm_gyle,
  "sf_layer" = sf_gyle,
  carrying_capacity = species_dat_pva$ccfun[[j_gyle]]
)


prop_disp_gyle <- c(
  1,
  rep(
    x = 0,
    times = length(ss_gyle) - 1
  )
)

disp_gyle <- kernel_dispersal(
  dispersal_kernel = exponential_dispersal_kernel(
    distance_decay = species_dat_pva$max_disp[j_gyle]/2
  ),
  max_distance = species_dat_pva$max_disp[j_gyle],
  arrival_probability = "both",
  dispersal_proportion = set_proportion_dispersing(
    proportions = prop_disp_gyle
  )
)


grow_gyle <- growth(
  transition_matrix = tm_gyle,
  global_stochasticity = species_dat_pva$stoch[j_gyle],
  transition_function = modified_transition(
    survival_layer = "sf_layer",
    fecundity_layer = "sf_layer"
  )
)

pop_dyn_gyle <- population_dynamics(
  change = grow_gyle,
  dispersal = disp_gyle
)

simres <- simulation(
  landscape = lsc_gyle,
  population_dynamics = pop_dyn_gyle,
  demo_stochasticity = "full",
  timesteps = 50,
  replicates = 5,
  verbose = TRUE
)

plot(simres, stages = 0)

plot(simres)

ipredo <- simres[[1]][[50]]$population


plot(simres[1], type = "raster", stage = 3, timesteps = c(1, 25, 50), panels = c(3,1))
plot(simres[1], type = "raster", stage = 3, timesteps = c(50), panels = c(1,1))


writeRaster(
  ipredo,
  filename = "output/initpop/ip_gyle_ch.grd"
)

ip_gyle <- brick("output/initpop/ip_gyle_ch.grd")

#####

tm_pevo <- matrix(
  data = c(
    0.00, 0.00, 0.50,
    0.50, 0.00, 0.00,
    0.00, 0.85, 0.90
  ),
  nrow = 3,
  ncol = 3,
  byrow = TRUE,
  dimnames = list(
    c('Newborn','Juvenile','Adult'),
    c('Newborn','Juvenile','Adult')
  )
)

#####

tm_peau <- matrix(
  data = c(
    0.0, 0.0, 0.5,
    0.7, 0.0, 0.0,
    0.0, 0.6, 0.8
  ),
  nrow = 3,
  ncol = 3,
  byrow = TRUE,
  dimnames = list(
    c('Newborn','Juvenile','Adult'),
    c('Newborn','Juvenile','Adult')
  )
)

#####

tm_smle <- matrix(
  data = c(
    0.00, 4.20, 4.20,
    0.29, 0.0,  0.00,
    0.00,  0.05, 0.05
  ),
  nrow = 3,
  ncol = 3,
  byrow = TRUE,
  dimnames = list(
    c('Newborn', 'Adult', "Dummy"),
    c('Newborn', 'Adult', "Dummy")
  )
)

#####

tm_tyte <- matrix(
  data = c(
    0.00, 0.00, 1.23,
    0.75, 0.00, 0.00,
    0.00, 0.90, 0.90 
  ),
  nrow = 3,
  ncol = 3,
  byrow = TRUE,
  dimnames = list(
    c('Newborn','Sub-adult','Adult'),
    c('Newborn','Sub-adult','Adult')
  )
)

ss_tyte <- get.stable.states(tm_tyte)

rmax(tm_tyte)

rmax(altm(tm_tyte))

hm_tyte <- agg5_ch$aggmap5[[
  which(
    agg5_ch$sp == "tyte" &
      agg5_ch$cscnid == "TH00_rcp45_PB_01_ACCESS1-0"
  )
  ]][[1]]


sf_tyte <- logistic_sf(hm_tyte, z = 0.2)

j_tyte <- which(species_dat_pva$sp == "tyte")

ip_tyte <- initpop2(
  hs = hm_tyte,
  popsize = species_dat_pva$popsize[j_tyte],
  cc = species_dat_pva$cc[j_tyte],
  ss = ss_tyte
)


lsc_tyte <- landscape(
  #population = ip_tyte,
  population = ipredo,
  suitability = hm_tyte,
  "sf_layer" = sf_tyte,
  carrying_capacity = species_dat_pva$ccfun[[j_tyte]]
)


prop_disp_tyte <- c(
  1,
  rep(
    x = 0,
    times = length(ss_tyte) - 1
  )
)

# disp_tyte <- kernel_dispersal(
#   dispersal_kernel = exponential_dispersal_kernel(
#     distance_decay = species_dat_pva$max_disp[j_tyte]/2
#   ),
#   max_distance = species_dat_pva$max_disp[j_tyte],
#   arrival_probability = "suitability",
#   dispersal_proportion = set_proportion_dispersing(
#     proportions = prop_disp_tyte
#   )
# )

disp_tyte <- cellular_automata_dispersal(
  max_cells = 40,
  dispersal_proportion = set_proportion_dispersing(
    proportions = c(1,0,0)
  )
)

grow_tyte <- growth(
  transition_matrix = tm_tyte,
  global_stochasticity = species_dat_pva$stoch[j_tyte],
  transition_function = modified_transition(
    survival_layer = "sf_layer",
    fecundity_layer = "sf_layer"
  )
)

pop_dyn_tyte <- population_dynamics(
  change = grow_tyte,
  dispersal = disp_tyte,
  density_dependence = ceiling_density(stages = c(2,3))
)

#ipredo_tyte <- simres[[1]][[50]]$population

simres <- simulation(
  landscape = lsc_tyte,
  population_dynamics = pop_dyn_tyte,
  demo_stochasticity = "full",
  timesteps = 50,
  replicates = 5,
  verbose = TRUE
)

dev.off()

plot(simres, stages = 0)

plot(simres)

ipredo <- simres[[1]][[50]]$population


plot(simres[1], type = "raster", stage = 3, timesteps = c(1, 25, 50), panels = c(3,1))
plot(simres[1], type = "raster", stage = 3, timesteps = c(50), panels = c(1,1))


writeRaster(
  ipredo,
  filename = "output/initpop/ip_tyte_ch.grd"
)

ip_tyte <- brick("output/initpop/ip_tyte_ch.grd")

#####

tm_vava <- matrix(
  data = c(
    0.00, 0.00, 0.00, 2.69,
    0.37, 0.00, 0.00, 0.00, 
    0.00, 0.37, 0.00, 0.00,
    0.00, 0.00, 0.37, 0.93
  ),
  nrow = 4,
  ncol = 4,
  byrow = TRUE,
  dimnames = list(
    c('Hatchling','Juvenile', "Sub-adult",'Adult'),
    c('Hatchling','Juvenile', "Sub-adult",'Adult')
  )
)







