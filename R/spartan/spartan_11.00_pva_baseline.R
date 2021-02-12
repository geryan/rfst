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
  "gyle", 5000,     60,  cc_60,  0.05,    2000,      habitat.downupfun,
  "pevo", 5000,     15,  cc_15,  0.05,    4000,      habitat.upfun,
  "peau", 2000,     3,    cc_3,  0.05,    20000,     habitat.upfun,
  "smle", 1000,     75,  cc_75,  0.07,    1000,      NA,
  "tyte", 1000,     2,    cc_2,  0.07,    100000,     habitat.downupfun,
  "vava", 5000,     25,  cc_25,  0.05,    5000,      NA
)

##### 

tm_gyle <- matrix(
  data = c(
    0.00, 0.00, 0.80,
    0.52, 0.00, 0.00,
    0.00, 0.65, 0.76
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
  ss = ss_gyle,
  pp = 0.98
)

#ip_gyle <- ipredo

lsc_gyle <- landscape(
  population = ip_gyle,
  suitability = hm_gyle,
  "sf_layer" = sf_gyle,
  carrying_capacity = species_dat_pva$ccfun[[j_gyle]]
)


# prop_disp_gyle <- c(
#   1,
#   rep(
#     x = 0,
#     times = length(ss_gyle) - 1
#   )
# )
# 
# disp_gyle <- kernel_dispersal(
#   dispersal_kernel = exponential_dispersal_kernel(
#     distance_decay = species_dat_pva$max_disp[j_gyle]/2
#   ),
#   max_distance = species_dat_pva$max_disp[j_gyle],
#   arrival_probability = "both",
#   dispersal_proportion = set_proportion_dispersing(
#     proportions = prop_disp_gyle
#   )
# )

disp_gyle <- cellular_automata_dispersal(
  max_cells = 40,
  dispersal_proportion = set_proportion_dispersing(
    proportions = c(1,0.2,0.2)
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
  replicates = 10,
  verbose = TRUE
)

plot(simres, stages = 0)

plot(simres)

simpop <- get_pop_simulation(simres)

psr(
  simpop,
  #ylim = c(0, 2000),
  stages = 0
)


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

rmax(tm_pevo)

rmax(altm(tm_pevo))

# hm_pevo <- agg5_ch$aggmap5[[
#   which(
#     agg5_ch$sp == "pevo" &
#       agg5_ch$cscnid == "TH00_rcp45_PB_01_ACCESS1-0"
#   )
# ]][[1]]

hm_pevo <- raster("/data/gpfs/projects/punim0995/rfst/output/habitat_pred/brt_initial_pred_TH19_rcp45_PB_01_ACCESS1-0_init__pevo.grd") %>%
  raster::aggregate(fact = 5)

hm_long <- brick("output/brtpred_TH19_rcp45_PB_01_ACCESS1-0__pevo.grd") %>%
  raster::aggregate(fact = 5)

plot(hm_pevo)
plot(pa_data$pa_dat[[2]]["PA"], add = TRUE)

plot(pa_data$pa_dat[[2]])

sf_pevo <- logistic_sf(hm_pevo)

j_pevo <- which(species_dat_pva$sp == "pevo")

ip_pevo <- initpop2(
  hs = hm_pevo,
  popsize = species_dat_pva$popsize[j_pevo],
  cc = species_dat_pva$cc[j_pevo],
  ss = ss_pevo,
  pp = 0.95
)


# ip_pevo2 <- initpop3(
#   hs = hm_pevo,
#   popsize = species_dat_pva$popsize[j_pevo],
#   cc = species_dat_pva$cc[j_pevo],
#   ss = ss_pevo
# )

#ip_pevo <- ipredo

lsc_pevo <- landscape(
  population = ip_pevo,
  suitability = hm_pevo,
  "sf_layer" = sf_pevo,
  carrying_capacity = species_dat_pva$ccfun[[j_pevo]]
)


prop_disp_pevo <- c(
  1,
  rep(
    x = 0,
    times = length(ss_pevo) - 1
  )
)

# disp_pevo <- kernel_dispersal(
#   dispersal_kernel = exponential_dispersal_kernel(
#     distance_decay = species_dat_pva$max_disp[j_pevo]/2
#   ),
#   max_distance = species_dat_pva$max_disp[j_pevo],
#   arrival_probability = "both",
#   dispersal_proportion = set_proportion_dispersing(
#     proportions = prop_disp_pevo
#   )
# )

disp_pevo <- cellular_automata_dispersal(
  max_cells = 80,
  dispersal_proportion = set_proportion_dispersing(
    proportions = prop_disp_pevo
  ),
  barriers = NULL,
  use_suitability = TRUE,
  carrying_capacity = "carrying_capacity"
)


grow_pevo <- growth(
  transition_matrix = tm_pevo,
  global_stochasticity = species_dat_pva$stoch[j_pevo],
  transition_function = modified_transition(
    survival_layer = "sf_layer",
    fecundity_layer = "sf_layer"
  )
)

pop_dyn_pevo <- population_dynamics(
  change = grow_pevo,
  dispersal = disp_pevo
)

simres <- simulation(
  landscape = lsc_pevo,
  population_dynamics = pop_dyn_pevo,
  demo_stochasticity = "full",
  timesteps = 50,
  replicates = 10,
  verbose = TRUE
)

plot(simres, stages = 0)

plot(simres)

#ipredo <- simres[[1]][[50]]$population


plot(simres[1], type = "raster", stage = 3, timesteps = c(1, 25, 50), panels = c(3,1))
plot(simres[1], type = "raster", stage = 3, timesteps = c(50), panels = c(1,1))





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


ss_smle <- get.stable.states(tm_smle)

rmax(tm_smle)

rmax(altm(tm_smle))

hm_smle <- agg5_ch$aggmap5[[
  which(
    agg5_ch$sp == "smle" &
      agg5_ch$cscnid == "TH00_rcp45_PB_01_ACCESS1-0"
  )
]][[1]]



sf_smle <- logistic_sf(hm_smle, z = 0.2)

j_smle <- which(species_dat_pva$sp == "smle")

ip_smle <- initpop2(
  hs = hm_smle,
  popsize = species_dat_pva$popsize[j_smle],
  cc = species_dat_pva$cc[j_smle],
  ss = ss_smle,
  pp = 0.90
)



lsc_smle <- landscape(
  #population = ip_smle,
  population = ip_smle,
  suitability = hm_smle,
  "sf_layer" = sf_smle,
  carrying_capacity = species_dat_pva$ccfun[[j_smle]]
)


prop_disp_smle <- c(
  1,
  rep(
    x = 0,
    times = length(ss_smle) - 1
  )
)

# disp_smle <- kernel_dispersal(
#   dispersal_kernel = exponential_dispersal_kernel(
#     distance_decay = species_dat_pva$max_disp[j_smle]/2
#   ),
#   max_distance = species_dat_pva$max_disp[j_smle],
#   arrival_probability = "suitability",
#   dispersal_proportion = set_proportion_dispersing(
#     proportions = prop_disp_smle
#   )
# )

disp_smle <- cellular_automata_dispersal(
  max_cells = 10,
  dispersal_proportion = set_proportion_dispersing(
    proportions = c(1,0.7,0.6)
  )
)

grow_smle <- growth(
  transition_matrix = tm_smle,
  global_stochasticity = species_dat_pva$stoch[j_smle],
  transition_function = modified_transition(
    survival_layer = "sf_layer",
    fecundity_layer = "sf_layer"
  )
)


pop_dyn_smle <- population_dynamics(
  change = grow_smle,
  dispersal = disp_smle,
  density_dependence = ceiling_density(stages = c(2,3))
)

#ipredo_smle <- simres[[1]][[50]]$population

simres <- simulation(
  landscape = lsc_smle,
  population_dynamics = pop_dyn_smle,
  demo_stochasticity = "full",
  timesteps = 50,
  replicates = 10,
  verbose = TRUE
)

#simres_smle_static <- simres

#dev.off()




plot(simres, stages = 0)

simpop <- get_pop_simulation(simres)

psr(
  simpop,
  #ylim = c(0, 2000),
  stages = 0
)

#####

######### static 

tm_tyte <- matrix(
  data = c(
    0.00, 0.00, 1.50,
    0.55, 0.00, 0.00,
    0.00, 0.70, 0.75 
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
  ss = ss_tyte,
  pp = 0.90
)



lsc_tyte <- landscape(
  #population = ip_tyte,
  population = ip_tyte,
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
  max_cells = 5000,
  dispersal_proportion = set_proportion_dispersing(
    proportions = c(1,0.6,0.6)
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

#simres_tyte_static <- simres

#dev.off()

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







