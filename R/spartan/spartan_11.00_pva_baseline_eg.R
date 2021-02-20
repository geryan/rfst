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

load(file = "output/RData/00_controls_eg.RData")
load(file = "output/RData/01_landscape_variables_eg.RData")
load(file = "output/RData/04.1_mortality_aggregated4_eg.RData")
load(file = "output/RData/04.3_tsl_aggregated_eg.RData")
load(file = "output/RData/10_predict_SDMs_agg_eg.RData")
load(file = "output/RData/11.0_pva_species_dat_eg.RData")

source.functions("R/functions")

#####


##### 

tm_polo <- matrix(
  data = c(
    0.00, 1.00,
    0.37, 0.64
  ),
  nrow = 2,
  ncol = 2,
  byrow = TRUE,
  dimnames = list(
    c('Newborn','Adult'),
    c('Newborn','Adult')
  )
)

ss_polo <- get.stable.states(tm_polo)

rmax(tm_polo)

rmax(altm(tm_polo))

hm_polo <- agg_set_eg$aggmaps[[
  which(
    agg_set_eg$sp == "polo" &
      agg_set_eg$ycscnid == "EG19_TH00_rcp45_PB_01_ACCESS1-0"
  )
]][[1]]


sf_polo <- logistic_sf(hm_polo)

j_polo <- which(species_dat_pva_eg$sp == "polo")

ip_polo <- initpop2(
  hs = hm_polo,
  popsize = species_dat_pva_eg$popsize[j_polo],
  cc = species_dat_pva_eg$cc[j_polo],
  ss = ss_polo,
  pp = 0.99
)

#ip_polo <- ipredo

lsc_polo <- landscape(
  population = ip_polo,
  suitability = hm_polo,
  "sf_layer" = sf_polo,
  carrying_capacity = species_dat_pva_eg$ccfun[[j_polo]]
)



disp_polo <- cellular_automata_dispersal(
  max_cells = 20,
  dispersal_proportion = set_proportion_dispersing(
    proportions = c(1,0.2)
  )
)


grow_polo <- growth(
  transition_matrix = tm_polo,
  global_stochasticity = species_dat_pva_eg$stoch[j_polo],
  transition_function = modified_transition(
    survival_layer = "sf_layer",
    fecundity_layer = "sf_layer"
  )
)

pop_dyn_polo <- population_dynamics(
  change = grow_polo,
  dispersal = disp_polo
)

simres <- simulation(
  landscape = lsc_polo,
  population_dynamics = pop_dyn_polo,
  demo_stochasticity = "full",
  timesteps = 50,
  replicates = 8,
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




plot(simres[1], type = "raster", stage = 3, timesteps = c(1, 25, 50), panels = c(3,1))
plot(simres[1], type = "raster", stage = 3, timesteps = c(50), panels = c(1,1))


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

hm_pevo <- agg_set_eg$aggmaps[[
  which(
    agg_set_eg$sp == "pevo" &
      agg_set_eg$ycscnid == "EG19_TH00_rcp45_PB_01_ACCESS1-0"
  )
]][[1]]


plot(hm_pevo)
plot(pa_data$pa_dat[[2]]["PA"], add = TRUE)

plot(pa_data$pa_dat[[2]])

sf_pevo <- logistic_sf(hm_pevo)

j_pevo <- which(species_dat_pva_eg$sp == "pevo")

ip_pevo <- initpop2(
  hs = hm_pevo,
  popsize = species_dat_pva_eg$popsize[j_pevo],
  cc = species_dat_pva_eg$cc[j_pevo],
  ss = ss_pevo,
  pp = 0.95
)


# ip_pevo2 <- initpop3(
#   hs = hm_pevo,
#   popsize = species_dat_pva_eg$popsize[j_pevo],
#   cc = species_dat_pva_eg$cc[j_pevo],
#   ss = ss_pevo
# )

#ip_pevo <- ipredo

lsc_pevo <- landscape(
  population = ip_pevo,
  suitability = hm_pevo,
  "sf_layer" = sf_pevo,
  carrying_capacity = species_dat_pva_eg$ccfun[[j_pevo]]
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
#     distance_decay = species_dat_pva_eg$max_disp[j_pevo]/2
#   ),
#   max_distance = species_dat_pva_eg$max_disp[j_pevo],
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
  global_stochasticity = species_dat_pva_eg$stoch[j_pevo],
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



ss_peau <- get.stable.states(tm_peau)

rmax(tm_peau)

rmax(altm(tm_peau))

hm_peau <- agg_set_eg$aggmaps[[
  which(
    agg_set_eg$sp == "peau" &
      agg_set_eg$ycscnid == "EG19_TH00_rcp45_PB_01_ACCESS1-0"
  )
]][[1]]


sf_peau <- logistic_sf(hm_peau)

j_peau <- which(species_dat_pva_eg$sp == "peau")

ip_peau <- initpop2(
  hs = hm_peau,
  popsize = species_dat_pva_eg$popsize[j_peau],
  cc = species_dat_pva_eg$cc[j_peau],
  ss = ss_peau,
  pp = 0.99
)

#ip_peau <- ipredo

lsc_peau <- landscape(
  population = ip_peau,
  suitability = hm_peau,
  "sf_layer" = sf_peau,
  carrying_capacity = species_dat_pva_eg$ccfun[[j_peau]]
)


# prop_disp_peau <- c(
#   1,
#   rep(
#     x = 0,
#     times = length(ss_peau) - 1
#   )
# )
# 
# disp_peau <- kernel_dispersal(
#   dispersal_kernel = exponential_dispersal_kernel(
#     distance_decay = species_dat_pva_eg$max_disp[j_peau]/2
#   ),
#   max_distance = species_dat_pva_eg$max_disp[j_peau],
#   arrival_probability = "both",
#   dispersal_proportion = set_proportion_dispersing(
#     proportions = prop_disp_peau
#   )
# )

disp_peau <- cellular_automata_dispersal(
  max_cells = 500,
  dispersal_proportion = density_dependence_dispersing(
    maximum_proportions = c(1, 1, 0.5)
  )
)


grow_peau <- growth(
  transition_matrix = tm_peau,
  global_stochasticity = species_dat_pva_eg$stoch[j_peau],
  transition_function = modified_transition(
    survival_layer = "sf_layer",
    fecundity_layer = "sf_layer"
  )
)

pop_dyn_peau <- population_dynamics(
  change = grow_peau,
  dispersal = disp_peau
)

simres <- simulation(
  landscape = lsc_peau,
  population_dynamics = pop_dyn_peau,
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

hm_smle <- agg_set_eg$aggmaps[[
  which(
    agg_set_eg$sp == "smle" &
      agg_set_eg$cscnid == "TH00_rcp45_PB_01_ACCESS1-0"
  )
]][[1]]



sf_smle <- logistic_sf(hm_smle, z = 0.2)

j_smle <- which(species_dat_pva_eg$sp == "smle")

ip_smle <- initpop2(
  hs = hm_smle,
  popsize = species_dat_pva_eg$popsize[j_smle],
  cc = species_dat_pva_eg$cc[j_smle],
  ss = ss_smle,
  pp = 0.90
)



lsc_smle <- landscape(
  #population = ip_smle,
  population = ip_smle,
  suitability = hm_smle,
  "sf_layer" = sf_smle,
  carrying_capacity = species_dat_pva_eg$ccfun[[j_smle]]
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
#     distance_decay = species_dat_pva_eg$max_disp[j_smle]/2
#   ),
#   max_distance = species_dat_pva_eg$max_disp[j_smle],
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
  global_stochasticity = species_dat_pva_eg$stoch[j_smle],
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

hm_tyte <- agg_set_eg$aggmaps[[
  which(
    agg_set_eg$sp == "tyte" &
      agg_set_eg$ycscnid == "EG19_TH00_rcp45_PB_01_ACCESS1-0"
  )
  ]][[1]]



sf_tyte <- logistic_sf(hm_tyte, z = 0.2)

j_tyte <- which(species_dat_pva_eg$sp == "tyte")

ip_tyte <- initpop2(
  hs = hm_tyte,
  popsize = species_dat_pva_eg$popsize[j_tyte],
  cc = species_dat_pva_eg$cc[j_tyte],
  ss = ss_tyte,
  pp = 0.95
)



lsc_tyte <- landscape(
  #population = ip_tyte,
  population = ip_tyte,
  suitability = hm_tyte,
  "sf_layer" = sf_tyte,
  carrying_capacity = species_dat_pva_eg$ccfun[[j_tyte]]
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
#     distance_decay = species_dat_pva_eg$max_disp[j_tyte]/2
#   ),
#   max_distance = species_dat_pva_eg$max_disp[j_tyte],
#   arrival_probability = "suitability",
#   dispersal_proportion = set_proportion_dispersing(
#     proportions = prop_disp_tyte
#   )
# )

disp_tyte <- cellular_automata_dispersal(
  max_cells = 100000,
  dispersal_proportion = set_proportion_dispersing(
    proportions = c(1,0.6,0.6)
  )
)

grow_tyte <- growth(
  transition_matrix = tm_tyte,
  global_stochasticity = species_dat_pva_eg$stoch[j_tyte],
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
  replicates = 10,
  verbose = TRUE
)

#simres_tyte_static <- simres

#dev.off()

plot(simres, stages = 0)

plot(simres)

simpop <- get_pop_simulation(simres)

psr(
  simpop,
  #ylim = c(0, 2000),
  stages = 0
)

#####

tm_vava <- matrix(
  data = c(
    0.00, 0.00, 0.00, 2.69,
    0.36, 0.00, 0.00, 0.00, 
    0.00, 0.36, 0.00, 0.00,
    0.00, 0.00, 0.36, 0.93
  ),
  nrow = 4,
  ncol = 4,
  byrow = TRUE,
  dimnames = list(
    c('Hatchling','Juvenile', "Sub-adult",'Adult'),
    c('Hatchling','Juvenile', "Sub-adult",'Adult')
  )
)




ss_vava <- get.stable.states(tm_vava)

rmax(tm_vava)

rmax(altm(tm_vava))

hm_vava <- agg_set_eg$aggmaps[[
  which(
    agg_set_eg$sp == "vava" &
      agg_set_eg$ycscnid == "EG19_TH00_rcp45_PB_01_ACCESS1-0"
  )
]][[1]]


sf_vava <- logistic_sf(hm_vava)

j_vava <- which(species_dat_pva_eg$sp == "vava")

ip_vava <- initpop2(
  hs = hm_vava,
  popsize = species_dat_pva_eg$popsize[j_vava],
  cc = species_dat_pva_eg$cc[j_vava],
  ss = ss_vava,
  pp = 0.98
)

#ip_vava <- ipredo

lsc_vava <- landscape(
  population = ip_vava,
  suitability = hm_vava,
  "sf_layer" = sf_vava,
  carrying_capacity = species_dat_pva_eg$ccfun[[j_vava]]
)


# prop_disp_vava <- c(
#   1,
#   rep(
#     x = 0,
#     times = length(ss_vava) - 1
#   )
# )
# 
# disp_vava <- kernel_dispersal(
#   dispersal_kernel = exponential_dispersal_kernel(
#     distance_decay = species_dat_pva_eg$max_disp[j_vava]/2
#   ),
#   max_distance = species_dat_pva_eg$max_disp[j_vava],
#   arrival_probability = "both",
#   dispersal_proportion = set_proportion_dispersing(
#     proportions = prop_disp_vava
#   )
# )

disp_vava <- cellular_automata_dispersal(
  max_cells = 100,
  dispersal_proportion = density_dependence_dispersing(
    maximum_proportions = c(1, 1, 1, 0.3)
  )
)


grow_vava <- growth(
  transition_matrix = tm_vava,
  global_stochasticity = species_dat_pva_eg$stoch[j_vava],
  transition_function = modified_transition(
    survival_layer = "sf_layer",
    fecundity_layer = "sf_layer"
  )
)

pop_dyn_vava <- population_dynamics(
  change = grow_vava,
  dispersal = disp_vava
)

simres <- simulation(
  landscape = lsc_vava,
  population_dynamics = pop_dyn_vava,
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



