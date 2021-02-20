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

agg_set_eg <- agg_set_eg %>%
  filter(sp != "pevo")

command_args <- commandArgs(trailingOnly = TRUE)

i <- as.numeric(command_args[1])

j <- which(species_dat_pva_eg$sp == agg_set_eg$sp[i])

k <- which(mort_agg4_eg$yscn_id == agg_set_eg$yscn_id[[i]])

l <- which(tsl_agg_eg$yscn_id == agg_set_eg$yscn_id[[i]])

yearid <- agg_set_eg$yearid[[i]]

habitat_map <- agg_set_eg$aggmaps[[i]]

mortality_map <- mort_agg4_eg$mort_agg[[k]]

disturbance_map <- tsl_agg_eg$tsl_agg[[l]]


survival_fecundity_map <- logistic_sf(
  x = habitat_map,
  z = species_dat_pva_eg$z[[j]]
)


initial_population <- initpop2(
  hs = habitat_map[[1]],
  popsize = species_dat_pva_eg$popsize[j],
  cc = species_dat_pva_eg$cc[j],
  ss = species_dat_pva_eg$ss[[j]],
  pp = species_dat_pva_eg$pp[[j]]
)


if(yearid == "EG20"){
  initial_population = initial_population*egf20
}


lsc <- landscape(
  population = initial_population,
  suitability = habitat_map,
  "mortality" = mortality_map,
  "sf_layer" = survival_fecundity_map,
  carrying_capacity = species_dat_pva_eg$ccfun[[j]]
)


# disp <- kernel_dispersal(
#   dispersal_kernel = exponential_dispersal_kernel(
#     distance_decay = species_dat_pva_eg$max_disp[j]/2
#   ),
#   max_distance = species_dat_pva_eg$max_disp[j],
#   arrival_probability = "both",
#   dispersal_proportion = set_proportion_dispersing(
#     proportions = prop_dispersing
#   )
# )

disp <- cellular_automata_dispersal(
  max_cells = species_dat_pva_eg$max_cells[[j]],
  dispersal_proportion = set_proportion_dispersing(
    proportions = species_dat_pva_eg$dp[[j]]
  ),
  barriers = NULL,
  use_suitability = TRUE,
  carrying_capacity = "carrying_capacity"
)

grow <- growth(
  transition_matrix = species_dat_pva_eg$tm[[j]],
  global_stochasticity = species_dat_pva_eg$stoch[j],
  transition_function = modified_transition(
    survival_layer = "sf_layer",
    fecundity_layer = "sf_layer"
  )
)

pop_dyn <- population_dynamics(
  change = grow,
  dispersal = disp,
  density_dependence = ceiling_density(),
  modification = mortality(mortality_layer = "mortality")
)


# simres <- simulation(
#   landscape = lsc,
#   population_dynamics = pop_dyn,
#   demo_stochasticity = "full",
#   timesteps = 50,
#   replicates = 3,
#   verbose = TRUE
# )



simres <- simulation(
  landscape = lsc,
  population_dynamics = pop_dyn,
  demo_stochasticity = "full",
  timesteps = ntimesteps,
  replicates = nreplicates,
  verbose = FALSE
)


simpop <- get_pop_simulation(simres)
lcc    <- get_lcc_simulation(simres)

pva_emp <- emp(simpop)

pva_emp_all <- emp.all(simpop)

pva_p_extinct <- length(which(pva_emp_all[[1]] == 0))/length(pva_emp_all[[1]])


med_pop <- med.pop(simpop)

med_pop_all <- med.pop.all(simpop)


pva_dat <- bind_cols(
  agg_set_eg[i,],
  species_dat_pva_eg[j,]
) %>%
  dplyr::select(-sp...14) %>%
  rename(sp = sp...12)

init_pop <- sum(getValues(initial_population), na.rm = TRUE)

pva_res <- tibble(
  pva = list(simpop),
  emp = pva_emp,
  emp_all = pva_emp_all,
  med_pop,
  med_pop_all,
  p_extinct = pva_p_extinct,
  lcc = list(lcc),
  init_pop
) %>%
  mutate(
    p_extant = 1 - p_extinct
  )

pva <- bind_cols(
  pva_dat,
  pva_res
)


saveRDS(
  object = pva,
  file = sprintf(
    fmt = "%s/pva5_%s_%s.Rds",
    "/data/gpfs/projects/punim1340/rfst_eg/output/spartan_RData/pva",
    agg_set_eg$ycscnid[i],
    agg_set_eg$sp[i]
  )
)

