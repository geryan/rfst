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
load(file = "output/RData/11.0_pva_species_dat.RData")

source.functions("R/functions")


command_args <- commandArgs(trailingOnly = TRUE)

i <- as.numeric(command_args[1])

j <- which(species_dat_pva$sp == agg5_ch$sp[i])

k <- which(mort_agg5_ch$scn_id == agg5_ch$scn_id[[i]])

l <- which(tsl_agg_ch$scn_id == agg5_ch$scn_id[[i]])



habitat_map <- agg5_ch$aggmap5[[i]]

mortality_map <- mort_agg5_ch$mort_agg[[k]]

disturbance_map <- tsl_agg_ch$tsl_agg5[[l]]


survival_fecundity_map <- logistic_sf(
  x = habitat_map,
  z = species_dat_pva$z[[j]]
)


initial_population <- initpop2(
  hs = habitat_map[[1]],
  popsize = species_dat_pva$popsize[j],
  cc = species_dat_pva$cc[j],
  ss = species_dat_pva$ss[[j]],
  pp = species_dat_pva$pp[[j]]
)


lsc <- landscape(
  population = initial_population,
  suitability = habitat_map,
  "mortality" = mortality_map,
  "sf_layer" = survival_fecundity_map,
  carrying_capacity = species_dat_pva$ccfun[[j]]
)


# disp <- kernel_dispersal(
#   dispersal_kernel = exponential_dispersal_kernel(
#     distance_decay = species_dat_pva$max_disp[j]/2
#   ),
#   max_distance = species_dat_pva$max_disp[j],
#   arrival_probability = "both",
#   dispersal_proportion = set_proportion_dispersing(
#     proportions = prop_dispersing
#   )
# )

disp <- cellular_automata_dispersal(
  max_cells = species_dat_pva$max_cells[[j]],
  dispersal_proportion = set_proportion_dispersing(
    proportions = species_dat_pva$dp[[j]]
  ),
  barriers = NULL,
  use_suitability = TRUE,
  carrying_capacity = "carrying_capacity"
)

grow <- growth(
  transition_matrix = species_dat_pva$tm[[j]],
  global_stochasticity = species_dat_pva$stoch[j],
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
#   timesteps = 5,
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
  agg5_ch[i,],
  species_dat_pva[j,]
) %>%
  dplyr::select(-sp...11) %>%
  rename(sp = sp...9)

pva_res <- tibble(
  pva = list(simpop),
  emp = pva_emp,
  emp_all = pva_emp_all,
  med_pop,
  med_pop_all,
  p_extinct = pva_p_extinct,
  lcc
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
    "/data/gpfs/projects/punim0995/rfst/output/spartan_RData/pva",
    agg5_ch$cscnid[i],
    agg5_ch$sp[i]
  )
)

