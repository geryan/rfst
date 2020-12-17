## Predict SDM


source("R/spartan/spartan_settings.R")


library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
#library(future)
#library(future.apply)
library(raster)
library(sp)
library(gbm)
library(dismo)


load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")
load(file = "output/RData/07_combined_variables.RData")
load(file = "output/RData/09_fit_distribution_models_2.RData")

source.functions("R/functions")

command_args <- commandArgs(trailingOnly = TRUE)

i <- as.numeric(command_args[1])

j <- 1


# for some reason paths here and below need to be full otherwise script fails to find the directory.

pred_out_path <- "/data/scratch/projects/punim0995/habitat_pred/"

agg_out_path <- "/data/gpfs/projects/punim0995/rfst/output/habitat_pred_aggregated/"

##############


pred <- brtpredict(
  variables = var_set$all_vars[[i]],
  model = sdm_results$brt.fit[[j]],
  out_path = pred_out_path,
  scn_id = var_set$cscnid[[i]], # NB needs cscnid not scn_id if multiple climate scenarios with each scenario
  varset = "",
  species = sdm_results$sp[[j]],
  initial = FALSE,
  pll = FALSE,
  ncores = 1
)

saveRDS(
  object = pred,
  file = sprintf(
    "%s/pred2_%s_%s.Rds",
    "/data/gpfs/projects/punim0995/rfst/output/spartan_RData/habitat_pred",
    var_set$cscnid[[i]], # NB needs cscnid not scn_id if multiple climate scenarios with each scenario
    sdm_results$sp[[j]]
  )
)

agg <- maggregate(
  x = pred,
  fact = 5,
  out_path = agg_out_path,
  aggname = "aggpred",
  scn_id = var_set$cscnid[[i]], # NB needs cscnid not scn_id if multiple climate scenarios with each scenario
  varset = "",
  species = sdm_results$sp[[j]]
)

saveRDS(
  object = agg,
  file = sprintf(
    "%s/agg2_%s_%s.Rds",
    "/data/gpfs/projects/punim0995/rfst/output/spartan_RData/habitat_pred_aggregated",
    var_set$cscnid[[i]], # NB needs cscnid not scn_id if multiple climate scenarios with each scenario
    sdm_results$sp[[j]]
  )
)

rm(pred)

vs <- var_set %>%
  dplyr::select(
    -dir,
    -th,
    -rc,
    -pb,
    -landis_vars,
    -dist_vars,
    -climate_projections,
    -geo_vars,
    -ll,
    -all_vars
  ) %>%
  mutate(
    sp = "pevo"
  )

rm(var_set)

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


load(file = "output/RData/04.1_mortality_aggregated5.RData")
load(file = "output/RData/04.3_tsl_aggregated.RData")

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


j <- which(species_dat_pva$sp == vs$sp[i])

k <- which(mort_agg5_ch$scn_id == vs$scn_id[[i]])

l <- which(tsl_agg_ch$scn_id == vs$scn_id[[i]])



habitat_map <- agg

mortality_map <- mort_agg5_ch$mort_agg[[k]]

disturbance_map <- tsl_agg_ch$tsl_agg5[[l]]


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

survival_fecundity_map <- logistic_sf(habitat_map2)

initial_population <- initpop2(
  hs = habitat_map2[[1]],
  popsize = species_dat_pva$popsize[j],
  cc = species_dat_pva$cc[j],
  ss = ss_pevo,
  pp = 0.95
)

# 

lsc <- landscape(
  population = initial_population,
  suitability = habitat_map2,
  "mortality" = mortality_map,
  #"sf_layer" = survival_fecundity_map,
  carrying_capacity = species_dat_pva$ccfun[[j]]
)

prop_dispersing <- c(
  1,
  rep(
    x = 0,
    times = 2
  )
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
  max_cells = 80,
  dispersal_proportion = set_proportion_dispersing(
    proportions = prop_dispersing
  ),
  barriers = NULL,
  use_suitability = TRUE,
  carrying_capacity = "carrying_capacity"
)

grow <- growth(
  #transition_matrix = species_dat_pva$tm[[j]],
  transition_matrix = tm_pevo,
  global_stochasticity = species_dat_pva$stoch[j]#,
  #transition_function = modified_transition(
  #  survival_layer = "sf_layer",
  #  fecundity_layer = "sf_layer"
  #)
)

pop_dyn <- population_dynamics(
  change = grow,
  dispersal = disp,
  density_dependence = ceiling_density(),
  modification = mortality(mortality_layer = "mortality")
)


# simres1 <- simulation(
#   landscape = lsc,
#   population_dynamics = pop_dyn,
#   demo_stochasticity = "full",
#   timesteps = 50,
#   replicates = 5,
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

pva_emp <- emp(simpop)

pva_emp_all <- emp.all(simpop)

pva_p_extinct <- length(which(pva_emp_all[[1]] == 0))/length(pva_emp_all[[1]])


med_pop <- med.pop(simpop)

med_pop_all <- med.pop.all(simpop)

pva_dat <- bind_cols(
  vs[i,],
  species_dat_pva[j,]
) %>%
  dplyr::select(-sp...11) %>%
  rename(sp = sp...10)

pva_res <- tibble(
  pva = list(simpop),
  emp = pva_emp,
  emp_all = pva_emp_all,
  med_pop,
  med_pop_all,
  p_extinct = pva_p_extinct,
) %>%
  mutate(
    p_extant = 1 - p_extinct
  )

pva_proj <- bind_cols(
  pva_dat,
  pva_res
) %>%
  mutate(
    habitat = "projected"
  )


saveRDS(
  object = pva_proj,
  file = sprintf(
    fmt = "%s/pva_proj_%s_%s.Rds",
    "/data/gpfs/projects/punim0995/rfst/output/spartan_RData/pva",
    vs$cscnid[i],
    vs$sp[i]
  )
)

rm(simres)

#

lsc <- landscape(
  population = initial_population,
  suitability = habitat_map3,
  "mortality" = mortality_map,
  #"sf_layer" = survival_fecundity_map,
  carrying_capacity = species_dat_pva$ccfun[[j]]
)

prop_dispersing <- c(
  1,
  rep(
    x = 0,
    times = 2
  )
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
  max_cells = 80,
  dispersal_proportion = set_proportion_dispersing(
    proportions = prop_dispersing
  ),
  barriers = NULL,
  use_suitability = TRUE,
  carrying_capacity = "carrying_capacity"
)

grow <- growth(
  #transition_matrix = species_dat_pva$tm[[j]],
  transition_matrix = tm_pevo,
  global_stochasticity = species_dat_pva$stoch[j]#,
  #transition_function = modified_transition(
  #  survival_layer = "sf_layer",
  #  fecundity_layer = "sf_layer"
  #)
)

pop_dyn <- population_dynamics(
  change = grow,
  dispersal = disp,
  density_dependence = ceiling_density(),
  modification = mortality(mortality_layer = "mortality")
)


# simres1 <- simulation(
#   landscape = lsc,
#   population_dynamics = pop_dyn,
#   demo_stochasticity = "full",
#   timesteps = 50,
#   replicates = 5,
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

pva_emp <- emp(simpop)

pva_emp_all <- emp.all(simpop)

pva_p_extinct <- length(which(pva_emp_all[[1]] == 0))/length(pva_emp_all[[1]])


med_pop <- med.pop(simpop)

med_pop_all <- med.pop.all(simpop)

pva_dat <- bind_cols(
  vs[i,],
  species_dat_pva[j,]
) %>%
  dplyr::select(-sp...11) %>%
  rename(sp = sp...10)

pva_res <- tibble(
  pva = list(simpop),
  emp = pva_emp,
  emp_all = pva_emp_all,
  med_pop,
  med_pop_all,
  p_extinct = pva_p_extinct,
) %>%
  mutate(
    p_extant = 1 - p_extinct
  )

pva_stat <- bind_cols(
  pva_dat,
  pva_res
) %>%
  mutate(
    habitat = "static"
  )


saveRDS(
  object = pva_stat,
  file = sprintf(
    fmt = "%s/pva_stat_%s_%s.Rds",
    "/data/gpfs/projects/punim0995/rfst/output/spartan_RData/pva",
    vs$cscnid[i],
    vs$sp[i]
  )
)

pva <- bind_rows(
  pva_stat,
  pva_proj
)


saveRDS(
  object = pva,
  file = sprintf(
    fmt = "%s/pva_both_%s_%s.Rds",
    "/data/gpfs/projects/punim0995/rfst/output/spartan_RData/pva",
    vs$cscnid[i],
    vs$sp[i]
  )
)