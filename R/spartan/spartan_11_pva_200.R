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
load(file = "output/RData/04.1_mortality_aggregated.RData")
load(file = "output/RData/10_predict_SDMs_agg.RData")


source.functions("R/functions")


command_args <- commandArgs(trailingOnly = TRUE)

i <- as.numeric(command_args[1])


tm_gyle <- matrix(
  data = c(
    0.00, 0.00, 0.80,
    0.50, 0.00, 0.00,
    0.00, 0.60, 0.88
  ),
  nrow = 3,
  ncol = 3,
  byrow = TRUE,
  dimnames = list(
    c('Newborn','Juvenile','Adult'),
    c('Newborn','Juvenile','Adult')
  )
)


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


tm_tyte <- matrix(
  data = c(
    0.00, 0.00, 0.55,
    0.67, 0.00, 0.00,
    0.00, 0.74, 0.80 
  ),
  nrow = 3,
  ncol = 3,
  byrow = TRUE,
  dimnames = list(
    c('Newborn','Sub-adult','Adult'),
    c('Newborn','Sub-adult','Adult')
  )
)


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


species_dat <- tribble(
  ~sp,    ~tm,     ~popsize, ~cc, ~ccfun, ~stoch, ~max_disp,
  "gyle", tm_gyle, 3000,     245, cc_245, 0.2,    2000,
  "pevo", tm_pevo, 5000,     60,  cc_60,  0.2,    4000,
  "peau", tm_peau, 2000,     10,  cc_10,  0.2,    20000,
  "smle", tm_smle, 1000,     309, cc_309, 0.6,    2000,
  "tyte", tm_tyte, 1000,     1,   cc_1,   0.4,    20000,
  "vava", tm_vava, 5000,     100, cc_100, 0.2,    5000
) %>% mutate(
  ss = map(
    .x = tm,
    .f = get.stable.states
  ),
  stages = map(
    .x = ss,
    .f = length
  ) %>%
    unlist
)


j <- which(species_dat$sp == agg_set$sp[i])

k <- which(mort_agg_ch$scn_id == agg_set$scn_id[[i]])


initial_population <- initpop2(
  hs = agg_set$aggmaps[[i]][[1]],
  popsize = species_dat$popsize[j],
  cc = species_dat$cc[j],
  ss = species_dat$ss[[j]]
)

sf_layer <- logistic_sf(x = agg_set$aggmaps[[i]])

lsc <- landscape(
  population = initial_population,
  suitability = agg_set$aggmaps[[i]],
  "mortality" = mort_agg_ch$mort_agg[[k]],
  "sf_layer" = sf_layer,
  carrying_capacity = species_dat$ccfun[[j]]
)

prop_dispersing <- c(
  1,
  rep(
    x = 0,
    times = species_dat$stages[j] - 1
  )
)

disp <- kernel_dispersal(
  dispersal_kernel = exponential_dispersal_kernel(
    distance_decay = species_dat$max_disp[j]/2
  ),
  max_distance = species_dat$max_disp[j],
  arrival_probability = "both",
  dispersal_proportion = set_proportion_dispersing(
    proportions = prop_dispersing
  )
)


grow <- growth(
  transition_matrix = species_dat$tm[[j]],
  global_stochasticity = species_dat$stoch[j],
  transition_function = modified_transition(
    survival_layer = "sf_layer",
    fecundity_layer = "sf_layer"
  )
)



pop_dyn <- population_dynamics(
  change = grow,
  dispersal = disp,
  modification = mortality(mortality_layer = "mortality")
)



# simres <- simulation(
#   landscape = lsc,
#   population_dynamics = pop_dyn,
#   demo_stochasticity = "full",
#   timesteps = 20,
#   replicates = 5,
#   verbose = TRUE
)


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

pva_dat <- bind_cols(
  agg_set[i,],
  species_dat[j,]
) %>%
  dplyr::select(-sp...11) %>%
  rename(sp = sp...9)

pva_res <- tibble(
  pva = list(simpop),
  emp = pva_emp,
  emp_all = pva_emp_all
)

pva <- bind_cols(
  pva_dat,
  pva_res
)


saveRDS(
  object = pva,
  file = sprintf(
    fmt = "%s/pva_%s_%s.Rds",
    "/data/gpfs/projects/punim0995/rfst/output/spartan_RData/pva",
    agg_set$cscnid[i],
    agg_set$sp[i]
  )
)

}