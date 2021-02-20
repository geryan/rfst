## data for PVAs


source("R/spartan/spartan_settings.R")


library(dplyr)
library(tibble)
library(purrr)

load(file = "output/RData/00_controls_eg.RData")
load(file = "output/RData/01_landscape_variables_eg.RData")

source.functions("R/functions")


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


tm_pevo <- matrix(
  data = c(
    0.00, 0.00, 0.50,
    0.50, 0.00, 0.00,
    0.00, 0.80, 0.84
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
    0.29, 0.00, 0.00,
    0.00, 0.05, 0.05
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


species_dat_pva_eg <- tribble(
     ~sp,     ~tm, ~popsize, ~cc, ~ccfun, ~stoch, ~max_disp,           ~habfun, ~max_cells,            ~dp,  ~pp, ~disp,  ~z,
  "polo", tm_polo,     1500,   8,   cc_8,   0.05,      2000,                NA,         20,       c(1, 0.2),  0.99,  "ca", 0.4,
  "pevo", tm_pevo,     5000,  15,  cc_15,   0.05,      4000,     habitat.upfun,         80,  c(1, 0.2, 0.2),  0.95,  "ca", 0.4,
  "peau", tm_peau,     2000,   3,   cc_3,   0.05,     20000,     habitat.upfun,        500,    c(1, 1, 0.5), 0.991,  "ca", 0.4,
  "smle", tm_smle,     1000,  75,  cc_75,   0.07,      2000,                NA,         10,  c(1, 0.7, 0.6),  0.95,  "ca", 0.2,
  "tyte", tm_tyte,     1000,   2,   cc_2,   0.07,    100000, habitat.downupfun,     100000,  c(1, 0.6, 0.6),  0.95,  "ca", 0.2,
  "vava", tm_vava,     5000,  25,  cc_25,   0.05,      5000,                NA,        100, c(1, 1, 1, 0.3), 0.995,  "ca", 0.4,
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


egf20vals <- which(getValues(eg_fire_history_20) == 2020)

eg_fire_20 <- eg_mask

eg_fire_20[egf20vals] <- 0

egf20 <- aggregate(eg_fire_20, fact = 2, fun = min)


save(
  species_dat_pva_eg,
  egf20,
  file = "output/RData/11.0_pva_species_dat_eg.RData"
)
