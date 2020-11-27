## data for PVAs


source("R/spartan/spartan_settings.R")


library(dplyr)
library(tibble)

load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")

source.functions("R/functions")


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


species_dat_pva <- tribble(
  ~sp,    ~tm,     ~popsize, ~cc, ~ccfun, ~stoch, ~max_disp, ~habfun,
  "gyle", tm_gyle, 3000,     245, cc_245, 0.2,    2000,      habitat.downupfun,
  "pevo", tm_pevo, 5000,     60,  cc_60,  0.2,    4000,      habitat.upfun,
  "peau", tm_peau, 2000,     10,  cc_10,  0.2,    20000,     habitat.upfun,
  "smle", tm_smle, 1000,     309, cc_309, 0.6,    2000,      NA,
  "tyte", tm_tyte, 1000,     1,   cc_1,   0.4,    20000,     habitat.downupfun,
  "vava", tm_vava, 5000,     100, cc_100, 0.2,    5000,      NA
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

save(
  species_dat_pva,
  file = "output/RData/11.0_pva_species_dat.RData"
)
