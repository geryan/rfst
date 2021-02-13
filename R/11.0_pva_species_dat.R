## data for PVAs


source("R/spartan/spartan_settings.R")


library(dplyr)
library(tibble)
library(purrr)

load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")

source.functions("R/functions")


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


# tm_tyte <- matrix(
#   data = c(
#     0.00, 0.00, 0.55,
#     0.67, 0.00, 0.00,
#     0.00, 0.74, 0.80 
#   ),
#   nrow = 3,
#   ncol = 3,
#   byrow = TRUE,
#   dimnames = list(
#     c('Newborn','Sub-adult','Adult'),
#     c('Newborn','Sub-adult','Adult')
#   )
# )


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
     ~sp,     ~tm, ~popsize, ~cc, ~ccfun, ~stoch, ~max_disp,           ~habfun, ~max_cells,            ~dp,  ~pp, ~disp,  ~z,
  "gyle", tm_gyle,     3000,  60,  cc_60,   0.05,      2000, habitat.downupfun,         40, c(1, 0.2, 0.2), 0.98,  "ca", 0.4,
  "pevo", tm_pevo,     5000,  15,  cc_15,   0.05,      4000,     habitat.upfun,         80, c(1, 0.2, 0.2), 0.95,  "ca", 0.4,
  "peau", tm_peau,     2000,   3,   cc_3,   0.05,     20000,     habitat.upfun,         NA, c(1, 0.2, 0.2), 0.95,  "ke", 0.4,
  "smle", tm_smle,     1000,  75,  cc_75,   0.07,      2000,                NA,         10, c(1, 0.7, 0.6), 0.95,  "ca", 0.2,
  "tyte", tm_tyte,     1000,   2,   cc_2,   0.07,    100000, habitat.downupfun,       1000, c(1, 0.6, 0.6), 0.95,  "ca", 0.2,
  "vava", tm_vava,     5000,  25,  cc_25,   0.05,      5000,                NA,         NA, c(1, 0.2, 0.2),   NA,  "ke", 0.4,
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
