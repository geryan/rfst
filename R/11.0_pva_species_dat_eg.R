## data for PVAs


source("R/spartan/spartan_settings.R")


library(dplyr)
library(tibble)
library(purrr)

load(file = "output/RData/00_controls_eg.RData")
load(file = "output/RData/01_landscape_variables_eg.RData")

source.functions("R/functions")


# polo -------------------

tm_polo <- matrix(
  data = c(
    0.00, 1.00,
    0.37, 0.63
  ),
  nrow = 2,
  ncol = 2,
  byrow = TRUE,
  dimnames = list(
    c('Newborn','Adult'),
    c('Newborn','Adult')
  )
)

rmax(tm_polo)
rmax(altm(tm_polo))

cc_polo <- function (
  landscape,
  timestep
) {
  
  library(raster)
  
  fun <- function(
    suitability,
    carcap = 8,
    z = 0.7,
    threshold = 0.454
  ) {
    
    ifelse(
      suitability < threshold,
      0,
      round(carcap * 1/(1 + exp(-10*(suitability - z))))
    )
  }
  
  suit <- landscape$suitability
  if (raster::nlayers(suit) > 1) {
    suit <- suit[[timestep]]
  }
  
  calc(suit, fun)
  
} 

# pevo ---------------------------------

# tm_pevo <- matrix(
#   data = c(
#     0.00, 0.00, 0.50,
#     0.50, 0.00, 0.00,
#     0.00, 0.80, 0.84
#   ),
#   nrow = 3,
#   ncol = 3,
#   byrow = TRUE,
#   dimnames = list(
#     c('Newborn','Juvenile','Adult'),
#     c('Newborn','Juvenile','Adult')
#   )
# )

tm_pevo <- matrix(
  data = c(
    0.00, 0.00, 0.50,
    0.50, 0.00, 0.00,
    0.00, 0.79, 0.80
  ),
  nrow = 3,
  ncol = 3,
  byrow = TRUE,
  dimnames = list(
    c('Newborn','Juvenile','Adult'),
    c('Newborn','Juvenile','Adult')
  )
)

rmax(tm_pevo)
rmax(altm(tm_pevo))

cc_pevo <- function (
  landscape,
  timestep
) {
  
  library(raster)
  
  fun <- function(
    suitability,
    carcap = 8,
    z = 0.6,
    threshold = 0.179
  ) {
    
    ifelse(
      suitability < threshold,
      0,
      round(carcap * 1/(1 + exp(-10*(suitability - z))))
    )
  }
  
  suit <- landscape$suitability
  if (raster::nlayers(suit) > 1) {
    suit <- suit[[timestep]]
  }
  
  calc(suit, fun)
  
}  

# peau -------------------------------------

# tm_peau <- matrix(
#   data = c(
#     0.0, 0.0, 0.5,
#     0.7, 0.0, 0.0,
#     0.0, 0.6, 0.8
#   ),
#   nrow = 3,
#   ncol = 3,
#   byrow = TRUE,
#   dimnames = list(
#     c('Newborn','Juvenile','Adult'),
#     c('Newborn','Juvenile','Adult')
#   )
# )

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

rmax(tm_peau)
rmax(altm(tm_peau))

cc_peau <- function (
  landscape,
  timestep
) {
  
  library(raster)
  
  fun <- function(
    suitability,
    carcap = 3,
    z = 0.7,
    threshold = 0.62
  ) {
    
    ifelse(
      suitability < threshold,
      0,
      round(carcap * 1/(1 + exp(-10*(suitability - z))))
    )
  }
  
  suit <- landscape$suitability
  if (raster::nlayers(suit) > 1) {
    suit <- suit[[timestep]]
  }
  
  calc(suit, fun)
  
}   


# tyte ------------------------

# tm_tyte <- matrix(
#   data = c(
#     0.00, 0.00, 1.50,
#     0.55, 0.00, 0.00,
#     0.00, 0.70, 0.75 
#   ),
#   nrow = 3,
#   ncol = 3,
#   byrow = TRUE,
#   dimnames = list(
#     c('Newborn','Sub-adult','Adult'),
#     c('Newborn','Sub-adult','Adult')
#   )
# )

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
    0.00, 0.00, 0.74,
    0.62, 0.00, 0.00,
    0.00, 0.66, 0.7 
  ),
  nrow = 3,
  ncol = 3,
  byrow = TRUE,
  dimnames = list(
    c('Newborn','Sub-adult','Adult'),
    c('Newborn','Sub-adult','Adult')
  )
)

rmax(tm_tyte)

rmax(altm(tm_tyte))

cc_tyte <- function (
  landscape,
  timestep
) {
  
  library(raster)
  
  fun <- function(
    suitability,
    carcap = 3,
    z = 0.3,
    threshold = 0.157
  ) {
    
    ifelse(
      suitability < threshold,
      0,
      round(carcap * 1/(1 + exp(-10*(suitability - z))))
    )
  }
  
  suit <- landscape$suitability
  if (raster::nlayers(suit) > 1) {
    suit <- suit[[timestep]]
  }
  
  calc(suit, fun)
  
}

# vava ----------------------------

# tm_vava <- matrix(
#   data = c(
#     0.00, 0.00, 0.00, 2.69,
#     0.36, 0.00, 0.00, 0.00, 
#     0.00, 0.36, 0.00, 0.00,
#     0.00, 0.00, 0.36, 0.93
#   ),
#   nrow = 4,
#   ncol = 4,
#   byrow = TRUE,
#   dimnames = list(
#     c('Hatchling','Juvenile', "Sub-adult",'Adult'),
#     c('Hatchling','Juvenile', "Sub-adult",'Adult')
#   )
# )

tm_vava <- matrix(
  data = c(
    0.00, 0.00, 0.00, 2.44,
    0.36, 0.00, 0.00, 0.00, 
    0.00, 0.36, 0.00, 0.00,
    0.00, 0.00, 0.36, 0.88
  ),
  nrow = 4,
  ncol = 4,
  byrow = TRUE,
  dimnames = list(
    c('Hatchling','Juvenile', "Sub-adult",'Adult'),
    c('Hatchling','Juvenile', "Sub-adult",'Adult')
  )
)

rmax(tm_vava)

rmax(altm(tm_vava))

cc_vava <- function (
  landscape,
  timestep
) {
  
  library(raster)
  
  fun <- function(
    suitability,
    carcap = 16,
    z = 0.65,
    threshold = 0.236
  ) {
    
    ifelse(
      suitability < threshold,
      0,
      round(carcap * 1/(1 + exp(-10*(suitability - z))))
    )
  }
  
  suit <- landscape$suitability
  if (raster::nlayers(suit) > 1) {
    suit <- suit[[timestep]]
  }
  
  calc(suit, fun)
  
}

# table ----------------------------------------------------
# sp    threshold_max_sss
# <chr>             <dbl>
# 1 peau              0.62 
# 2 pevo              0.179
# 3 polo              0.454
# 4 tyte              0.157
# 5 vava              0.236

species_dat_pva_eg <- tribble(
  ~sp,     ~tm, ~popsize, ~cc,  ~ccfun, ~stoch, ~max_disp,           ~habfun, ~max_cells,                   ~dp,   ~pp, ~disp,  ~z, ~scale, ~threshold,
  "polo", tm_polo,     1500,   8, cc_polo,   0.05,      2000,                NA,         10,           c(1, 0.2),  0.99,  "ca", 0.7,    500,      0.454,
  "pevo", tm_pevo,     5000,   8, cc_pevo,   0.05,      4000,     habitat.upfun,         20,      c(1, 0.2, 0.0),  0.95,  "ca", 0.6,    500,      0.179,
  "peau", tm_peau,     3000,   3, cc_peau,   0.05,     20000,     habitat.upfun,        100,      c(1, 0.2, 0.0),  0.95,  "ca", 0.7,   1000,      0.620,
  "tyte", tm_tyte,     1000,   3, cc_tyte,   0.07,    100000, habitat.downupfun,        200,      c(1, 0.6, 0.0),  0.95,  "ca", 0.3,   1000,      0.157,
  "vava", tm_vava,     5000,  16, cc_vava,   0.05,      5000,                NA,         20, c(1, 0.5, 0.2, 0.0),  0.95,  "ca", 0.65,    500,      0.236
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
