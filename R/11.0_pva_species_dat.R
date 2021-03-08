## data for PVAs


source("R/spartan/spartan_settings.R")


library(dplyr)
library(tibble)
library(purrr)

load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")

source.functions("R/functions")

## gyle -------------------

tm_gyle <- matrix(
  data = c(
    0.00, 0.00, 0.75,
    0.52, 0.00, 0.00,
    0.00, 0.65, 0.75
  ),
  nrow = 3,
  ncol = 3,
  byrow = TRUE,
  dimnames = list(
    c('Newborn','Juvenile','Adult'),
    c('Newborn','Juvenile','Adult')
  )
)

rmax(tm_gyle)
rmax(altm(tm_gyle))

cc_gyle <- function (
  landscape,
  timestep
) {
  
  library(raster)
  
  fun <- function(
    suitability,
    carcap = 50,
    z = 0.4,
    threshold = 0.347
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

tm_pevo <- matrix(
  data = c(
    0.00, 0.00, 0.48,
    0.50, 0.00, 0.00,
    0.00, 0.79, 0.81
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
    carcap = 10,
    z = 0.4,
    threshold = 0.2
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

tm_peau <- matrix(
  data = c(
    0.0, 0.0, 0.5,
    0.7, 0.0, 0.0,
    0.0, 0.6, 0.83
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
    z = 0.3,
    threshold = 0.117
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


# smle -------------------------------

tm_smle <- matrix(
  data = c(
    0.00, 3.3, 3.3,
    0.29, 0.00, 0.00,
    0.00, 0.01, 0.01
  ),
  nrow = 3,
  ncol = 3,
  byrow = TRUE,
  dimnames = list(
    c('Newborn', 'Adult', "Dummy"),
    c('Newborn', 'Adult', "Dummy")
  )
)

rmax(tm_smle)

rmax(altm(tm_smle))

cc_smle <- function (
  landscape,
  timestep
) {
  
  library(raster)
  
  fun <- function(
    suitability,
    carcap = 75,
    z = 0.5,
    threshold = 0.041
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
    z = 0.2,
    threshold = 0.141
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

tm_vava <- matrix(
  data = c(
    0.00, 0.00, 0.00, 2.43,
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
    carcap = 25,
    z = 0.5,
    threshold = 0.086
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

## threshold matrix -----------------------

# sp    threshold_max_sss
# <chr>             <dbl>
#   1 gyle              0.347
# 2 peau              0.117
# 3 smle              0.041
# 4 tyte              0.141
# 5 vava              0.086
# 6 pevo              0.2


## table ------------------

# changed peau from 400 to 100 max cells
# changed tyte from 1000 to 200 max cells 

species_dat_pva <- tribble(
     ~sp,     ~tm, ~popsize, ~cc,  ~ccfun, ~stoch, ~max_disp,           ~habfun, ~max_cells,             ~dp,   ~pp, ~disp,  ~z, ~scale, ~threshold,
  "gyle", tm_gyle,     3000,  50, cc_gyle,   0.05,      2000, habitat.downupfun,         40,  c(1, 0.2, 0.0),  0.98,  "ca", 0.5,    500,      0.347,
  "pevo", tm_pevo,     5000,  10, cc_pevo,   0.05,      4000,     habitat.upfun,         80,  c(1, 0.2, 0.0),  0.95,  "ca", 0.4,    500,        0.2,
  "peau", tm_peau,     3000,   3, cc_peau,   0.05,     20000,     habitat.upfun,        100,  c(1, 0.5, 0.0),  0.90,  "ca", 0.3,   1000,      0.117,
  "smle", tm_smle,     1000,  75, cc_smle,   0.07,      2000,                NA,         10,  c(1, 0.0, 0.0),  0.99,  "ca", 0.5,    500,      0.041,
  "tyte", tm_tyte,     1000,   3, cc_tyte,   0.07,    100000, habitat.downupfun,        200,  c(1, 0.6, 0.0),  0.95,  "ca", 0.2,   1000,      0.141,
  "vava", tm_vava,     5000,  25, cc_vava,   0.05,      5000,                NA,         20, c(1, 0.5, 0.2, 0.0), 0.99,  "ca", 0.5,    500,      0.086
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
