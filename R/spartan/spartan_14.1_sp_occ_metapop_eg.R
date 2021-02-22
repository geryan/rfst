# get 14.1 Species occurrence data processing

source("R/spartan/spartan_settings.R")

library(magrittr)
library(raster)
library(sf)
library(tidyr)
library(dplyr)



load(file = "output/RData/00_controls_eg.RData")
load(file = "output/RData/01_landscape_variables_eg.RData")
load(file = "output/RData/14.0_sp_occ_metapop_list_eg.RData")

eg_grid <- readRDS(file = "output/RData/14.00_grid_eg.Rds")

source.functions("R/functions")

command_args <- commandArgs(trailingOnly = TRUE)

i <- as.numeric(command_args[1])


pad <- buff.sample.pa(
  x = vba_dat_eg,
  rfa = eg_rfa,
  cellsize = 200,
  buff.dist = 500,
  species = pa_list_eg$species[i],
  survey_method = pa_list_eg$survey_methods[i],
  sg = eg_grid
  
)


saveRDS(
  object = pad,
  file = sprintf(
    "%s/pa_eg_%s.Rds",
    "/data/gpfs/projects/punim0995/rfst/output/spartan_RData/pa_metapop/eg",
    pa_list_eg$sp[i]
  )
)


st_write(
  obj = pad,
  dsn = sprintf(
    "%s/pa_eg_%s.shp",
    "/data/gpfs/projects/punim0995/rfst/output/pa_metapop/eg",
    pa_list_eg$sp[i]
  ),
  delete_dsn = TRUE
)


st_write(
  obj = pad,
  dsn = sprintf(
    "%s/pa_eg_%s.csv",
    "/data/gpfs/projects/punim0995/rfst/output/pa_metapop/eg",
    pa_list_eg$sp[i]
  ),
  layer_options = "GEOMETRY=AS_XY",
  delete_dsn = TRUE
)


