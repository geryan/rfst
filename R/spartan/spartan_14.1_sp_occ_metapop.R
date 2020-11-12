# get 14.0 Species occurrence lists for metapopulation capacity models

source("R/spartan/spartan_settings.R")

library(magrittr)
library(raster)
library(sf)
library(tidyr)
library(dplyr)



load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")
load(file = "output/RData/14.0_sp_occ_metapop_list.RData")

source.functions("R/functions")

command_args <- commandArgs(trailingOnly = TRUE)

i <- as.numeric(command_args[1])


pad <- buff.sample.pa(
  x = vba_dat_ch %>% filter(species == "Pseudophryne semimarmorata"),
  rfa = ch_rfa,
  cellsize = 200,
  buff.dist = 500,
  species = pa_list_ch$species[i],
  survey_method = pa_list_ch$survey_methods[i]
)


saveRDS(
  object = pad,
  file = sprintf(
    "%s/pa_ch_%s.Rds",
    "/data/gpfs/projects/punim0995/rfst/output/spartan_RData/pa_metapop/ch",
    pa_list_ch$sp[i]
  )
)


st_write(
  obj = pad,
  dsn = sprintf(
    "output/pa_metapop/ch/pa_ch_%s.shp",
    pa_list_ch$sp[i]
  ),
  delete_dsn = TRUE
)


st_write(
  obj = pad,
  dsn = sprintf(
    "output/pa_metapop/ch/pa_ch_%s.csv",
    pa_list_ch$sp[i]
  ),
  layer_options = "GEOMETRY=AS_XY",
  delete_dsn = TRUE
)


