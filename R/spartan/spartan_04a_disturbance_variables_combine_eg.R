
source("R/spartan/spartan_settings.R")


library(dplyr)
library(magrittr)
library(tibble)
library(raster)

load(file = "output/RData/00_controls_eg.RData")
load(file = "output/RData/01_landscape_variables_eg.RData")
load(file = "output/RData/03_LANDIS_variables_eg.RData")

source.functions("R/functions")

file_list <- list.files(
  path = sprintf(
    "%s/spartan_RData/dist_vars",
    out_path_eg
  )
)

yscn_id <- sub(
  pattern = "dvs_",
  replacement = "",
  x = file_list
) %>%
  sub(
    pattern = "_eg.Rds",
    replacement = "",
    x = .
  )

dist_vars <- lapply(
  X = file_list,
  FUN = function(x){
    z <- readRDS(
      file = sprintf(
        "%s/spartan_RData/dist_vars/%s",
        out_path_eg,
        x
      )
    )
    
    return(z)
    
  }
)


dvs <- tibble(yscn_id, dist_vars)


disturbance_variables_eg <- full_join(
  x = landis_variables_eg,
  y = dvs,
  by = "yscn_id"
)

save(
  disturbance_variables_eg,
  file = "output/RData/04_disturbance_variables_eg.RData"
) 


