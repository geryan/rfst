
source("R/spartan/spartan_settings.R")


library(dplyr)
library(magrittr)
library(tibble)
library(raster)

load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")
load(file = "output/RData/03_LANDIS_variables.RData")

source.functions("R/functions")

file_list <- list.files("output/spartan_RData/dist_vars/")

scn_id <- sub(
  pattern = "dvs_",
  replacement = "",
  x = file_list
) %>%
  sub(
    pattern = ".Rds",
    replacement = "",
    x = .
  )

dist_vars <- lapply(
  X = file_list,
  FUN = function(x){
    z <- readRDS(
      file = sprintf(
        "output/spartan_RData/dist_vars/%s",
        x
      )
    )
    
    return(z)
    
  }
)


dvs <- tibble(scn_id, dist_vars)


disturbance_variables <- full_join(
  x = landis_variables,
  y = dvs,
  by = "scn_id"
)

save(
  disturbance_variables,
  file = "output/RData/04_disturbance_variables.RData"
) 


