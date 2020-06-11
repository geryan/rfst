
source("R/spartan/spartan_settings.R")


library(dplyr)
library(magrittr)
library(tibble)
library(raster)

load(file = "output/RData/00_controls.RData")

source.functions("R/functions")

file_list <- list.files("output/landscape_vars/landis_RDS/")

scn_id <- sub(
  pattern = "landis_vars_",
  replacement = "",
  x = file_list
) %>%
  sub(
    pattern = ".Rds",
    replacement = "",
    x = .
  )

landis_vars <- lapply(
  X = file_list,
  FUN = function(x){
    z <- readRDS(
      file = sprintf(
        "output/landscape_vars/landis_RDS/%s",
        x
      )
    )
    
    return(z)
    
  }
)


lvs <- tibble(scn_id, landis_vars)


landis_variables <- full_join(
  x = scn_table,
  y = lvs,
  by = "scn_id"
  )

save(
  landis_vars,
  landis_variables,
  file = "output/RData/03_LANDIS_variables.RData"
)
