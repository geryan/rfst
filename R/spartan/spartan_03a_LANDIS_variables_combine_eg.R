
source("R/spartan/spartan_settings.R")


library(dplyr)
library(magrittr)
library(tibble)
library(raster)

load(file = "output/RData/00_controls_eg.RData")

source.functions("R/functions")

file_list <- list.files(
  path = sprintf(
    "%s/%s",
    out_path_eg,
    "landscape_vars/landis_RDS/"
  )
)

yscn_id <- sub(
  pattern = "landis_vars_",
  replacement = "",
  x = file_list
) %>%
  sub(
    pattern = ".Rds",
    replacement = "",
    x = .
  )


landis_vars_eg <- lapply(
  X = file_list,
  FUN = function(x){
    z <- readRDS(
      file = sprintf(
        "%s/landscape_vars/landis_RDS/%s",
        out_path_eg,
        x
      )
    )
    
    return(z)
    
  }
)


lvs <- tibble(yscn_id, landis_vars)


landis_variables_eg <- full_join(
  x = scn_table_eg,
  y = lvs,
  by = "yscn_id"
  )

save(
  landis_vars_eg,
  landis_variables_eg,
  file = "output/RData/03_LANDIS_variables_eg.RData"
)
