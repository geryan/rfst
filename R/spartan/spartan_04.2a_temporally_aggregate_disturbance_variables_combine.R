# 18.1 combine  metapopulation capacity model results

source("R/spartan/spartan_settings.R")

library(raster)
library(dplyr)
library(tibble)
library(tidyr)


load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")

source.functions("R/functions")


file_list <- list.files("/data/gpfs/projects/punim0995/rfst/output/spartan_RData/ag_dist/")


ad_list <- lapply(
  X = file_list,
  FUN = function(x){
    z <- readRDS(
      file = sprintf(
        "/data/gpfs/projects/punim0995/rfst/output/spartan_RData/ag_dist/%s",
        x
      )
    )
    
    return(z)
    
  }
)

temporally_agg_dist_vars <- bind_rows(ad_list)



save(
  temporally_agg_dist_vars,
  file = "/data/gpfs/projects/punim0995/rfst/output/RData/04.2_temporally_agg_dist_vars.RData"
)
