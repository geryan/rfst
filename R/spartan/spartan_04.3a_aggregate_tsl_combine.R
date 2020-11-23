

source("R/spartan/spartan_settings.R")

library(raster)
library(dplyr)
library(tibble)
library(tidyr)


load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")

source.functions("R/functions")


file_list <- list.files("/data/gpfs/projects/punim0995/rfst/output/spartan_RData/tsl_agg/")


tsl_list <- lapply(
  X = file_list,
  FUN = function(x){
    z <- readRDS(
      file = sprintf(
        "/data/gpfs/projects/punim0995/rfst/output/spartan_RData/tsl_agg/%s",
        x
      )
    )
    
    return(z)
    
  }
)

tsl_agg_ch <- bind_rows(tsl_list)



save(
  tsl_agg_ch,
  file = "/data/gpfs/projects/punim0995/rfst/output/RData/04.3_tsl_aggregated.RData"
)
