

source("R/spartan/spartan_settings.R")

library(raster)
library(dplyr)
library(tibble)
library(tidyr)


load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")

source.functions("R/functions")


file_list <- list.files(
  path = "/data/gpfs/projects/punim0995/rfst/output/spartan_RData/habitat_pred_aggregated/",
  pattern = "agg5"
)


agg5_list <- lapply(
  X = file_list,
  FUN = function(x){
    z <- readRDS(
      file = sprintf(
        "/data/gpfs/projects/punim0995/rfst/output/spartan_RData/habitat_pred_aggregated/%s",
        x
      )
    )
    
    return(z)
    
  }
)

agg5_ch <- bind_rows(agg5_list)



save(
  agg5_ch,
  file = "/data/gpfs/projects/punim0995/rfst/output/RData/10.1_aggregate_sdm.RData"
)
