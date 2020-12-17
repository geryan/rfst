

source("R/spartan/spartan_settings.R")

library(raster)
library(dplyr)
library(tibble)
library(tidyr)


load(file = "output/RData/00_controls_eg.RData")
load(file = "output/RData/01_landscape_variables_eg.RData")

source.functions("R/functions")


file_list <- list.files(
  path = "/data/gpfs/projects/punim1340/rfst_eg/output/spartan_RData/mort_agg/",
  pattern = "mort_agg4"
)


ma_list <- lapply(
  X = file_list,
  FUN = function(x){
    z <- readRDS(
      file = sprintf(
        "/data/gpfs/projects/punim1340/rfst_eg/output/spartan_RData/mort_agg/%s",
        x
      )
    )
    
    return(z)
    
  }
)

mort_agg4_eg <- bind_rows(ma_list)# %>%
  #mutate(yscn_id = ifelse(is.na(yscn_id), scn_id, yscn_id)) %>%
  #dplyr::select(yscn_id, mort_agg)

save(
  mort_agg4_eg,
  file = "/data/gpfs/projects/punim0995/rfst/output/RData/04.1_mortality_aggregated4_eg.RData"
)
