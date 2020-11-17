# Run PVAs combine

source("R/spartan/spartan_settings.R")

library(raster)
library(dplyr)
library(tibble)
library(tidyr)


load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")

source.functions("R/functions")


file_list <- list.files("/data/gpfs/projects/punim0995/rfst/output/spartan_RData/pva/")


pva_list <- lapply(
  X = file_list,
  FUN = function(x){
    z <- readRDS(
      file = sprintf(
        "/data/gpfs/projects/punim0995/rfst/output/spartan_RData/pva/%s",
        x
      )
    )
    
    return(z)
    
  }
)

pva_results_ch <- bind_rows(pva_list)



save(
  pva_results_ch,
  file = "/data/gpfs/projects/punim0995/rfst/output/RData/11_pva.RData"
)
