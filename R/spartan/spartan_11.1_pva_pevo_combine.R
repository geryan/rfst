# Run PVAs combine

source("R/spartan/spartan_settings.R")

library(raster)
library(dplyr)
library(tibble)
library(tidyr)


load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")

source.functions("R/functions")


pp_list <- list.files(
  path = "/data/gpfs/projects/punim0995/rfst/output/spartan_RData/pva/",
  pattern = "pva_proj"
)


pva_pp_list <- lapply(
  X = pp_list,
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

pva_results_pevo_proj <- bind_rows(pva_pp_list)

#

ps_list <- list.files(
  path = "/data/gpfs/projects/punim0995/rfst/output/spartan_RData/pva/",
  pattern = "pva_stat"
)


pva_ps_list <- lapply(
  X = ps_list,
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

pva_results_pevo_stat <- bind_rows(pva_ps_list)

#

pb_list <- list.files(
  path = "/data/gpfs/projects/punim0995/rfst/output/spartan_RData/pva/",
  pattern = "pva_both"
)


pva_pb_list <- lapply(
  X = pb_list,
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

pva_results_pevo_both <- bind_rows(pva_pb_list)



save(
  pva_results_pevo_proj,
  pva_results_pevo_stat,
  pva_results_pevo_both,
  file = "/data/gpfs/projects/punim0995/rfst/output/RData/11.1_pva_pevo.RData"
)
