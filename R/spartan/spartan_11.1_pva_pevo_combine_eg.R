# Run PVAs combine

source("R/spartan/spartan_settings.R")

library(raster)
library(dplyr)
library(tibble)
library(tidyr)


load(file = "output/RData/00_controls_eg.RData")
load(file = "output/RData/01_landscape_variables_eg.RData")

source.functions("R/functions")


pp_list <- list.files(
  path = "/data/gpfs/projects/punim1340/rfst_eg/output/spartan_RData/pva/",
  pattern = "pva_proj"
)


pva_pp_list <- lapply(
  X = pp_list,
  FUN = function(x){
    z <- readRDS(
      file = sprintf(
        "/data/gpfs/projects/punim1340/rfst_eg/output/spartan_RData/pva/%s",
        x
      )
    )
    
    return(z)
    
  }
)

pva_results_pevo_proj_eg <- bind_rows(pva_pp_list)

#

ps_list <- list.files(
  path = "/data/gpfs/projects/punim1340/rfst_eg/output/spartan_RData/pva/",
  pattern = "pva_stat"
)


pva_ps_list <- lapply(
  X = ps_list,
  FUN = function(x){
    z <- readRDS(
      file = sprintf(
        "/data/gpfs/projects/punim1340/rfst_eg/output/spartan_RData/pva/%s",
        x
      )
    )
    
    return(z)
    
  }
)

pva_results_pevo_stat_eg <- bind_rows(pva_ps_list)

#

pb_list <- list.files(
  path = "/data/gpfs/projects/punim1340/rfst_eg/output/spartan_RData/pva/",
  pattern = "pva_both"
)


pva_pb_list <- lapply(
  X = pb_list,
  FUN = function(x){
    z <- readRDS(
      file = sprintf(
        "/data/gpfs/projects/punim1340/rfst_eg/output/spartan_RData/pva/%s",
        x
      )
    )
    
    return(z)
    
  }
)

pva_results_pevo_both_eg <- bind_rows(pva_pb_list)



save(
  pva_results_pevo_proj_eg,
  pva_results_pevo_stat_eg,
  pva_results_pevo_both_eg,
  file = "/data/gpfs/projects/punim1340/rfst_eg/output/RData/11.1_pva_pevo.RData"
)
