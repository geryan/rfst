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
  pattern = "pva_projb"
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

pva_results_pevo_proj_egb <- bind_rows(pva_pp_list)

#

ps_list <- list.files(
  path = "/data/gpfs/projects/punim1340/rfst_eg/output/spartan_RData/pva/",
  pattern = "pva_statb"
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

pva_results_pevo_stat_egb <- bind_rows(pva_ps_list)

#

pb_list <- list.files(
  path = "/data/gpfs/projects/punim1340/rfst_eg/output/spartan_RData/pva/",
  pattern = "pva_bothb"
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

pva_results_pevo_both_egb <- bind_rows(pva_pb_list)



save(
  pva_results_pevo_proj_egb,
  pva_results_pevo_stat_egb,
  pva_results_pevo_both_egb,
  file = "/data/gpfs/projects/punim0995/rfst/output/RData/11.1_pva_pevo_egb.RData"
)
