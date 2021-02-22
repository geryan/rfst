# 16 combine fitted distribution models for metapopulation capacity models

source("R/spartan/spartan_settings.R")

library(raster)
library(dplyr)
library(sf)
library(lubridate)
library(purrr)
library(tibble)
library(tidyr)
library(dismo)
library(gbm)


load(file = "output/RData/00_controls_eg.RData")
load(file = "output/RData/01_landscape_variables_eg.RData")

source.functions("R/functions")


file_list <- list.files("/data/gpfs/projects/punim0995/rfst/output/spartan_RData/sdm_fit_mpc/eg/")


fit_list <- lapply(
  X = file_list,
  FUN = function(x){
    z <- readRDS(
      file = sprintf(
        "/data/gpfs/projects/punim0995/rfst/output/spartan_RData/sdm_fit_mpc/eg/%s",
        x
      )
    )
    
    return(z)
    
  }
)

sdm_results_mpc_eg <- bind_rows(fit_list)



save(
  sdm_results_mpc_eg,
  file = "/data/gpfs/projects/punim0995/rfst/output/RData/16_mpc_fit_sdm_eg.RData"
)