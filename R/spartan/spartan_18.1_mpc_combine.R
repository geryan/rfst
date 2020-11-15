# 18.1 combine  metapopulation capacity model results

source("R/spartan/spartan_settings.R")

library(raster)
library(dplyr)
library(tibble)
library(tidyr)


load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")

source.functions("R/functions")


file_list <- list.files("/data/gpfs/projects/punim0995/rfst/output/spartan_RData/mpc/")


mpc_list <- lapply(
  X = file_list,
  FUN = function(x){
    z <- readRDS(
      file = sprintf(
        "/data/gpfs/projects/punim0995/rfst/output/spartan_RData/mpc/%s",
        x
      )
    )
    
    return(z)
    
  }
)

mpc_results_ch <- bind_rows(mpc_list)



save(
  mpc_results_ch,
  file = "/data/gpfs/projects/punim0995/rfst/output/RData/18.1_mpc.RData"
)
