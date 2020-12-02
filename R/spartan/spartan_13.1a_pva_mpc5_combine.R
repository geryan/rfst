# 18.1 combine  metapopulation capacity model results

source("R/spartan/spartan_settings.R")

library(raster)
library(dplyr)
library(tibble)
library(tidyr)


load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")

source.functions("R/functions")


file_list <- list.files(
  path = "/data/gpfs/projects/punim0995/rfst/output/spartan_RData/mpc_pva/",
  pattern = "mpc_pva5"
)


mpc_pva_list <- lapply(
  X = file_list,
  FUN = function(x){
    z <- readRDS(
      file = sprintf(
        "/data/gpfs/projects/punim0995/rfst/output/spartan_RData/mpc_pva/%s",
        x
      )
    )
    
    return(z)
    
  }
)

mpc_results_pva5_ch <- bind_rows(mpc_pva_list)



save(
  mpc_results_pva5_ch,
  file = "/data/gpfs/projects/punim0995/rfst/output/RData/13.1_pva_mpc5.RData"
)


# mpc_results_all_ch <- bind_rows(
#   mpc_results_pva_ch %>%
#     select(-aggmaps),
#   mpc_results_ch %>%
#     select(
#       colnames(mpc_results_ch)[
#         which(
#           sapply(
#             X = colnames(mpc_results_ch),
#             FUN = function(x, y){
#               any(x == y)
#             },
#             y = colnames(mpc_results_pva_ch)
#           )
#         )
#       ]
#     )
# )
# 
# save(
#   mpc_results_all_ch,
#   file = "/data/gpfs/projects/punim0995/rfst/output/RData/13.1a_mpc_all.RData"
# )