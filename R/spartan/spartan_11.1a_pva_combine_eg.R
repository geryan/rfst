# Run PVAs combine

source("R/spartan/spartan_settings.R")

library(raster)
library(dplyr)
library(tibble)
library(tidyr)


load(file = "output/RData/00_controls_eg.RData")
load(file = "output/RData/01_landscape_variables_eg.RData")

source.functions("R/functions")


file_list <- list.files(
  path = "/data/gpfs/projects/punim1340/rfst_eg/output/spartan_RData/pva/",
  pattern = "pva5_"
)


pva_list <- lapply(
  X = file_list,
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

pva_results5_eg <- bind_rows(pva_list)



#

load(file = "/data/gpfs/projects/punim0995/rfst/output/RData/11.1_pva_pevo_egb.RData")


pva_results5_eg <- bind_rows(
  pva_results5_eg %>%
    dplyr::select(
      -tm,
      -max_cells,
      -dp,
      -pp,
      -disp,
      -z,
      -ss,
      -stages
    ) %>%
    dplyr::select(
      everything(),
      lcc,
      init_pop
    ),
  pva_results_pevo_proj_egb %>%
    dplyr::select(-habitat) %>%
    mutate(
      lcc = NA,
      init_pop = NA
    )
)


#

save(
  pva_results5_eg,
  file = "/data/gpfs/projects/punim0995/rfst/output/RData/11.1_pva_eg.RData"
)
