# Run PVAs combine

source("R/spartan/spartan_settings.R")

library(raster)
library(dplyr)
library(tibble)
library(tidyr)


load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")

source.functions("R/functions")


file_list <- list.files(
  path = "/data/gpfs/projects/punim0995/rfst/output/spartan_RData/pva/",
  pattern = "pva5_"
)


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

pva_results5_ch <- bind_rows(pva_list)



#
pv5 <- pva_results5_ch %>%
  filter(sp != "pevo")
  
pv5

pv5lcc <- pv5 %>%
  dplyr::select(sp, cscnid, lcc) %>%
  nest(lcc = lcc)

pv5lcc

pva_results5_ch <- pv5 %>%
  dplyr::select(-lcc) %>%
  distinct(sp, cscnid, .keep_all = TRUE) %>%
  left_join(pv5lcc)


load(file = "output/RData/11.1_pva_pevo3.RData")

pva_results5_ch <- bind_rows(
  pva_results5_ch,
  pva_results_pevo_proj %>%
    dplyr::select(-habitat) %>%
    mutate(lcc = NA)
)


#

save(
  pva_results5_ch,
  file = "/data/gpfs/projects/punim0995/rfst/output/RData/11.1_pva.RData"
)
