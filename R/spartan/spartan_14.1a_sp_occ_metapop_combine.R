## 14.1a combine Species occurrence data for metapopulation capacity models

source("R/spartan/spartan_settings.R")

library(magrittr)
library(raster)
library(sf)
library(tidyr)
library(dplyr)

load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")
load(file = "output/RData/14.0_sp_occ_metapop_list.RData")

source.functions("R/functions")



file_list <- list.files("/data/gpfs/projects/punim0995/rfst/output/spartan_RData/pa_metapop/ch/")

sp <- sub(
  pattern = "pa_ch_",
  replacement = "",
  x = file_list
) %>%
  sub(
    pattern = ".Rds",
    replacement = "",
    x = .
  )

pa_dat <- lapply(
  X = file_list,
  FUN = function(x){
    z <- readRDS(
      file = sprintf(
        "/data/gpfs/projects/punim0995/rfst/output/spartan_RData/pa_metapop/ch/%s",
        x
      )
    )
    
    return(z)
    
  }
)

pad_ch <- tibble(
  sp,
  pa_dat
)

pa_data_ch <- full_join(
  x = pa_list_ch,
  y = pad_ch,
  by = "sp"
)

#pa_data_ch


# Number of presences and absences for each species
pa_tables <- lapply(
  X = pa_data_ch$pa_dat,
  FUN = function(x){
    table(x$PA)
  }
)


pa_data_ch <- pa_data_ch %>%
  mutate(
    pa_table = pa_tables,
    absences = map(
      .x = pa_table,
      .f = ~.[[1]]
    ) %>%
      unlist,
    presences = map(
      .x = pa_table,
      .f = ~.[[2]]
    ) %>%
      unlist
  )

#pa_data_ch

pa_data_ch_model <- pa_data_ch %>%
  filter(presences >= 14)

save(
  pa_data_ch,
  pa_data_ch_model,
  file = "/data/gpfs/projects/punim0995/rfst/output/RData/14.1_sp_occ_metapop.RData"
)
