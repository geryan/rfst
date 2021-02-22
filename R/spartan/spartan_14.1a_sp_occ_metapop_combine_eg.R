## 14.1a combine Species occurrence data for metapopulation capacity models

source("R/spartan/spartan_settings.R")

library(magrittr)
library(raster)
library(sf)
library(tidyr)
library(dplyr)
library(purrr)

load(file = "output/RData/00_controls_eg.RData")
load(file = "output/RData/01_landscape_variables_eg.RData")
load(file = "output/RData/14.0_sp_occ_metapop_list_eg.RData")

source.functions("R/functions")



file_list <- list.files("/data/gpfs/projects/punim0995/rfst/output/spartan_RData/pa_metapop/eg/")

sp <- sub(
  pattern = "pa_eg_",
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
        "/data/gpfs/projects/punim0995/rfst/output/spartan_RData/pa_metapop/eg/%s",
        x
      )
    )
    
    return(z)
    
  }
)

pad_eg <- tibble(
  sp,
  pa_dat
)

pa_data_eg <- full_join(
  x = pa_list_eg,
  y = pad_eg,
  by = "sp"
)

#pa_data_eg


# Number of presences and absences for each species
pa_tables <- lapply(
  X = pa_data_eg$pa_dat,
  FUN = function(x){
    table(x$PA)
  }
)


pa_data_eg <- pa_data_eg %>%
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

#pa_data_eg

pa_data_eg_model <- pa_data_eg %>%
  filter(presences >= 14)

save(
  pa_data_eg,
  pa_data_eg_model,
  file = "/data/gpfs/projects/punim0995/rfst/output/RData/14.1_sp_occ_metapop_eg.RData"
)
