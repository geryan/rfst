## 02.1a combine Species occurrence data for pva species in east gippsland

source("R/spartan/spartan_settings.R")

library(magrittr)
library(raster)
library(sf)
library(tidyr)
library(dplyr)

load(file = "output/RData/00_controls_eg.RData")
load(file = "output/RData/01_landscape_variables_eg.RData")
load(file = "output/RData/02.0_species_occurrences_list_eg.RData")

source.functions("R/functions")



file_list <- list.files(
  path = sprintf(
    "%s/%s",
    out_path_eg,
    "/spartan_RData/pa/"
  )
)

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
        "%s/%s/%s",
        out_path_eg,
        "/spartan_RData/pa/",
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

save(
  pa_data_eg,
  file = "/data/gpfs/projects/punim0995/rfst/output/RData/02.1_species_occurrence_eg.RData"
)
