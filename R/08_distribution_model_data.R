# 08 Distribution model data


source("R/spartan/spartan_settings.R")

library(raster)
library(dplyr)
library(sf)
library(lubridate)
library(tibble)
library(magrittr)
library(tidyr)

load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")
load(file = "output/RData/02_species_occurrences.RData")
load(file = "output/RData/07_combined_variables.RData")

source.functions("R/functions")

# ----------------------------

md_set <- bind_cols(
  var_set[1,],
  pa_data
)


md <- md_set %$%
  mapply(
    FUN = get.model.data,
    x = pa_dat,
    y = all_vars,
    na.omit = FALSE,
    SIMPLIFY = FALSE
  )


distribution_model_data <- pa_data %>%
  bind_cols(
    tibble(
      dist_mod_dat = md
    )
  )

save(
  distribution_model_data,
  file = "output/RData/08_distribution_model_data.RData"
)