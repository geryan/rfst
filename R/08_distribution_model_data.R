# 08 Distribution model data


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

md_set <- expand_grid(
  varset[1,],
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

# ----------------------------

# md_lb_09b <- get.model.data(pa_lb_09b, vars_1_01, na.omit = FALSE) %>%
#   mutate(nd = as.numeric(ymd("2019-01-01") - date)/365,
#          nd = ifelse(nd > 0, nd, 0)) %>%
#   mutate(tsf = tsf - nd,
#          tsl = tsl - nd,
#          max_age = max_age - nd) %>%
#   mutate(max_age = case_when(tsf < 0 ~ NA_real_,
#                              tsl < 0 ~ NA_real_,
#                              max_age < 0 ~ NA_real_,
#                              TRUE ~ max_age)) %>%
#   mutate(tsf = ifelse(tsf >= 0, tsf, NA),
#          tsl = ifelse(tsl >= 0, tsl, NA))

save(
  distribution_model_data,
  file = "output/RData/08_distribution_model_data.RData"
)