# 08 Distribution model data


library(raster)
library(dplyr)

load(file = "output/RData/00_comp_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")
load(file = "output/RData/02_species_occurrences.RData")
load(file = "output/RData/07_combined_variables.RData")

source.functions("R/functions")

# ----------------------------

md_lb_09 <- get.model.data(pa_lb_09, vars_1_01, na.omit = FALSE) %>%
  mutate(nd = as.numeric(ymd("2019-01-01") - date)/365) %>%
  mutate(tsf = tsf - nd,
         max_age = max_age - nd) %>%
  mutate(tsf = ifelse(tsf >= 0, tsf, NA),
         max_age = case_when())

md_lb_80 <- get.model.data(pa_lb_80, vars_1_01, na.omit = FALSE) %>%
  mutate(nd = as.numeric(ymd("2019-01-01") - date)/365) %>%
  mutate(tsf = tsf - nd,
         max_age = max_age - nd) %>%
  mutate(tsf = ifelse(tsf >= 0, tsf, NA),
         max_age = case_when())

md_gg_09 <- get.model.data(pa_gg_09, vars_1_01, na.omit = FALSE) %>%
  mutate(nd = as.numeric(ymd("2019-01-01") - date)/365) %>%
  mutate(tsf = tsf - nd,
         max_age = max_age - nd) %>%
  mutate(tsf = ifelse(tsf >= 0, tsf, NA),
         max_age = case_when())

md_gg_80 <- get.model.data(pa_gg_80, vars_1_01, na.omit = FALSE) %>%
  mutate(nd = as.numeric(ymd("2019-01-01") - date)/365) %>%
  mutate(tsf = tsf - nd,
         max_age = max_age - nd) %>%
  mutate(tsf = ifelse(tsf >= 0, tsf, NA),
         max_age = case_when())