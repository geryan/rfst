# 08 Distribution model data


library(raster)
library(dplyr)
library(sf)
library(lubridate)

load(file = "output/RData/00_comp_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")
load(file = "output/RData/02_species_occurrences.RData")
load(file = "output/RData/07_combined_variables.RData")

source.functions("R/functions")

# ----------------------------

md_lb_09 <- get.model.data(pa_lb_09, vars_1_01, na.omit = FALSE) %>%
  mutate(nd = as.numeric(ymd("2019-01-01") - date)/365,
         nd = ifelse(nd > 0, nd, 0)) %>%
  mutate(tsf = tsf - nd,
         tsl = tsl - nd,
         max_age = max_age - nd) %>%
  mutate(max_age = case_when(tsf < 0 ~ NA_real_,
                             tsl < 0 ~ NA_real_,
                             max_age < 0 ~ NA_real_,
                             TRUE ~ max_age)) %>%
  mutate(tsf = ifelse(tsf >= 0, tsf, NA),
         tsl = ifelse(tsl >= 0, tsl, NA))

md_lb_09x <- get.model.data(pa_lb_09x, vars_1_01, na.omit = FALSE) %>%
  mutate(nd = as.numeric(ymd("2019-01-01") - date)/365,
         nd = ifelse(nd > 0, nd, 0)) %>%
  mutate(tsf = tsf - nd,
         tsl = tsl - nd,
         max_age = max_age - nd) %>%
  mutate(max_age = case_when(tsf < 0 ~ NA_real_,
                             tsl < 0 ~ NA_real_,
                             max_age < 0 ~ NA_real_,
                             TRUE ~ max_age)) %>%
  mutate(tsf = ifelse(tsf >= 0, tsf, NA),
         tsl = ifelse(tsl >= 0, tsl, NA))

md_lb_80 <- get.model.data(pa_lb_80, vars_1_01, na.omit = FALSE) %>%
  mutate(nd = as.numeric(ymd("2019-01-01") - date)/365,
         nd = ifelse(nd > 0, nd, 0)) %>%
  mutate(tsf = tsf - nd,
         tsl = tsl - nd,
         max_age = max_age - nd) %>%
  mutate(max_age = case_when(tsf < 0 ~ NA_real_,
                             tsl < 0 ~ NA_real_,
                             max_age < 0 ~ NA_real_,
                             TRUE ~ max_age)) %>%
  mutate(tsf = ifelse(tsf >= 0, tsf, NA),
         tsl = ifelse(tsl >= 0, tsl, NA))

md_lb_80x <- get.model.data(pa_lb_80x, vars_1_01, na.omit = FALSE) %>%
  mutate(nd = as.numeric(ymd("2019-01-01") - date)/365,
         nd = ifelse(nd > 0, nd, 0)) %>%
  mutate(tsf = tsf - nd,
         tsl = tsl - nd,
         max_age = max_age - nd) %>%
  mutate(max_age = case_when(tsf < 0 ~ NA_real_,
                             tsl < 0 ~ NA_real_,
                             max_age < 0 ~ NA_real_,
                             TRUE ~ max_age)) %>%
  mutate(tsf = ifelse(tsf >= 0, tsf, NA),
         tsl = ifelse(tsl >= 0, tsl, NA))

md_gg_09 <- get.model.data(pa_gg_09, vars_1_01, na.omit = FALSE) %>%
  mutate(nd = as.numeric(ymd("2019-01-01") - date)/365,
         nd = ifelse(nd > 0, nd, 0)) %>%
  mutate(tsf = tsf - nd,
         tsl = tsl - nd,
         max_age = max_age - nd) %>%
  mutate(max_age = case_when(tsf < 0 ~ NA_real_,
                             tsl < 0 ~ NA_real_,
                             max_age < 0 ~ NA_real_,
                             TRUE ~ max_age)) %>%
  mutate(tsf = ifelse(tsf >= 0, tsf, NA),
         tsl = ifelse(tsl >= 0, tsl, NA))

md_gg_09x <- get.model.data(pa_gg_09x, vars_1_01, na.omit = FALSE) %>%
  mutate(nd = as.numeric(ymd("2019-01-01") - date)/365,
         nd = ifelse(nd > 0, nd, 0)) %>%
  mutate(tsf = tsf - nd,
         tsl = tsl - nd,
         max_age = max_age - nd) %>%
  mutate(max_age = case_when(tsf < 0 ~ NA_real_,
                             tsl < 0 ~ NA_real_,
                             max_age < 0 ~ NA_real_,
                             TRUE ~ max_age)) %>%
  mutate(tsf = ifelse(tsf >= 0, tsf, NA),
         tsl = ifelse(tsl >= 0, tsl, NA))

md_gg_80 <- get.model.data(pa_gg_80, vars_1_01, na.omit = FALSE) %>%
  mutate(nd = as.numeric(ymd("2019-01-01") - date)/365,
         nd = ifelse(nd > 0, nd, 0)) %>%
  mutate(tsf = tsf - nd,
         tsl = tsl - nd,
         max_age = max_age - nd) %>%
  mutate(max_age = case_when(tsf < 0 ~ NA_real_,
                             tsl < 0 ~ NA_real_,
                             max_age < 0 ~ NA_real_,
                             TRUE ~ max_age)) %>%
  mutate(tsf = ifelse(tsf >= 0, tsf, NA),
         tsl = ifelse(tsl >= 0, tsl, NA))

md_gg_80x <- get.model.data(pa_gg_80x, vars_1_01, na.omit = FALSE) %>%
  mutate(nd = as.numeric(ymd("2019-01-01") - date)/365,
         nd = ifelse(nd > 0, nd, 0)) %>%
  mutate(tsf = tsf - nd,
         tsl = tsl - nd,
         max_age = max_age - nd) %>%
  mutate(max_age = case_when(tsf < 0 ~ NA_real_,
                             tsl < 0 ~ NA_real_,
                             max_age < 0 ~ NA_real_,
                             TRUE ~ max_age)) %>%
  mutate(tsf = ifelse(tsf >= 0, tsf, NA),
         tsl = ifelse(tsl >= 0, tsl, NA))

save(
  md_lb_09,
  md_lb_80,
  md_gg_09,
  md_gg_80,
  md_lb_09x,
  md_lb_80x,
  md_gg_09x,
  md_gg_80x,
  file = "output/RData/08_distribution_model_data.RData"
)