#20 comparison of pva mpc and cc trajectory

source("R/spartan/spartan_settings.R")

library(magrittr)
library(purrr)
library(tibble)
library(tidyr)
library(dplyr)
library(ggplot2)
library(raster)
library(forcats)

load(file = "output/RData/00_controls.RData")
load(file = "output/RData/11.4_pva_results.R")
load(file = "output/RData/13.2_mpc_results.RData")

source.functions("R/functions")

pvlong <- pvr %>%
  dplyr::select(
    "scenario",
    "scenario_replicate",
    "rcp",
    "climate_model",
    "yearid",
    "harvest_scenario",
    "plan_burn",
    "yscn_id",
    "scn_id",
    "cscnid",
    "ycscnid",
    "sp",
    "species_com",
    "species",
    "scn",
    "pva",
    "lcc",
    "popsize"
  ) %>%
  mutate(
    pop = map(
      .x = pva,
      .f = function(x,y){
        apply(
          X = x,
          MARGIN = c(1,3),
          FUN = sum
        ) %>%
          apply(
            MARGIN = 1,
            FUN = median
          )
        
        
      }
    )
  ) %>%
  mutate(yr = list(1:50)) %>%
  dplyr::select(-pva, -popsize) %>%
  unnest(cols = c(pop, lcc, yr)) %>%
  mutate(
    id = case_when(
      is.na(ycscnid) ~ paste0(sp, "_", yearid, "_", cscnid),
      TRUE ~ paste0(sp, "_", ycscnid)
    )
  ) %>%
  group_by(id) %>%
  mutate(
    scaled_pop = (pop - min(pop))/(max(pop) - min(pop)),
    scaled_cc = (lcc - min(lcc))/(max(lcc) - min(lcc))
  ) %>%
  ungroup

pvlong



mplong <- mpc_r %>%
  dplyr::select(
    "scenario",
    "scenario_replicate",
    "rcp",
    "climate_model",
    "yearid",
    "harvest_scenario",
    "plan_burn",
    "yscn_id",
    "scn_id",
    "cscnid",
    "ycscnid",
    "year",
    "sp",
    "species_com",
    "species",
    "scn",
    "mpc"
  ) %>%
  mutate(
    id = case_when(
      is.na(ycscnid) ~ paste0(sp, "_", yearid, "_", cscnid),
      TRUE ~ paste0(sp, "_", ycscnid)
    ),
    yr = ifelse(yearid == "EG19", year -2019, year-2020)
  ) %>%
  filter(yr != 0) %>%
  dplyr::select(-year) %>%
  group_by(id) %>%
  mutate(
    scaled_mpc = (mpc - min(mpc))/(max(mpc) - min(mpc))
  ) %>%
  ungroup

mplong



full_res <- left_join(
  x = pvlong,
  y = mplong
) %>%
  mutate(
    id = as.factor(id),
    scenario = as.factor(scenario)
  )

full_res


full_res_plot <- full_res %>%
  dplyr::select(-pop, -lcc, -mpc) %>%
  pivot_longer(
    cols = c(scaled_pop, scaled_cc, scaled_mpc),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = sub(
      pattern = "scaled_",
      replacement = "",
      x = metric
    )
  ) %>%
  group_by(id, metric) %>%
  mutate(
    r_value = value[yr == min(yr)],
    value0 = value - r_value
  ) %>% 
  ungroup %>%
  mutate(
    idm = paste0(id, "_", metric),
    metric = case_when(
      metric == "cc" ~ "Carrying capacity",
      metric == "pop" ~ "Population size",
      metric == "mpc" ~ "Metapopulation capacity"
    )
  )

full_res_plot



full_res_plot_median <- full_res_plot %>%
  group_by(sp, species_com, species, scenario, scn, climate_model, rcp, yearid, yr, metric) %>%
  summarise(
    value = median(value),
    value0 = median(value0)
  )

full_res_plot_median


save(
  full_res,
  full_res_plot,
  full_res_plot_median,
  file = "/data/gpfs/projects/punim0995/rfst/output/RData/20_pva_mpc_cc_year1_start.RData"
)

