## 19 mpc summary results


source("R/spartan/spartan_settings.R")




library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(raster)
library(sp)
library(sf)
library(magrittr)
library(ggplot2)
library(forcats)



load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")
load(file = "output/RData/14.0_sp_occ_metapop_list.RData")
load(file = "output/RData/18.1_mpc.RData")


load(file = "output/RData/02_species_occurrences.RData")
load(file = "output/RData/13.1_pva_mpc5.RData")



source.functions("R/functions")



species_names_pva <- pa_data %>%
  dplyr::select(species, sp) %>%
  mutate(
    species_com = case_when(
      sp == "gyle" ~ "Leadbeater's possum",
      sp == "pevo" ~ "Greater glider",
      sp == "peau" ~ "Yellow-bellied glider",
      sp == "smle" ~ "White-footed dunnart",
      sp == "tyte" ~ "Sooty Owl",
      sp == "vava" ~ "Lace monitor"
    )
  ) %>%
  mutate(
    species_com = factor(
      x = species_com,
      levels = c(
        "Leadbeater's possum",
        "Greater glider",
        "Yellow-bellied glider",
        "White-footed dunnart",
        "Sooty Owl",
        "Lace monitor"
      )
    )
  ) %>%
  mutate(sp_group = "PVA") %>%
  filter(!is.na(species_com))

species_names_pva




species_names_mpc <- pa_list_ch %>%
  dplyr::select(-survey_methods) %>%
  mutate(
    species_com = case_when(
      sp == "psse" ~ "Southern toadlet",
      sp == "acno" ~ "Grey Goshawk",
      sp == "cipu" ~ "Spotted Quail-thrush",
      sp == "lois" ~ "Square-tailed Kite",
      sp == "nico" ~ "Barking Owl",
      sp == "nist" ~ "Powerful Owl",
      sp == "pero" ~ "Rose Robin",
      sp == "tyno" ~ "Masked Owl",
      sp == "isob" ~ "Southern brown bandicoot",
      sp == "psfu" ~ "Smoky mouse",
      sp == "grba" ~ "Gully Grevillea",
      sp == "lese" ~ "Toothed Leionema",
      sp == "pear" ~ "Tree Gebung",
      sp == "wiva" ~ "Baw Baw Berry"
    )
  ) %>%
  mutate(
    species_com = factor(
      x = species_com,
      levels = c(
        "Southern toadlet",
        "Spotted Quail-thrush",
        "Rose Robin",
        "Grey Goshawk",
        "Square-tailed Kite",
        "Barking Owl",
        "Powerful Owl",
        "Masked Owl",
        "Southern brown bandicoot",
        "Smoky mouse",
        "Gully Grevillea",
        "Toothed Leionema",
        "Tree Gebung",
        "Baw Baw Berry"
      )
    )
  ) %>%
  mutate(sp_group = "MPC") %>%
  filter(!is.na(species_com))

species_names_mpc



spnames <- bind_rows(
  species_names_pva,
  species_names_mpc
)

spnames





scenario_names <- tribble(
  ~scenario,       ~scn,
  "TH19_rcp45_PB", "Ongoing harvest RCP4.5",
  "TH30_rcp45_PB", "Stop harvest 2030 RCP4.5",
  "TH30_rcp85_PB", "Stop harvest 2030 RCP8.5",
  "TH00_rcp45_PB", "No harvest RCP4.5",
  "TH00_rcp85_PB", "No harvest RCP8.5",
  "TH00_rcp45_NB", "No harvest no planned burning RCP4.5"
) %>%
  mutate(
    scn = factor(
      x = scn,
      levels = c(
        "Ongoing harvest RCP4.5",
        "Stop harvest 2030 RCP4.5",
        "Stop harvest 2030 RCP8.5",
        "No harvest RCP4.5",
        "No harvest RCP8.5",
        "No harvest no planned burning RCP4.5"
      )
    )
  )

scenario_names
levels(scenario_names$scn)



mpc_results_all_ch <- bind_rows(
  mpc_results_pva5_ch %>%
    dplyr::select(
      -aggmap5
    ),
  mpc_results_ch %>%
    dplyr::select(
      -th,
      -rc,
      -pb,
      -scn_no
    )
)
mpc_results_all_ch




mpc_r_ch <- mpc_results_all_ch %>%
  left_join(
    y = spnames,
    by = "sp"
  ) %>%
  left_join(
    y = scenario_names,
    by = "scenario"
  ) %>%
  mutate(
    scenario = fct_reorder(
      .f = scenario,
      .x = as.numeric(scn)
    ),
    sp = fct_reorder(
      .f = sp,
      .x = as.numeric(species_com)
    ),
    species = fct_reorder(
      .f = species,
      .x = as.numeric(species_com)
    )
  )

mpc_r_ch




mpc_res_summary <- mpc_r_ch  %>%
  group_by(
    scenario,
    scenario_replicate,
    rcp,
    climate_model,
    harvest_scenario,
    plan_burn,
    scn_id,
    cscnid,
    sp,
    species,
    species_com,
    scn,
    sp_group
  ) %>%
  summarise(
    mpc_min = min(mpc),
    mpc_mean = mean(mpc),
    mpc_median = median(mpc),
    mpc_min_07 = min(mpc_07),
    mpc_mean_07 = mean(mpc_07),
    mpc_median_07 = median(mpc_07),
    mpc_min_05 = min(mpc_05),
    mpc_mean_05 = mean(mpc_05),
    mpc_median_05 = median(mpc_05),
  )

mpc_res_summary



mpc_rs <- mpc_res_summary %>%
  group_by(sp) %>%
  mutate(
    r_min = median(mpc_min[scenario == "TH19_rcp45_PB"]),
    r_median = median(mpc_median[scenario == "TH19_rcp45_PB"]),
    r_min_07 = median(mpc_min_07[scenario == "TH19_rcp45_PB"]),
    r_median_07 = median(mpc_median_07[scenario == "TH19_rcp45_PB"]),
    r_min_05 = median(mpc_min_05[scenario == "TH19_rcp45_PB"]),
    r_median_05 = median(mpc_median_05[scenario == "TH19_rcp45_PB"])
  ) %>%
  ungroup %>%
  mutate(
    delta_min    = mpc_min    - r_min,
    delta_median = mpc_median - r_median,
    delta_min_07    = mpc_min_07    - r_min_07,
    delta_median_07 = mpc_median_07 - r_median_07,
    delta_min_05    = mpc_min_05    - r_min_05,
    delta_median_05 = mpc_median_05 - r_median_05,
    dp_min       = delta_min    / r_min,
    dp_median    = delta_median / r_median,
    dp_min_05       = delta_min_05    / r_min_05,
    dp_median_05    = delta_median_05 / r_median_05,
    dp_min_07       = delta_min_07    / r_min_07,
    dp_median_07    = delta_median_07 / r_median_07
  ) 

mpc_rs


save(
  mpc_r_ch,
  mpc_rs,
  file = "output/RData/19_mpc_results_summary.RData"
)
