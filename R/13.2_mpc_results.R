# 13.2 mpc results summary

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
load(file = "output/RData/13.1_pva_mpc5.RData")
load(file = "output/RData/13.1_pva_mpc5_eg.RData")

source.functions("R/functions")

mpc_eg <- mpc_results_pva5_eg  %>%
  mutate(
    landscape = "East Gippsland",
    yscenario = scenario,
    scenario = sub(
      pattern = "...._",
      replacement = "",
      x = scenario
    )
  )

mpc_eg

mpc_ch <- mpc_results_pva5_ch  %>%
  mutate(
    yscn_id = NA,
    ycscnid = NA,
    yearid = "CH",
    yscenario = NA,
    landscape = "Central Highlands"
  ) %>%
  dplyr::select(names(mpc_eg))

mpc_ch

mpc_results <- bind_rows(
  mpc_ch,
  mpc_eg
)


species_names_pva <- tibble::tribble(
  ~sp,    ~species_com, ~species,
  "gyle",   "Leadbeater's possum", "Gymnobelideus leadbeateri",
  "peau", "Yellow-bellied glider",        "Petaurus australis",
  "pevo",        "Greater glider",        "Petauroides volans",
  "polo",   "Long-footed potoroo",         "Potorous longipes",
  "smle",  "White-footed dunnart",      "Sminthopsis leucopus",
  "tyte",             "Sooty Owl",          "Tyto tenebricosa",
  "vava",          "Lace monitor",            "Varanus varius"
) %>%
  mutate(
    species_com = factor(
      x = species_com,
      levels = c(
        "Leadbeater's possum",
        "Yellow-bellied glider",
        "Greater glider",
        "White-footed dunnart",
        "Long-footed potoroo",
        "Sooty Owl",
        "Lace monitor"
      )
    )
  )


scenario_names <- tribble(
  ~scenario,       ~scn,
  "TH30_rcp85_PB", "Stop harvest 2030 RCP8.5",
  "TH00_rcp85_PB", "No harvest RCP8.5",
  "TH19_rcp45_PB", "Ongoing harvest RCP4.5",
  "TH30_rcp45_PB", "Stop harvest 2030 RCP4.5",
  "TH00_rcp45_PB", "No harvest RCP4.5",
  "TH00_rcp45_NB", "No harvest no planned burning RCP4.5"
) %>%
  mutate(
    scn = factor(
      x = scn,
      levels = c(
        "Stop harvest 2030 RCP8.5",
        "No harvest RCP8.5",
        "Ongoing harvest RCP4.5",
        "Stop harvest 2030 RCP4.5",
        "No harvest RCP4.5",
        "No harvest no planned burning RCP4.5"
      )
    )
  )

scenario_names

mpc_r <- mpc_results %>%
  left_join(
    y = scenario_names,
    by = "scenario"
  ) %>%
  left_join(
    y = species_names_pva,
    by = "sp"
  ) %>%
  mutate(
    logmpc = ifelse(mpc == 0, 0, log(mpc, base = 10))
  )

mpc_r


mpc_res_summary <- mpc_r  %>%
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
    scn,
    species_com,
    landscape,
    #year,
    yearid
  ) %>%
  summarise(
    mpc_min = min(mpc),
    mpc_mean = mean(mpc),
    mpc_median = median(mpc),
    lmpc_min = min(logmpc),
    lmpc_mean = mean(logmpc),
    lmpc_median = median(logmpc)
  )

mpc_res_summary


mpc_rs <- mpc_res_summary %>%
  group_by(
    yearid,
    sp
  ) %>%
  mutate(
    r_min = median(mpc_min[scenario == "TH19_rcp45_PB" & climate_model == "ACCESS1-0"]),
    r_median = median(mpc_median[scenario == "TH19_rcp45_PB" & climate_model == "ACCESS1-0"]),
    lr_min = median(lmpc_min[scenario == "TH19_rcp45_PB" & climate_model == "ACCESS1-0"]),
    lr_median = median(lmpc_median[scenario == "TH19_rcp45_PB" & climate_model == "ACCESS1-0"])
  ) %>%
  ungroup %>%
  mutate(
    delta_min    = mpc_min    - r_min,
    delta_median = mpc_median - r_median,
    dp_min       = delta_min    / r_min,
    dp_median    = delta_median / r_median,
    ldelta_min    = lmpc_min    - lr_min,
    ldelta_median = lmpc_median - lr_median,
    ldp_min       = ldelta_min    / lr_min,
    ldp_median    = ldelta_median / lr_median
  ) 

mpc_rs

mpc_cs <- mpc_res_summary %>%
  group_by(
    yearid,
    climate_model,
    sp
  ) %>%
  mutate(
    r_min = median(mpc_min[scenario == "TH19_rcp45_PB"]),
    r_median = median(mpc_median[scenario == "TH19_rcp45_PB"]),
    lr_min = median(lmpc_min[scenario == "TH19_rcp45_PB"]),
    lr_median = median(lmpc_median[scenario == "TH19_rcp45_PB"])
  ) %>%
  ungroup %>%
  mutate(
    delta_min    = mpc_min    - r_min,
    delta_median = mpc_median - r_median,
    dp_min       = delta_min    / r_min,
    dp_median    = delta_median / r_median,
    ldelta_min    = lmpc_min    - lr_min,
    ldelta_median = lmpc_median - lr_median,
    ldp_min       = ldelta_min    / lr_min,
    ldp_median    = ldelta_median / lr_median
  ) 

mpc_cs


save(
  mpc_r,
  mpc_rs,
  mpc_cs,
  mpc_res_summary,
  file = "/data/gpfs/projects/punim0995/rfst/output/RData/13.2_mpc_results.RData"
)
