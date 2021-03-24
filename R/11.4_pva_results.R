
source("R/spartan/spartan_settings.R")


library(magrittr)
library(tibble)
library(dplyr)
library(purrr)
library(tidyr)
library(raster)
library(sp)
library(sf)
library(ggplot2)
library(forcats)
library(patchwork)
library(cowplot)


load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")
load(file = "output/RData/02_species_occurrences.RData")
load(file = "output/RData/11.1_pva.RData")
load(file = "output/RData/11.1_pva_eg.RData")

source.functions("R/functions")

pr_eg <- pva_results_eg %>%
  dplyr::select(
    -dist_map
  ) %>%
  mutate(
    year = ifelse(
      yearid == "EG19",
      "2019",
      "2020"
    ),
    landscape = "East Gippsland",
    yscenario = scenario,
    scenario = sub(
      pattern = "...._",
      replacement = "",
      x = scenario
    )
  )


pr_ch <- pva_results_ch %>%
  dplyr::select(
    -dist_map
  ) %>%
  mutate(
    yscn_id = NA,
    ycscnid = NA,
    yearid = "CH",
    yscenario = NA,
    year = "2019",
    landscape = "Central Highlands",
    init_pop = NA
  ) %>%
  dplyr::select(names(pr_eg))


pva_results <- bind_rows(
  pr_eg,
  pr_ch
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

species_names_pva

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

exminpop <- emp

pvr <- pva_results %>%
  left_join(
    y = species_names_pva,
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
  ) %>%
  mutate(
    emp_med = map(
      .x = emp_all,
      .f = median
    ) %>%
      unlist,
    med_pop = map(
      .x = pva,
      .f = med.pop
    ) %>%
      unlist,
    emp_95 = map(
      .x = pva,
      .f = exminpop,
      q = 0.05
    ) %>%
      unlist,
    prop_extinct = map(
      .x = pva,
      .f = nex,
      p = TRUE
    ) %>%
      unlist,
    prop_extant = 1 - prop_extinct,
    tt_ex = map(
      .x = pva,
      .f = ttex
    ),
    mean_tt_ex = map(
      .x = tt_ex,
      .f = mean,
      na.rm = TRUE
    ) %>%
      unlist
  ) %>%
  group_by(
    #landscape,
    #habitat,
    #year,
    yearid,
    climate_model,
    sp
  ) %>%
  mutate(
    ref_emp = median(emp[scenario == "TH19_rcp45_PB"]),
    ref_emp_95 = median(emp_95[scenario == "TH19_rcp45_PB"]),
    ref_emp_med = median(emp_med[scenario == "TH19_rcp45_PB"]),
    ref_med_pop = median(med_pop[scenario == "TH19_rcp45_PB"]),
    ref_prop_extant = median(prop_extant[scenario == "TH19_rcp45_PB"]),
    ref_mean_tt_ex = median(mean_tt_ex[scenario == "TH19_rcp45_PB"])
  ) %>%
  ungroup %>%
  mutate(
    delta_emp = emp - ref_emp,
    delta_emp_95 = emp_95 - ref_emp_95,
    delta_emp_med = emp_med - ref_emp_med,
    delta_med_pop = med_pop - ref_med_pop,
    delta_prop_extant = prop_extant - ref_prop_extant,
    delta_mean_tt_ex = mean_tt_ex - ref_mean_tt_ex
  )

pvr

save(
  pvr,
  file = "output/RData/11.4_pva_results.R"
)
