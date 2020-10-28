source("R/spartan/spartan_settings.R")

library(dplyr)
library(tidyr)
library(magrittr)

source("R/functions/make.landis.eg.R")

rep_list <- sprintf("%02d", 1:10)

harvest_scenario <- c("TH00", "TH19", "TH30")

rcp <- c("rcp45", "rcp85")

plan_burn <- c("PB", "NB")

lsc <- c("EG19", "EG20")

scn_table <- expand.grid(
  harvest_scenario = harvest_scenario,
  rcp = rcp,
  plan_burn = plan_burn,
  scenario_replicate = rep_list,
  lsc = lsc
) %>%
  as_tibble %>%
  mutate(
    scenario = sprintf(
      "%s_%s_%s_%s",
      lsc,
      harvest_scenario,
      rcp,
      plan_burn
    ),
    scn_id = sprintf(
      "%s_%s_%s_%s_%s",
      lsc,
      harvest_scenario,
      rcp,
      plan_burn,
      scenario_replicate
    ),
    dir = paste0(
      "/data/scratch/projects/punim1340/landis_raw/east_gippsland/",
      scn_id
    ),
    th = sub(
      pattern = "TH",
      replacement = "",
      x = harvest_scenario
    ),
    rc = sub(
      pattern = "rcp",
      replacement = "",
      x = rcp
    ),
    pb = ifelse(
      plan_burn == "PB",
      TRUE,
      FALSE
    )
  ) %>%
  filter(
    plan_burn == "PB" |
      (harvest_scenario == "TH00" & rcp == "rcp45")
  ) %>%
  filter(
    rcp == "rcp45" |
      harvest_scenario == "TH00" |
      harvest_scenario == "TH30"
  ) %>%
  mutate(
    scn_no = case_when(
      th == "19" ~ 1,
      th == "30" & rc == "45" ~ 2,
      th == "30" & rc == "85" ~ 3,
      th == "00" & rc == "45" & pb ~ 4,
      th == "00" & rc == "85" & pb ~ 5,
      th == "00" & rc == "45" & !pb ~ 6
    )
  ) %>%
  arrange(scn_no) #%>%
  #filter(scenario_replicate == "01")

scn_table %$%
  mapply(
    FUN = make.landis.eg,
    new.dir = dir,
    lsc = lsc,
    th = th,
    pb = pb,
    rcp = rc,
    rep = scenario_replicate,
    MoreArgs = list(
      master.dir = "/data/gpfs/projects/punim1340/landis_master_scenario_eg_4"
    )
  )