# 00 project controls

library(dismo)
library(doMC)
library(dplyr)
library(foreach)
library(future)
library(future.apply)
library(gbm)
library(ggplot2)
library(lubridate)
library(lwgeom)
library(magrittr)
library(metacapa)
library(purrr)
library(raster)
library(rasterVis)
library(readr)
library(readxl)
library(rerddap)
library(rgdal)
library(rlang)
library(sf)
library(steps)
library(tibble)
library(tidyr)
library(viridis)


source(file = "R/functions/source.functions.R")
source.functions("R/functions")


proj_path <- "/home/unimelb.edu.au/ryange/rfst"
# proj_path <- "D:/Users/ryan/Dropbox/Work/RFA_STEPS/rfst/"

year0 <- 2019

ntimesteps <- 50

ncores <- 20

nreplicates <- 100


scn_list <- c("1", "4", "8")
rep_list <- sprintf("%02d", 1:10)


scn_table <- expand_grid(
  scenario = scn_list,
  scenario_replicate = rep_list
) %>%
  mutate(
    scn_id = sprintf(
      "%s_%s",
      scenario,
      scenario_replicate
    )
  ) %>%
  mutate(
    rcp = "rcp45"
  ) %>% # EDIT  ###############################   EDIT    #################
  filter(scenario_replicate == "01") # EDIT ###   EDIT    #################



# harvest_scenario <- c("TH00", "TH19", "TH30")
# 
# rcp <- c("rcp45", "rcp85")
# 
# pb <- c("PB", "NB")

# scn_table <- expand_grid(
#   harvest_scenario,
#   rcp,
#   pb,
#   scenario_replicate = rep_list
# ) %>%
#   mutate(
#     scenario = sprintf(
#       "%s_%s_%s",
#       harvest_scenario,
#       rcp,
#       pb
#     ),
#     scn_id = sprintf(
#       "%s_%s_%s_%s",
#       harvest_scenario,
#       rcp,
#       pb,
#       scenario_replicate
#     )
#   ) %>%
#   filter(
#     pb == "PB" |
#       (harvest_scenario == "TH00" & rcp == "rcp45")
#   )



species_table <- tibble(
  species = c(
    "Gymnobelideus leadbeateri",
    "Petauroides volans",
    "Petaurus australis",
    #"Potorous longipes",
    "Sminthopsis leucopus",
    "Tyto tenebricosa",
    "Varanus varius"
  )
) %>%
  mutate(
    gen = sub(
      pattern = " .*",
      replacement = "",
      x = species
    ) %>%
      tolower %>%
      substr(
        start = 1,
        stop = 2
      ),
    spe = sub(
      pattern = ".* ",
      replacement = "",
      x = species
    ) %>%
      substr(
        start = 1,
        stop = 2
      ),
    sp = paste0(
      gen,
      spe
    )
  ) %>%
  dplyr::select(
    species,
    sp
  ) %>% 
  mutate(
    pva = TRUE,
    mpc = TRUE
  )


save(
  proj_path,
  year0,
  ntimesteps,
  ncores,
  nreplicates,
  scn_list,
  rep_list,
  source.functions,
  scn_table,
  species_table,
  file = "output/RData/00_controls.RData"
)
