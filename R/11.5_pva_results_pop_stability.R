
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
load(file = "output/RData/11.1_pva_export.RData")
load(file = "output/RData/11.1_pva_eg_export.RData")

source.functions("R/functions")

pr_eg <- pva_results_eg_export %>%
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


pr_ch <- pva_results_ch_export %>%
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

pvr_export <- pva_results %>%
  left_join(
    y = species_names_pva,
    by = "sp"
  ) %>%
  left_join(
    y = scenario_names,
    by = "scenario"
  ) #%>%
  # mutate(
  #   scenario = fct_reorder(
  #     .f = scenario,
  #     .x = as.numeric(scn)
  #   ),
  #   sp = fct_reorder(
  #     .f = sp,
  #     .x = as.numeric(species_com)
  #   ),
  #   species = fct_reorder(
  #     .f = species,
  #     .x = as.numeric(species_com)
  #   )
  # )

pvr_stabilty <- pvr_export %>%
  filter(harvest_scenario == "TH00", rcp == "rcp45", plan_burn == "PB") %>%
  dplyr::select(
    scenario,
    scenario_replicate,
    rcp,
    climate_model,
    yearid,
    harvest_scenario,
    plan_burn,
    yscn_id,
    scn_id,
    cscnid,
    ycscnid,
    landscape,
    yscenario,
    species_com,
    species,
    scn,
    sp,
    rst_pop,
    rst_k
  ) %>%
  filter() %>%
  mutate(
    pop_stability = map(
      .x = rst_pop,
      .f = function(x){
        
        z <- vector("list", length = length(x))
        
        for(i in 1:length(x)){
          z[[i]] <- sum(x[[i]])
        }
        
        y <- mean(stack(z))
        print("!!!")
        return(y)
      }
    )
  )

pop_stab <- pvr_stabilty %>%
  group_by(sp, species_com, landscape) %>%
  summarise(pop_stability = list(pop_stability)) %>%
  ungroup %>%
  mutate(
    pop_stability = pmap(
      .l = list(
        x = pop_stability,
        y = sp,
        z = landscape
      ),
      .f = function(x, y, z){
        
        yearid <- ifelse(
          test = z == "Central Highlands",
          yes = "CH",
          no = "EG20"
        )
        
        x %>%
          stack %>%
          mean %>%
          rst.op(
            filename = sprintf(
              "%s/pop_stability_%s_%s.grd",
              "/data/gpfs/projects/punim0995/rfst/output/pop_stability",
              yearid,
              y
            ),
            layernames = "population_stability"
          )
      }
    )
  )

ls_stab <- pop_stab %>%
  mutate(
    pop_stability = pmap(
      .l = list(
        x = pop_stability,
        y = sp,
        z = landscape
      ),
      .f = function(x, y, z){
        
        if(z == "Central Highlands"){
          if(y == "tyte" | y == "peau"){
            z <- x
          } else{
            z <- raster::aggregate(x, fact = 2)
          }
        } else {
          z <- x
        }
        
        return(z)
      }
    )
  ) %>%
  mutate(
    pop_stability = map(
      .x = pop_stability,
      .f = function(x){
        
        y <- x
        z <- raster::getValues(x)
        
        z <- (z - min(z, na.rm = TRUE))/(max(z, na.rm = TRUE) - min(z, na.rm = TRUE))
        
        y[] <- z
        
        return(y)
        
      }
    )
  ) %>%
  group_by(landscape) %>%
  summarise(pop_stability = list(pop_stability)) %>%
  mutate(
    pop_stability = pmap(
      .l = list(
        x = pop_stability,
        z = landscape
      ),
      .f = function(x, z){
        
        yearid <- ifelse(
          test = z == "Central Highlands",
          yes = "CH",
          no = "EG20"
        )
        
        x %>%
          stack %>%
          mean %>%
          rst.op(
            filename = sprintf(
              "%s/pop_stability_%s.grd",
              "/data/gpfs/projects/punim0995/rfst/output/pop_stability",
              yearid
            ),
            layernames = "population_stability"
          )
      }
    )
  )


ch_stab_zero <- ls_stab$pop_stability[[1]]

ch_s_v <- getValues(ch_stab_zero)

ch_s_v[ch_s_v > 0] <- 1

ch_stab_zero[] <- ch_s_v


ch_stab_zero <- ch_stab %>%
  rst.op(
    filename = sprintf(
      "%s/pop_stability_zero_CH.grd",
      "/data/gpfs/projects/punim0995/rfst/output/pop_stability"
    ),
    layernames = "population_stability_zero"
  )

eg_stab_zero <- ls_stab$pop_stability[[2]]

eg_s_v <- getValues(eg_stab_zero)

eg_s_v[eg_s_v > 0] <- 1

eg_stab_zero[] <- eg_s_v

eg_stab_zero <- eg_stab %>%
  rst.op(
    filename = sprintf(
      "%s/pop_stability_zero_EG20.grd",
      "/data/gpfs/projects/punim0995/rfst/output/pop_stability"
    ),
    layernames = "population_stability_zero"
  )


ls_stab <- tibble(
  ls_stab,
  pop_stability_zero = list(
    ch_stab_zero,
    eg_stab_zero
  )
)


save(
  pop_stab,
  ls_stab,
  file = "output/RData/11.5_pva_results_pop_stability.R"
)
