
source("R/spartan/spartan_settings.R")


library(dplyr)
library(purrr)
library(raster)

load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")
load(file = "output/RData/10.1_aggregate_sdm.RData")
load(file = "output/RData/11.0_pva_species_dat.RData")


source.functions("R/functions")


aggcc_ch <- agg5_ch %>% 
  full_join(
    y = species_dat_pva,
    by = "sp"
  ) %>%
  mutate(
    lcc = pmap(
      .l = list(
        cc = cc,
        aggmap5 = aggmap5
      ),
      .f = function(cc, aggmap5){
        q <- as.list(aggmap5)
        
        lcc <- sapply(
          X = q,
          FUN = function(x, cc){
            y <- x %>%
              getValues
            
            z <- round(cc * 1/(1 + exp(-10*(y - 0.5))))
            
            result <- sum(z, na.rm = TRUE)
            
            return(result)
          },
          cc
        )
      }
    )
  ) %>%
  dplyr::select(
    scenario,
    scenario_replicate,
    rcp,
    climate_model,
    harvest_scenario,
    plan_burn,
    scn_id,
    cscnid,
    sp,
    lcc
  )

aggcc_ch


save(
  aggcc_ch,
  file = "/data/gpfs/projects/punim0995/rfst/output/RData/11.2_pva_cc.RData"
)
