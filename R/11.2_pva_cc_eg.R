
source("R/spartan/spartan_settings.R")


library(dplyr)
library(purrr)
library(raster)

load(file = "output/RData/00_controls_eg.RData")
load(file = "output/RData/01_landscape_variables_eg.RData")
load(file = "output/RData/10_predict_SDMs_agg_eg.RData")
load(file = "output/RData/11.0_pva_species_dat_eg.RData")


source.functions("R/functions")


aggcc_eg <- agg_set_eg %>% 
  left_join(
    y = species_dat_pva_eg,
    by = "sp"
  ) %>%
  mutate(
    lcc = pmap(
      .l = list(
        cc = cc,
        aggmaps = aggmaps
      ),
      .f = function(cc, aggmaps){
        q <- as.list(aggmaps)
        
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
    yscn_id,
    scn_id,
    cscnid,
    ycscnid,
    sp,
    lcc
  )

aggcc_eg


save(
  aggcc_eg,
  file = "/data/gpfs/projects/punim0995/rfst/output/RData/11.2_pva_cc_eg.RData"
)

