# 07 combine variables

library(dplyr)
library(raster)
library(tibble)
library(magrittr)
library(tidyr)

load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")
load(file = "output/RData/03_LANDIS_variables.RData")
load(file = "output/RData/04_disturbance_variables.RData")
load(file = "output/RData/05_geophys_vars.RData")
load(file = "output/RData/06_climate_variables.RData")

source.functions("R/functions")


varset <- expand_grid(
  disturbance_variables,
  clim_vars %>% rename(rcp1 = rcp),
  tibble(
    geo_vars = list(gv)
  )
) %>%
  filter(rcp == rcp1) %>%
  dplyr::select(
    scenario,
    scenario_replicate,
    rcp,
    climate_model,
    everything(),
    -rcp1
  )

all_vars <- varset %$%
  mapply(
    FUN = function(
      lv,
      dv,
      cp,
      gv
    ){
      
      result <- mapply(
        FUN = function(
          lv,
          dv,
          cp,
          gv
        ){
          
          stack(
            lv,
            dv,
            cp,
            gv
          )
          
        },
        lv = lv,
        dv = dv,
        cp = cp,
        gv = gv,
        SIMPLIFY = FALSE
      )
      
      return(result)
      
    },
    lv = landis_vars,
    dv = dist_vars,
    cp = climate_projections,
    gv = geo_vars,
    SIMPLIFY = FALSE
  )


varset %<>%
  bind_cols(
    tibble(
      all_vars = all_vars
    )
  )


save(
  varset,
  file = "output/RData/07_combined_variables.RData"
)
