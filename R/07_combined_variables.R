# 07 combine variables

source("R/spartan/spartan_settings.R")

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


ll_lon <- init(
  x = ch_mask,
  fun = "x"
)

names(ll_lon) <- "lon"

writeRaster(
  ll_lon,
  filename = "output/landscape_vars/ch_lon.grd",
  overwrite = TRUE
)

ll_lon <- raster(
  x = "output/landscape_vars/ch_lon.grd"
)


ll_lat <- init(
  x = ch_mask,
  fun = "x"
)

names(ll_lat) <- "lat"

writeRaster(
  ll_lat,
  filename = "output/landscape_vars/ch_lat.grd",
  overwrite = TRUE
)

ll_lat <- raster(
  x = "output/landscape_vars/ch_lat.grd"
)


ll <- vector(
  mode = "list",
  length = (ntimesteps + 1)
) %>%
  lapply(
    FUN = function(
      x,
      lon,
      lat
    ){
      
      stack(lon, lat)
      
    },
    lon = ll_lon,
    lat = ll_lat
  )


varset <- expand_grid(
  disturbance_variables,
  clim_vars %>% rename(rcp1 = rcp),
  tibble(
    geo_vars = list(gv),
    ll = list(ll)
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
      gv,
      ll
    ){
      
      result <- mapply(
        FUN = function(
          lv,
          dv,
          cp,
          gv,
          ll
        ){
          
          stack(
            lv,
            dv,
            cp,
            gv,
            ll
          )
          
        },
        lv = lv,
        dv = dv,
        cp = cp,
        gv = gv,
        ll = ll,
        SIMPLIFY = FALSE
      )
      
      return(result)
      
    },
    lv = landis_vars,
    dv = dist_vars,
    cp = climate_projections,
    gv = geo_vars,
    ll = ll,
    SIMPLIFY = FALSE
  )


var_set <- varset %>%
  bind_cols(
    tibble(
      all_vars = all_vars
    )
  )


save(
  var_set,
  file = "output/RData/07_combined_variables.RData"
)
