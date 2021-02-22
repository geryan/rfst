# 07 combine variables

source("R/spartan/spartan_settings.R")

library(dplyr)
library(raster)
library(tibble)
library(magrittr)
library(tidyr)

load(file = "output/RData/00_controls_eg.RData")
load(file = "output/RData/01_landscape_variables_eg.RData")
load(file = "output/RData/03_LANDIS_variables_eg.RData")
load(file = "output/RData/04_disturbance_variables_eg.RData")
load(file = "output/RData/05_geophys_vars_eg.RData")
load(file = "output/RData/06_climate_variables_eg.RData")

source.functions("R/functions")


ll_lon <- init(
  x = eg_mask,
  fun = "x"
)

names(ll_lon) <- "lon"

writeRaster(
  ll_lon,
  filename = sprintf(
    "%s/landscape_vars/eg_lon.grd",
    out_path_eg
  ),
  overwrite = TRUE
)

ll_lon <- raster(
  x = sprintf(
    "%s/landscape_vars/eg_lon.grd",
    out_path_eg
  )
)


ll_lat <- init(
  x = eg_mask,
  fun = "y"
)

names(ll_lat) <- "lat"

writeRaster(
  ll_lat,
  filename = sprintf(
    "%s/landscape_vars/eg_lat.grd",
    out_path_eg
  ),
  overwrite = TRUE
)

ll_lat <- raster(
  x = sprintf(
    "%s/landscape_vars/eg_lat.grd",
    out_path_eg
  )
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
## This raster-derived lon and lat is unnecessary as we can get exact lon and lat from each point directly.

varset <- expand_grid(
  disturbance_variables_eg,
  clim_vars_eg %>% rename(rcp1 = rcp),
  tibble(
    geo_vars = list(gv_eg),
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


var_set_eg <- varset %>%
  bind_cols(
    tibble(
      all_vars = all_vars
    )
  )

var_set_eg$cscnid <- sprintf(
  "%s_%s",
  var_set_eg$scn_id,
  var_set_eg$climate_model
)

var_set_eg$ycscnid <- sprintf(
  "%s_%s",
  var_set_eg$yscn_id,
  var_set_eg$climate_model
)

save(
  var_set_eg,
  file = "output/RData/07_combined_variables_eg.RData"
)


varset_mpc_eg <- var_set_eg %>%
  filter(climate_model == "ACCESS1-0")

save(
  varset_mpc_eg,
  file = "output/RData/07a_varset_mpc_eg.RData"
)