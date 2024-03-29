# 06 Climate variables

source("R/spartan/spartan_settings.R")

library(raster)
library(magrittr)
library(rerddap)
library(rlang)
library(dplyr)
library(tidyr)
library(purrr)
#library(future)
#library(future.apply)

load(file = "output/RData/00_controls_eg.RData")
load(file = "output/RData/01_landscape_variables_eg.RData")

source.functions("R/functions")


#plan(multisession, workers = ncores)

#plan(sequential)

# Current climate
# -----------------------------------------

bioclim_vars <- c(
  "tmax",
  "tmin",
  "prec"
)


raw_bioclim <- expand_grid(
  bioclim_vars) %>%
  rowwise %>%
  mutate(
    raw_data = pmap(
      .f = getData,
      .l = list(
        var = bioclim_vars
      ),
      name = "worldclim",
      path = sprintf(
        "%s/wc_30sec",
        out_path_eg
      ),
      res = 0.5,
      lat = -60,
      lon = 120
    )
  )


raw_djf <- raw_bioclim %$%
  lapply(
    X = raw_data,
    FUN = function(x){
      result <- x[[c(12, 1, 2)]]
      return(result)
    }
  ) %>%
  lapply(
    FUN = mean
  ) %>%
  mapply(
    FUN = function(x, var){
      if(var == "prec"){
        return(x)
      } else{
        z <- round(x)/10
        return(z)
      }
    },
    var = raw_bioclim$bioclim_vars
  )


raw_jja <- raw_bioclim %$%
  lapply(
    X = raw_data,
    FUN = function(x){
      result <- x[[c(6, 7, 8)]]
      return(result)
    }
  ) %>%
  lapply(
    FUN = mean
  ) %>%
  mapply(
    FUN = function(x, var){
      if(var == "prec"){
        return(x)
      } else{
        z <- round(x)/10
        return(z)
      }
    },
    var = raw_bioclim$bioclim_vars
  )


raw_season <- bind_cols(
  raw_bioclim,
  tibble(djf = raw_djf),
  tibble(jja = raw_jja)
) %>%
  dplyr::select(-raw_data) %>%
  pivot_longer(
    -bioclim_vars,
    names_to = "season",
    values_to = "raw_seasonal_data"
  ) %>%
  filter(!(bioclim_vars == "tmax" & season == "jja")) %>%
  filter(!(bioclim_vars == "tmin" & season == "djf")) %>%
  mutate(
    layername = sprintf(
      "%s_%s",
      bioclim_vars,
      season
    ),
    filename = sprintf(
      "%s/clim_vars/base_%s_%s_eg.grd",
      out_path_eg,
      bioclim_vars,
      season
    )
  )

base_climate_raster <- raw_season %$%
  #future_mapply(
  mapply(
    FUN = function(
      x,
      proj_mask,
      filename,
      layername
    ){
      result <- x %>%
        projectRaster(
          to = proj_mask
        ) %>%
        rst.op(
          op = "writeonly",
          proj_mask = proj_mask,
          filename = filename,
          layernames = layername
        )
      
      return(result)
    },
    x = raw_seasonal_data,
    layername = layername,
    filename = filename,
    MoreArgs = list(
      proj_mask = eg_mask
    )
  )


base_climate_variables <- tibble(base_climate_raster) %>%
  bind_cols(raw_season) %>%
  dplyr::select(
    climate_variable = bioclim_vars,
    season,
    base_climate_raster
  )




# Future climate
# -------------------------------------------------------------
# Absolute change in temperature
erddap_url <- "http://nrm-erddap.nci.org.au/erddap/"

climate_model <- c(
  "ACCESS1-0",  # in landis growth model as consensus model with those inputs
  "NorESM1-M",  # climate futures tool 'maximum consensus' (high consensus)
  "GFDL-ESM2M", # climate futures tool 'worst case' (very low consensus)
  "CanESM2"     # climate futures tool 'best case' (high consensus)
  
)

climate_variable <- c(
  "pr_djf",     # precipitation december, january, february
  "pr_jja",     # precipitation june, july, august
  "tasmax_djf", # max air temperature at surface december, january, february,
  "tasmin_jja"  # minimum air temperature at surface june, july, august
)

rcp <- c(
  "rcp45", # relative concentration pathway 4.5
  "rcp85"  # relative concentration pathway 8.5
)

## Download data from climate change in australia ERDDAP
raw_climate_projection_data <- expand_grid(
  climate_model,
  rcp,
  climate_variable
) %>%
  mutate(
    clim_var_class = sub(
      pattern = "_.*",
      replacement = "",
      x = climate_variable
    ),
    data_string = paste0(
      clim_var_class,
      "_Amon_",
      climate_model,
      "_",
      rcp,
      "_r1i1p1_",
      ifelse(
        clim_var_class == "pr",
        "perc-change-wrt-seassum-clim_native",
        "abs-change-wrt-seasavg-clim_native"
      )
    ),
    filename = sprintf(
      "%s/clim_vars/raw_%s_%s_%s_eg.grd",
      out_path_eg,
      climate_model,
      rcp,
      climate_variable
    )
  ) %>%
  rowwise %>%
  mutate(
    raw_data = pmap(
      .l = list(
        x = data_string,
        fields = climate_variable
      ),
      .f = griddap,
      url = erddap_url,
      latitude = c(-40.10297775, -32.64199448), # clips download of data to small area around CH target area
      longitude = c(140.625, 150)
    )
  ) %>%
  ungroup
###
# Because the tile sizes for data for the CanESM2 model aren't perfectly square
# some adjustment is necessary to replace the latitudes with evenly spaced latitudes
# shifts middle two latitudes by < 1e-5 degree, so likely to be < 10 m

CanESM2_dat <- raw_climate_projection_data %>%
  filter(climate_model == "CanESM2")

unique.lats <- unique(CanESM2_dat$raw_data[[1]]$data$lat)

adjusted.lats <- seq(
  from = unique.lats[1],
  to = tail(unique.lats, n = 1),
  length.out = length(unique.lats)
)

names(adjusted.lats) <- unique.lats

CanESM2_lats <- CanESM2_dat$raw_data[[1]]$data$lat %>%
  recode(
    !!!adjusted.lats # https://rlang.r-lib.org/reference/nse-force.html
  )

CanESM2_adj_raw_data <- lapply(
    X = CanESM2_dat$raw_data,
    FUN = function(x, y){
      x$data$lat <- y

      return(x)
    },
    y = CanESM2_lats
  )

CanESM2_adj <- CanESM2_dat %>%
  dplyr::select(-raw_data) %>%
  bind_cols(
    tibble(
      raw_data = CanESM2_adj_raw_data
    )
  )

raw_climate_projection_data_adj <- raw_climate_projection_data %>%
  filter(climate_model != "CanESM2") %>%
  bind_rows(CanESM2_adj)


# Reporojected layers
# plan(multisession, workers = ncores)

raw_climate_projection_rasters <- raw_climate_projection_data_adj %$%
#raw_climate_projection_rasters <- raw_climate_projection_data %$%
  mapply(
    FUN = rasterize.climate.projections,
    dat = raw_data,
    filename = filename,
    MoreArgs = list(
      new.proj.layer = eg_mask
    )
  )

# plan(sequential)


raw_climate_projections <- raw_climate_projection_data_adj %>%
#raw_climate_projections <- raw_climate_projection_data %>%
  bind_cols(tibble(raw_climate_projection_rasters)) %>%
  mutate(
    season = sub(
      pattern = ".*_",
      replacement = "",
      x = climate_variable
    )
  ) %>%
  dplyr::select(
    climate_model,
    rcp,
    climate_variable = clim_var_class,
    season,
    raw_climate_projection_rasters
  ) %>%
  mutate(
    climate_variable = recode(
      climate_variable,
      "tasmax" = "tmax",
      "tasmin" = "tmin",
      "pr" = "prec"
    )
  )

####  Absolute predicted values
# --------------------------------------------------------------

# plan(multisession, workers = ncores)

climate_projection_rasters <- raw_climate_projections %$%
  mapply(
    FUN = function(
      base,
      change,
      proj_mask,
      cm,
      rc,
      cv,
      se,
      out_path
    ){
      
      filename <- sprintf(
        "%s/clim_vars/projected_%s_%s_%s_%s_eg.grd",
        out_path,
        cm,
        rc,
        cv,
        se
      )
      
      op <- ifelse(
        cv == "prec",
        "addper",
        "addabs"
      )
      
      base_layer <- base %>%
        filter(climate_variable == cv) %>%
        filter(season == se) %>%
        dplyr::select(base_climate_raster) %>%
        unlist %>%
        magrittr::extract2(1)
      
      result <- rst.op(
        input1 = base_layer,
        input2 = change,
        proj_mask = proj_mask,
        op = op,
        filename = filename
      )
      
      return(result)
    },
    change = raw_climate_projection_rasters,
    cm = climate_model,
    rc = rcp,
    cv = climate_variable,
    se = season,
    MoreArgs = list(
      base = base_climate_variables,
      proj_mask = eg_mask,
      out_path = out_path_eg
    )
  )

# plan(sequential)



initial_raster <- raw_climate_projections %$%
  mapply(
    FUN = function(
      base,
      cv,
      se
    ){
      
      base_layer <- base %>%
        filter(climate_variable == cv) %>%
        filter(season == se) %>%
        dplyr::select(base_climate_raster) %>%
        unlist %>%
        magrittr::extract2(1)
      
      return(base_layer)
    },
    cv = climate_variable,
    se = season,
    MoreArgs = list(
      base = base_climate_variables
    )
  )


climate_projections <- raw_climate_projections %>%
  bind_cols(
    tibble(
      climate_projection_rasters,
      initial_raster
    )
  )

# Interploate prediction data -------------------------------------------


# plan(multisession, workers = ncores)

interpolated_climate_projection_rasters <- climate_projections %$%
  mapply(
    FUN = interpolate.climdat,
    initras = initial_raster,
    futras = climate_projection_rasters,
    climate_model = climate_model,
    rcp = rcp,
    climate_variable = climate_variable,
    season = season,
    MoreArgs = list(
      ntimesteps = ntimesteps,
      year0 = year0,
      proj_mask = eg_mask,
      out_path = sprintf(
        "%s/clim_vars",
        out_path_eg
      )
    )
  )




climproj_eg <- climate_projections %>%
  bind_cols(
    tibble(
      interpolated_climate_projection_rasters
    )
  ) %>%
  mutate(
    clim_var = sprintf(
      "%s_%s",
      climate_variable,
      season
    )
  ) %>%
  dplyr::select(
    climate_model,
    rcp,
    clim_var,
    climate_projection = interpolated_climate_projection_rasters
  ) %>%
  pivot_wider(
    names_from = clim_var,
    values_from = climate_projection
  )


### Climate variable sets for SDM  ------------------------------------------


climset <- climproj_eg %$%
  mapply(
    FUN = stack.climate,
    prec_djf = prec_djf,
    prec_jja = prec_jja,
    tmax_djf = tmax_djf,
    tmin_jja = tmin_jja,
    MoreArgs = list(
      ntimesteps = ntimesteps
    ),
    SIMPLIFY = FALSE
  )


clim_vars_eg <- climproj %>%
  bind_cols(
    tibble(
      climate_projections = climset
    )
  ) %>%
  dplyr::select(
    climate_model,
    rcp,
    climate_projections
  )

cpdat_eg <- clim_vars_eg %>%
  mutate(
    prec_djf = map(
      .x = climate_projections,
      .f = function(x){
        sapply(
          X = x,
          FUN = function(y){
            mean(
              x = getValues(y[[1]]),
              na.rm = TRUE
            )
          }
        )
      }
    ),
    prec_jja = map(
      .x = climate_projections,
      .f = function(x){
        sapply(
          X = x,
          FUN = function(y){
            mean(
              x = getValues(y[[2]]),
              na.rm = TRUE
            )
          }
        )
      }
    ),
    tmax_djf = map(
      .x = climate_projections,
      .f = function(x){
        sapply(
          X = x,
          FUN = function(y){
            mean(
              x = getValues(y[[3]]),
              na.rm = TRUE
            )
          }
        )
      }
    ),
    tmin_jja = map(
      .x = climate_projections,
      .f = function(x){
        sapply(
          X = x,
          FUN = function(y){
            mean(
              x = getValues(y[[4]]),
              na.rm = TRUE
            )
          }
        )
      }
    )
  )

# Save outputs ---------------------

save(
  cpdat_eg,
  clim_vars_eg,
  climproj_eg,
  file = "output/RData/06_climate_variables_eg.RData"
)
