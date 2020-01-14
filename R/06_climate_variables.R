# 06 Climate variables

library(raster)
library(magrittr)
library(rerddap)
library(rlang)
library(dplyr)
library(tidyr)
library(purrr)
library(future)
library(future.apply)

load(file = "output/RData/00_comp_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")

source.functions("R/functions")

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
      path = paste0(
        proj_path,
        "/output/wc_30sec/tmax"
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
      "output/clim_vars/base_%s_%s.grd",
      bioclim_vars,
      season
    )
  )

base_climate_raster <- raw_season %$%
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
      proj_mask = ch_mask
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
      "output/clim_vars/%s_%s_%s.grd",
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
plan(multisession, workers = ncores)

raw_climate_projection_rasters <- raw_climate_projection_data_adj %$%
  future_mapply(
    FUN = rasterize.climate.projections,
    dat = raw_data,
    filename = filename,
    MoreArgs = list(
      new.proj.layer = ch_mask
    )
  )

plan(sequential)


raw_climate_projections <- raw_climate_projection_data_adj %>%
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
  )


#Years data available
n_prec01_4.5 <- as.numeric(sub("-.*", "", unique(raw_prec01_4.5_pc$data$time)))
n_prec01_8.5 <- as.numeric(sub("-.*", "", unique(raw_prec01_4.5_pc$data$time)))
n_prec07_4.5 <- as.numeric(sub("-.*", "", unique(raw_prec07_4.5_pc$data$time)))
n_prec07_8.5 <- as.numeric(sub("-.*", "", unique(raw_prec07_4.5_pc$data$time)))


####  Absolute predicted values
# --------------------------------------------------------------

###### Temperature

###### Jan max temperature RCP 4.5
tmax01_4.5 <- rst.op(input1 = tmax01,
                     input2 = tmax01_4.5_ac,
                     proj_mask = ch_mask,
                     op = "addabs",
                     filename = "output/clim_vars/tmax01_4.5",
                     layernames = n_tmax01_4.5)

###### Jan max temperature RCP 8.5
tmax01_8.5 <- rst.op(input1 = tmax01,
                     input2 = tmax01_8.5_ac,
                     proj_mask = ch_mask,
                     op = "addabs",
                     filename = "output/clim_vars/tmax01_8.5",
                     layernames = n_tmax01_8.5)

###### July min temperature RCP 4.5
tmin07_4.5 <- rst.op(input1 = tmin07,
                     input2 = tmin07_4.5_ac,
                     proj_mask = ch_mask,
                     op = "addabs",
                     filename = "output/clim_vars/tmin07_4.5",
                     layernames = n_tmin07_4.5)

###### July min temperature RCP 8.5
tmin07_8.5 <- rst.op(input1 = tmin07,
                     input2 = tmin07_8.5_ac,
                     proj_mask = ch_mask,
                     op = "addabs",
                     filename = "output/clim_vars/tmin07_8.5",
                     layernames = n_tmin07_8.5)

##### Precipitation

##### January precipitation RCP 4.5
prec01_4.5 <- rst.op(input1 = prec01,
                     input2 = prec01_4.5_pc,
                     proj_mask = ch_mask,
                     op = "addper",
                     filename = "output/clim_vars/prec01_4.5",
                     layernames = n_prec01_4.5)

##### January precipitation RCP 8.5
prec01_8.5 <- rst.op(input1 = prec01,
                     input2 = prec01_8.5_pc,
                     proj_mask = ch_mask,
                     op = "addper",
                     filename = "output/clim_vars/prec01_8.5",
                     layernames = n_prec01_8.5)
##### July precipitation RCP 4.5
prec07_4.5 <- rst.op(input1 = prec07,
                     input2 = prec07_4.5_pc,
                     proj_mask = ch_mask,
                     op = "addper",
                     filename = "output/clim_vars/prec07_4.5",
                     layernames = n_prec07_4.5)

##### July precipitation RCP 8.5
prec07_8.5 <- rst.op(input1 = prec07,
                     input2 = prec07_8.5_pc,
                     proj_mask = ch_mask,
                     op = "addper",
                     filename = "output/clim_vars/prec07_8.5",
                     layernames = n_prec07_8.5)


#### Interploate prediction data
# -------------------------------------------

tmax01_4.5_int <- interpolate.climdat(initras = tmax01,
                                      futras = tmax01_4.5,
                                      ntimesteps = ntimesteps,
                                      data_years = n_tmax01_4.5,
                                      year0 =  year0,
                                      varname = "tmax01",
                                      proj_mask = ch_mask,
                                      filename = "output/clim_vars/tmax01_4.5")

tmax01_8.5_int <- interpolate.climdat(initras = tmax01,
                                      futras = tmax01_8.5,
                                      ntimesteps = ntimesteps,
                                      data_years = n_tmax01_8.5,
                                      year0 =  year0,
                                      varname = "tmax01",
                                      proj_mask = ch_mask,
                                      filename = "output/clim_vars/tmax01_8.5")

tmin07_4.5_int <- interpolate.climdat(initras = tmin07,
                                      futras = tmin07_4.5,
                                      ntimesteps = ntimesteps,
                                      data_years = n_tmin07_4.5,
                                      year0 =  year0,
                                      varname = "tmin07",
                                      proj_mask = ch_mask,
                                      filename = "output/clim_vars/tmin07_4.5")

tmin07_8.5_int <- interpolate.climdat(initras = tmin07,
                                      futras = tmin07_8.5,
                                      ntimesteps = ntimesteps,
                                      data_years = n_tmin07_8.5,
                                      year0 =  year0,
                                      varname = "tmin07",
                                      proj_mask = ch_mask,
                                      filename = "output/clim_vars/tmin07_8.5")

prec01_4.5_int <- interpolate.climdat(initras = prec01,
                                      futras = prec01_4.5,
                                      ntimesteps = ntimesteps,
                                      data_years = n_prec01_4.5,
                                      year0 =  year0,
                                      varname = "prec01",
                                      proj_mask = ch_mask,
                                      filename = "output/clim_vars/prec01_4.5")

prec01_8.5_int <- interpolate.climdat(initras = prec01,
                                      futras = prec01_8.5,
                                      ntimesteps = ntimesteps,
                                      data_years = n_prec01_8.5,
                                      year0 =  year0,
                                      varname = "prec01",
                                      proj_mask = ch_mask,
                                      filename = "output/clim_vars/prec01_8.5")

prec07_4.5_int <- interpolate.climdat(initras = prec07,
                                      futras = prec07_4.5,
                                      ntimesteps = ntimesteps,
                                      data_years = n_prec07_4.5,
                                      year0 =  year0,
                                      varname = "prec07",
                                      proj_mask = ch_mask,
                                      filename = "output/clim_vars/prec07_4.5")

prec07_8.5_int <- interpolate.climdat(initras = prec07,
                                      futras = prec07_8.5,
                                      ntimesteps = ntimesteps,
                                      data_years = n_prec07_8.5,
                                      year0 =  year0,
                                      varname = "prec07",
                                      proj_mask = ch_mask,
                                      filename = "output/clim_vars/prec07_8.5")

### Climate variable sets ------------------------------------------
clim_vars_cc0 <- vector("list", ntimesteps + 1)

for(i in 1:(ntimesteps + 1)){
  clim_vars_cc0[[i]] <- stack(prec01, prec07, tmax01, tmin07)
}

clim_vars_4.5 <- mapply(prec01_4.5_int, prec07_4.5_int, tmax01_4.5_int, tmin07_4.5_int, FUN = stack)
clim_vars_8.5 <- mapply(prec01_8.5_int, prec07_8.5_int, tmax01_8.5_int, tmin07_8.5_int, FUN = stack)


# Save outputs ---------------------

save(
  clim_vars_cc0,
  clim_vars_4.5,
  clim_vars_8.5,
  file = "output/RData/06_climate_variables.RData"
)
