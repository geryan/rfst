# 06 Climate variables

library(raster)
library(magrittr)
library(rerddap)

load(file = "output/RData/00_comp_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")

source.functions("R/functions")

# Current climate
# -----------------------------------------

raw_tmax01 <- getData(name = "worldclim", var = "tmax", path = paste0(proj_path, "/output/wc_30sec/tmax/"), res = 0.5, lat = -60, lon = 120)[[1]] %>%
  projectRaster(to = ch_mask) %>%
  mask(mask = ch_mask, filename = "output/clim_vars/raw_tmax01.grd", overwrite = TRUE)

names(raw_tmax01) <- "raw_tmax01"

tmax01 <- rst.op(input1 = raw_tmax01,
                 op = "div10",
                 proj_mask = ch_mask,
                 filename = "output/clim_vars/tmax01.grd",
                 layernames = "tmax01")


raw_tmin07 <- getData(name = "worldclim", var = "tmin", path = paste0(proj_path, "/output/wc_30sec/tmin/"), res = 0.5, lat = -60, lon = 120)[[7]] %>%
  projectRaster(to = ch_mask) %>%
  mask(mask = ch_mask, filename = "output/clim_vars/raw_tmin07.grd", overwrite = TRUE)

names(raw_tmin07) <- "raw_tmin07"

tmin07 <- rst.op(input1 = raw_tmin07,
                 op = "div10",
                 proj_mask = ch_mask,
                 filename = "output/clim_vars/tmin07.grd",
                 layernames = "tmin07")


prec01 <- getData(name = "worldclim", var = "prec", path = paste0(proj_path, "/output/wc_30sec/prec/"), res = 0.5, lat = -60, lon = 120)[[1]] %>%
  projectRaster(to = ch_mask) %>%
  mask(mask = ch_mask, filename = "output/clim_vars/prec01.grd", overwrite = TRUE)

names(prec01) <- "prec01"


prec07 <- getData(name = "worldclim", var = "prec", path = paste0(proj_path, "/output/wc_30sec/prec/"), res = 0.5, lat = -60, lon = 120)[[7]] %>%
  projectRaster(to = ch_mask) %>%
  mask(mask = ch_mask, filename = "output/clim_vars/prec07.grd", overwrite = TRUE)

names(prec07) <- "prec07"

# Future climate
# -------------------------------------------------------------
# Absolute change in temperature


erddap.url <- "http://nrm-erddap.nci.org.au/erddap/"

climate.models <- c(
  "ACCESS1-0",  # in landis growth model as consensus model with those inputs
  "CanESM2",    # climate futures tool 'best case' (high consensus)
  "GFDL-ESM2M", # climate futures tool 'worst case' (very low consensus)
  "NorESM1-M"   # climate futures tool 'maximum consensus' (high consensus)
)

climate.variables <- c(
  "pr_djf",     # precipitation december, january, february
  "pr_jja",     # precipitation june, july, august
  "tasmax_djf", # max air temperature at surface december, january, february,
  "taxmin_jja"  # minimum air temperature at surface june, july, august
)

rcps <- c(
  "rcp45", # relative concentration pathway 4.5
  "rcp85"  # relative concentration pathway 8.5
)

raw_tmax01_4.5_ac <- griddap(
  x = "tasmax_Amon_ACCESS1-0_rcp45_r1i1p1_abs-change-wrt-seasavg-clim_native",
  latitude = c(-40.10297775, -32.64199448),
  longitude = c(140.625, 150),
  time = c("2025-01-01T12:00:00", "2090-01-01T12:00:00"),
  fields = "tasmax_january",
  url = "http://nrm-erddap.nci.org.au/erddap/")

raw_tmax01_8.5_ac <- griddap(
  x = "tasmax_Amon_ACCESS1-0_rcp85_r1i1p1_abs-change-wrt-seasavg-clim_native",
  latitude = c(-40.10297775, -32.64199448),
  longitude = c(140.625, 150),
  time = c("2025-01-01T12:00:00", "2090-01-01T12:00:00"),
  fields = "tasmax_january",
  url = "http://nrm-erddap.nci.org.au/erddap/")

raw_tmin07_4.5_ac <- griddap(
  x = "tasmin_Amon_ACCESS1-0_rcp45_r1i1p1_abs-change-wrt-seasavg-clim_native",
  latitude = c(-40.10297775, -32.64199448),
  longitude = c(140.625, 150),
  time = c("2025-01-01T12:00:00", "2090-01-01T12:00:00"),
  fields = "tasmin_july",
  url = "http://nrm-erddap.nci.org.au/erddap/")

raw_tmin07_8.5_ac <- griddap(
  x = "tasmin_Amon_ACCESS1-0_rcp85_r1i1p1_abs-change-wrt-seasavg-clim_native",
  latitude = c(-40.10297775, -32.64199448),
  longitude = c(140.625, 150),
  time = c("2025-01-01T12:00:00", "2090-01-01T12:00:00"),
  fields = "tasmin_july",
  url = "http://nrm-erddap.nci.org.au/erddap/")

# Years data available
n_tmax01_4.5 <- as.numeric(sub("-.*", "", unique(raw_tmax01_4.5_ac$data$time)))
n_tmax01_8.5 <- as.numeric(sub("-.*", "", unique(raw_tmax01_8.5_ac$data$time)))
n_tmin07_4.5 <- as.numeric(sub("-.*", "", unique(raw_tmin07_4.5_ac$data$time)))
n_tmin07_8.5 <- as.numeric(sub("-.*", "", unique(raw_tmin07_8.5_ac$data$time)))

# Reporojected layers
tmax01_4.5_ac <- rascc(raw_tmax01_4.5_ac, new.proj.layer = ch_mask, filename = "output/clim_vars/tmax01_4.5_ac.grd")
tmax01_8.5_ac <- rascc(raw_tmax01_8.5_ac, new.proj.layer = ch_mask, filename = "output/clim_vars/tmax01_8.5_ac.grd")
tmin07_4.5_ac <- rascc(raw_tmin07_4.5_ac, new.proj.layer = ch_mask, filename = "output/clim_vars/tmin07_4.5_ac.grd")
tmin07_8.5_ac <- rascc(raw_tmin07_8.5_ac, new.proj.layer = ch_mask, filename = "output/clim_vars/tmin07_8.5_ac.grd")

##### Percentage change in precipitation
raw_prec01_4.5_pc <- griddap(
  x = "pr_Amon_ACCESS1-0_rcp45_r1i1p1_perc-change-wrt-seassum-clim_native",
  latitude = c(-40.10297775, -32.64199448),
  longitude = c(140.625, 150),
  time = c("2025-01-01T12:00:00", "2090-01-01T12:00:00"),
  fields = "pr_january",
  url = "http://nrm-erddap.nci.org.au/erddap/")

raw_prec01_8.5_pc <- griddap(
  x = "pr_Amon_ACCESS1-0_rcp85_r1i1p1_perc-change-wrt-seassum-clim_native",
  latitude = c(-40.10297775, -32.64199448),
  longitude = c(140.625, 150),
  time = c("2025-01-01T12:00:00", "2090-01-01T12:00:00"),
  fields = "pr_january",
  url = "http://nrm-erddap.nci.org.au/erddap/")

raw_prec07_4.5_pc <- griddap(
  x = "pr_Amon_ACCESS1-0_rcp45_r1i1p1_perc-change-wrt-seassum-clim_native",
  latitude = c(-40.10297775, -32.64199448),
  longitude = c(140.625, 150),
  time = c("2025-01-01T12:00:00", "2090-01-01T12:00:00"),
  fields = "pr_july",
  url = "http://nrm-erddap.nci.org.au/erddap/")

raw_prec07_8.5_pc <- griddap(
  x = "pr_Amon_ACCESS1-0_rcp85_r1i1p1_perc-change-wrt-seassum-clim_native",
  latitude = c(-40.10297775, -32.64199448),
  longitude = c(140.625, 150),
  time = c("2025-01-01T12:00:00", "2090-01-01T12:00:00"),
  fields = "pr_july",
  url = "http://nrm-erddap.nci.org.au/erddap/")


# NEED TO FIX PROCESSING BECAUSE RAW DATA FRAMES NOW ggriddap_nc class

#Years data available
n_prec01_4.5 <- as.numeric(sub("-.*", "", unique(raw_prec01_4.5_pc$data$time)))
n_prec01_8.5 <- as.numeric(sub("-.*", "", unique(raw_prec01_4.5_pc$data$time)))
n_prec07_4.5 <- as.numeric(sub("-.*", "", unique(raw_prec07_4.5_pc$data$time)))
n_prec07_8.5 <- as.numeric(sub("-.*", "", unique(raw_prec07_4.5_pc$data$time)))

#Reprojected layers
prec01_4.5_pc <- rascc(raw_prec01_4.5_pc, new.proj.layer = ch_mask, filename = "output/clim_vars/prec01_4.5_pc.grd")
prec01_8.5_pc <- rascc(raw_prec01_8.5_pc, new.proj.layer = ch_mask, filename = "output/clim_vars/prec01_8.5_pc.grd")
prec07_4.5_pc <- rascc(raw_prec07_4.5_pc, new.proj.layer = ch_mask, filename = "output/clim_vars/prec07_4.5_pc.grd")
prec07_8.5_pc <- rascc(raw_prec07_8.5_pc, new.proj.layer = ch_mask, filename = "output/clim_vars/prec07_8.5_pc.grd")



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
