# 02 Species occurrence

library(dplyr)
library(magrittr)
library(raster)
library(sf)

load(file = "output/RData/00_comp_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")

source.functions("R/functions")



####### Remove later
rfa <- read_sf("data/shapefiles/RFA/")%>%
  st_transform(crs = ch_proj)

ch_rfa <- rfa[rfa$NAME== "CENTRAL HIGHLANDS",] 

ch_rfa

########