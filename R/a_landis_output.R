# a. LANDIS raw outputs and input rasters

library(dplyr)
library(raster)


source(file = "R/functions/source.functions.R")
source.functions("R/functions")

landis_s1_01 <- landis.out("s1_01")
landis_s4_01 <- landis.out("s1_01")
landis_s8_01 <- landis.out("s1_01")


save(
  landis_s1_01,
  landis_s4_01,
  landis_s8_01,
  file = "output/RData/a_landis_output.RData"
)