## Run PVAs

source("R/spartan/spartan_settings.R")


library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(raster)
library(sp)
library(magrittr)
library(steps)

load(file = "output/RData/00_controls_eg.RData")
load(file = "output/RData/01_landscape_variables_eg.RData")
load(file = "output/RData/11.0.1_hab_set_eg.RData")

source.functions("R/functions")


cc_set_eg <- hab_set_eg %>%
  mutate(
    cc_mean = pmap(
      .l = list(
        hab_map = hab_map,
        cc = cc,
        threshold = threshold,
        z = z,
        id = ycscnid,
        sp = sp
      ),
      .f = function(hab_map, cc, threshold, z, id, sp){
        
        cc_raster <- hab_map
        
        cc_val <- getValues(cc_raster) %>%
          cc_fun(
            carcap = cc,
            z = z,
            threshold = threshold
          )
        
        cc_raster[] <- cc_val
        
        cc_mean <- mean(cc_raster, na.rm = TRUE) %>%
          rst.op(
            op = "writeonly",
            filename = sprintf(
              "/data/gpfs/projects/punim1340/rfst_eg/output/k_mean/k_mean_%s_%s.grd",
              id,
              sp
            ),
            layernames = "k_mean",
            proj_mask = hab_map[[1]]
          )
        
        return(cc_mean)
        
      }
    )
  ) %>%
  dplyr::select(ycscnid, sp, cc_mean)


save(
  object = cc_set_eg,
  file = "/data/gpfs/projects/punim0995/rfst/output/RData/11.3_cc_average_eg.RData"
)

