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

load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")
load(file = "output/RData/11.0.1_hab_set.RData")

source.functions("R/functions")


cc_set <- hab_set %>%
  mutate(
    cc_mean = pmap(
      .l = list(
        hab_map = hab_map,
        cc = cc,
        threshold = threshold,
        z = z,
        id = cscnid,
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
              "/data/gpfs/projects/punim0995/rfst/output/k_mean/k_mean_%s_%s.grd",
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
  dplyr::select(cscnid, sp, cc_mean)


saveRDS(
  object = cc_set,
  file = "/data/gpfs/projects/punim0995/rfst/output/RData/11.3_cc_average.RData"
)

