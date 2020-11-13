# 17 predict SDMs for metapopulation capacity models 

source("R/spartan/spartan_settings.R")


library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(raster)
library(sp)
library(gbm)
library(dismo)
#library(sf)

load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")
load(file = "output/RData/07a_varset_mpc.RData")

source.functions("R/functions")

p_list <- list.files("/data/gpfs/projects/punim0995/rfst/output/spartan_RData/mpc_pred/")

p_id <- sub(
  pattern = "pred_mpc_",
  replacement = "",
  x = p_list
) %>%
  sub(
    pattern = ".Rds",
    replacement = "",
    x = .
  )

p_sp <- sub(
  pattern = ".*_",
  replacement = "",
  x = p_id
)


p_csn <- strsplit(
  x = p_id,
  split = "_"
) %>%
  lapply(
    FUN = function(x){
      x[1:5]
    }
  ) %>%
  sapply(
    FUN = paste,
    collapse = "_"
  )


p_maps <- lapply(
  X = p_list,
  FUN = function(x){
    z <- readRDS(
      file = sprintf(
        "/data/gpfs/projects/punim0995/rfst/output/spartan_RData/mpc_pred/%s",
        x
      )
    )
    
    return(z)
    
  }
)

preds <- tibble(
  cscnid = p_csn,
  sp =  p_sp,
  predmaps = p_maps
)



spid <- sdm_results_mpc_ch$sp[i]

threshold <- pred[[1]] %>%
  raster::extract(
    y = pa_data_ch$pa_dat[[which(pa_data_ch$sp == spid)]] %>% dplyr::filter(PA == 1)
  ) %>%
  quantile(probs = c(0.75)) %>%
  signif(digits = 3)

patch_pred <- patmat(
  x = pred,
  threshold = threshold,
  write = FALSE
)









