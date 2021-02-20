# 13 metapopulation capacity models data list for PVA species

source("R/spartan/spartan_settings.R")


library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(raster)
library(sp)
library(sf)
library(magrittr)
library(igraph)
library(metacapa)

load(file = "output/RData/00_controls_eg.RData")
load(file = "output/RData/01_landscape_variables_eg.RData")
load(file = "output/RData/04.3_tsl_aggregated_eg.RData")
load(file = "output/RData/10_predict_SDMs_agg_eg.RData")
load(file = "output/RData/13.0_pva_mpc_list_eg.RData")

source.functions("R/functions")



command_args <- commandArgs(trailingOnly = TRUE)

i <- as.numeric(command_args[1])


mpcdat <- mpc_dat_pva_eg

agg_map <- mpcdat$aggmaps[[i]]

tsl <- tsl_agg_eg$tsl_agg[[which(tsl_agg_eg$yscn_id == mpcdat$yscn_id[i])]]

species_dat <- tribble(
  ~sp,    ~habfun,
  "polo", NA,
  "pevo", habitat.upfun,
  "peau", habitat.upfun,
  "smle", NA,
  "tyte", habitat.downupfun,
  "vava", NA
)

habfun <- species_dat$habfun[[which(species_dat$sp == mpcdat$sp[i])]]

if(!is.na(habfun)){
  hd_vals <- getValues(tsl)
  
  hd_vals_mod <- apply(
    X = hd_vals,
    FUN = function(x){
      if(is.na(x)){
        return(1)
      } else {
        return(habfun(x))
      }
    },
    MARGIN = c(1,2)
  )
  
  agg_vals <- getValues(agg_map)
  
  hab_vals <- agg_vals * hd_vals_mod
  
  agg_map[] <- hab_vals
}


dispfun <- dispersal_negexp(1/mpcdat$ddist[i])

patch <- patmat(
  x = agg_map,
  threshold = mpcdat$threshold_max_sss[i],
  write = FALSE
)

year0 <- ifelse(
  mpcdat$yearid[i] == "EG20",
  2020,
  2019
)

mpc <- metacapstack(
  x = patch,
  f = dispfun,
  year0 = year0
) %>%
  rename(
    mpc = metapopulation_capacity
  )



mpc_results_pva <- bind_cols(
  mpcdat[i,] %>%
    dplyr::select(-aggmaps),
  mpc
)


saveRDS(
  object = mpc_results_pva,
  file = sprintf(
    "%s/mpc_pva5_%s_%s.Rds",
    "/data/gpfs/projects/punim1340/rfst_eg/output/spartan_RData/mpc_pva",
    mpcdat$ycscnid[i], # NB needs cscnid not scn_id if multiple climate scenarios with each scenario
    mpcdat$sp[i]
  )
)

