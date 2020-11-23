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

load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")
load(file = "output/RData/04.3_tsl_aggregated.RData")
load(file = "output/RData/13.0_pva_mpc_list.RData")

source.functions("R/functions")


command_args <- commandArgs(trailingOnly = TRUE)

i <- as.numeric(command_args[1])

mpc_dat_pva <- mpc_dat_pva %>%
  filter(sp != "smle" & sp != "vava")

agg_map <- mpc_dat_pva$aggmaps[[i]]

tsl <- tsl_agg_ch$tsl_agg[[which(tsl_agg_ch$scn_id == mpc_dat_pva$scn_id[i])]]

species_dat <- tribble(
  ~sp,    ~habfun,
  "gyle", habitat.downupfun,
  "pevo", habitat.upfun,
  "peau", habitat.upfun,
  "smle", NA,
  "tyte", habitat.downupfun,
  "vava", NA
)

hab_dyn <- tsl

hd_vals <- getValues(hab_dyn)

habfun <- species_dat$habfun[[which(species_dat$sp == mpc_dat_pva$sp[i])]]


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

hab_dyn[] <- hd_vals_mod

agg_vals <- getValues(agg_map)

hab_vals <- agg_vals * hd_vals_mod

agg_map[] <- hab_vals

dispfun <- dispersal_negexp(1/mpc_dat_pva$ddist[i])

patch <- patmat(
  x = agg_map,
  threshold = mpc_dat_pva$threshold_max_sss[i],
  write = FALSE
)

mpc <- metacapstack(
  x = patch,
  f = dispfun,
  year0 = year0
) %>%
  rename(
    mpc = metapopulation_capacity
  )



patch_07 <- patmat(
  x = agg_map,
  threshold = 0.7,
  write = FALSE
)


mpc_07 <- metacapstack(
  x = patch_07,
  f = dispfun,
  year0 = year0
) %>%
  rename(
    mpc_07 = metapopulation_capacity
  )


patch_05 <- patmat(
  x = agg_map,
  threshold = 0.5,
  write = FALSE
)


mpc_05 <- metacapstack(
  x = patch_05,
  f = dispfun,
  year0 = year0
) %>%
  rename(
    mpc_05 = metapopulation_capacity
  )


mpc_results_pva <- bind_cols(
  mpc_dat_pva[i,],
  mpc,
  mpc_05,
  mpc_07
) %>%
  rename(
    year = year...14
  ) %>%
  dplyr::select(
    -year...16,
    -year...18
  )


saveRDS(
  object = mpc_results_pva,
  file = sprintf(
    "%s/mpc_pva_habitat_%s_%s.Rds",
    "/data/gpfs/projects/punim0995/rfst/output/spartan_RData/mpc_pva_habitat",
    mpc_dat_pva$cscnid[i], # NB needs cscnid not scn_id if multiple climate scenarios with each scenario
    mpc_dat_pva$sp[i]
  )
)

