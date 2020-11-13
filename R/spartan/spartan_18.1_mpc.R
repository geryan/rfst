# 18 metapopulation capacity models data list

source("R/spartan/spartan_settings.R")


library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(raster)
library(sp)
library(sf)
library(magrittr)

load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")
load(file = "output/RData/18.0_mpc_list.RData")

source.functions("R/functions")



command_args <- commandArgs(trailingOnly = TRUE)

i <- as.numeric(command_args[1])


dispfun <- dispersal_negexp(1/mpc_dat$ddist[1])

patch_90 <- patmat(
  x = mpc_dat$predmaps[[i]],
  threshold = mpc_dat$threshold_90[i],
  write = FALSE
)



mpc_09 <- metacapstack(
  x = patch_90[[1:3]],
  f = dispfun
)


patch_75 <- patmat(
  x = mpc_dat$predmaps[[i]],
  threshold = mpc_dat$threshold_75[i],
  write = FALSE
)


mpc_75 <- metacapstack(
  x = patch_75,
  f = dispfun
)
