# 11 run PVAs


library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(future)
library(future.apply)
library(raster)
library(sp)


load(file = "output/RData/00_comp_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")
load(file = "output/RData/07_combined_variables.RData")
load(file = "output/RData/09_fit_distribution_models.RData")
load(file = "output/RData/10_predict_SDMs.RData")

source.functions("R/functions")
