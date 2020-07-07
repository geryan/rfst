
source("R/spartan/spartan_settings.R")

library(raster)
library(dplyr)
library(sf)
library(lubridate)
library(purrr)
library(tibble)
library(future)
#library(furrr)
library(tidyr)
library(future.apply)
library(dismo)
library(gbm)

load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")
load(file = "output/RData/07_combined_variables.RData")
load(file = "output/RData/09_fit_distribution_models.RData")

source.functions("R/functions")


rds_list <- list.files("/output/spartan_RData/habitat_pred_aggregated")

