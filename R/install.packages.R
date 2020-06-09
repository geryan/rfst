# install packages

.libPaths("/home/ryange/R/gr_lib/")

lib <- .libPaths("/home/ryange/R/gr_lib/")

install.packages(
  pkgs = c(
    "dismo",
    "doMC",
    "dplyr",
    "foreach",
    "future",
    "future.apply",
    "gbm",
    "ggplot2",
    "lubridate",
    "lwgeom",
    "magrittr",
    "metacapa",
    "purrr",
    "raster",
    "rasterVis",
    "readr",
    "readxl",
    "rerddap",
    "rgdal",
    "rlang",
    "sf",
    #"steps",
    "tibble",
    "tidyr",
    "viridis"
  ),
  lib = lib,
  repos = "https://cran.ms.unimelb.edu.au/"
)