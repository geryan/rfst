# install packages

.libPaths("/home/ryange/R/gr_lib/")

lib <- .libPaths("/home/ryange/R/gr_lib/")

install.packages(
  pkgs = c(
    "devtools",
    "dismo",
    "dplyr",
    "doMC",
    "foreach",
    "furrr",
    "future",
    "future.apply",
    "gbm",
    "ggplot2",
    "lazyeval",
    "lubridate",
    "lwgeom",
    "magrittr",
    "patchwork",
    "purrr",
    "raster",
    "rasterVis",
    "Rcpp",
    "readr",
    "readxl",
    "rerddap",
    "rgdal",
    "rlang",
    "sf",
    "sp",
    "stringr",
    "tibble",
    "tidyr",
    "viridis"
  ),
  lib = lib,
  repos = "https://cran.ms.unimelb.edu.au/"
)


install.packages(
  pkgs = "udunits2",
  lib = "/home/ryange/R/gr_lib",
  configure.args='--with-udunits2-lib=/home/ryange/R/gr_lib'
)


install.packages(
  pkgs = "udunits2",
  lib = "/home/ryange/R/gr_lib",
  configure.args='--with-udunits2-include=/home/ryange/include/udunits2'
)


install.packages(
  pkgs = "udunits2",
  lib = "/home/ryange/R/gr_lib",
  configure.vars='UDUNITS2_INCLUDE'
)


