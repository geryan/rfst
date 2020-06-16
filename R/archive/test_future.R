source("R/spartan/spartan_settings.R")

library(magrittr)
library(future)
library(future.apply)

plan(multisession)


aa <- mtcars %$%
  future_mapply(
    FUN = sum,
    disp,
    hp,
    drat,
    wt,
    qsec
  )


aa

