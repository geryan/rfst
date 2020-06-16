source("R/spartan/spartan_settings.R")

library(future)
library(future.apply)

plan(multisession)

print(availableCores())