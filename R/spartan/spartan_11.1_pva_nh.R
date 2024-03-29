## Run PVAs

source("R/spartan/spartan_settings.R")


library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(raster)
library(sp)
library(magrittr)
library(steps)

load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")
load(file = "output/RData/11.0.1_hab_set.RData")

source.functions("R/functions")


#nreplicates <- 20

command_args <- commandArgs(trailingOnly = TRUE)

i <- as.numeric(command_args[1])


simset <- do_pva(
  hab = hab_set[i,],
  ntimresteps = ntimesteps,
  nreps = nreplicates,
  index = 1,
  static = FALSE,
  ip_raster = FALSE,
  mod_habitat = FALSE
)

simpop <- get_pop_simulation(simset$simres)
lcc    <- get_lcc_simulation(simset$simres)

init_pop <- simset$ip


pva_emp <- emp(simpop)

pva_emp_all <- emp.all(simpop)

pva_p_extinct <- length(which(pva_emp_all[[1]] == 0))/length(pva_emp_all[[1]])


med_pop <- med.pop(simpop)

med_pop_all <- med.pop.all(simpop)



pva_res <- tibble(
  pva = list(simpop),
  emp = pva_emp,
  emp_all = pva_emp_all,
  med_pop,
  med_pop_all,
  p_extinct = pva_p_extinct,
  lcc = list(lcc),
  init_pop
) %>%
  mutate(
    p_extant = 1 - p_extinct
  )

pva <- bind_cols(
  hab_set[i,],
  pva_res
)


saveRDS(
  object = pva,
  file = sprintf(
    fmt = "%s/pva_cdnh_%s_%s.Rds",
    "/data/gpfs/projects/punim0995/rfst/output/spartan_RData/pva",
    hab_set$cscnid[i],
    hab_set$sp[i]
  )
)

