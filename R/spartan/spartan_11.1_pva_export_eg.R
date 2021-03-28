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

load(file = "output/RData/00_controls_eg.RData")
load(file = "output/RData/01_landscape_variables_eg.RData")
load(file = "output/RData/11.0_pva_species_dat_eg.RData")
load(file = "output/RData/11.0.1_hab_set_eg.RData")

source.functions("R/functions")

hab_set_eg <- hab_set_eg %>%
  filter(scenario == "EG20_TH00_rcp45_PB" | scenario == "EG20_TH30_rcp45_PB") %>%
  filter(yearid != "EG19", climate_model == "ACCESS1-0", rcp =="rcp45")

nreplicates <- 20

command_args <- commandArgs(trailingOnly = TRUE)

i <- as.numeric(command_args[1])


simset <- do_pva(
  hab = hab_set_eg[i,],
  ntimresteps = ntimesteps,
  nreps = nreplicates,
  index = 1,
  static = FALSE,
  ip_raster = TRUE,
  mod_habitat = FALSE
)


temp_mask <- raster::aggregate(
  x = eg_mask,
  fact = 2
) # generalise

#temp_mask <- eg_mask_agg

ipsum <- sum(simset$ip_raster)

rst_pop <- export_pop_rasters(
  simulation_result = simset$simres,
  initial_pops = simset$ip_raster
) %>%
  write_pop_rasters(
    path = "/data/gpfs/projects/punim1340/rfst_eg/output/pva_pops",
    id = hab_set_eg$ycscnid[i],
    sp = hab_set_eg$sp[i],
    proj_mask = temp_mask
  )


cc_raster <- hab_set_eg$hab_map[[1]][[1]]

cc_val <- getValues(cc_raster) %>%
  do_cc_fun(hab_set_eg[i,])

cc_raster[] <- cc_val


rst_k <- export_k_rasters(
  simulation_result = simset$simres,
  initial_k = cc_raster
) %>%
  brick %>%
  rst.op(
    op = "writeBrick",
    proj_mask = temp_mask,
    filename = sprintf(
      fmt = "%s/pva_k_%s_%s.grd",
      "/data/gpfs/projects/punim1340/rfst_eg/output/pva_k",
      hab_set_eg$ycscnid[i],
      hab_set_eg$sp[i]
    ),
    layernames = sprintf(
      "k%s",
      c("0", "1-10", "11-20", "21-30", "31-40", "41-50")
    )
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
  init_pop,
  rst_pop = list(rst_pop),
  rst_k = list(rst_k)
) %>%
  mutate(
    p_extant = 1 - p_extinct
  )

pva <- bind_cols(
  hab_set_eg[i,],
  pva_res
)


saveRDS(
  object = pva,
  file = sprintf(
    fmt = "%s/pva_exp_%s_%s_eg.Rds",
    "/data/gpfs/projects/punim1340/rfst_eg/output/spartan_RData/pva",
    hab_set_eg$ycscnid[i],
    hab_set_eg$sp[i]
  )
)

