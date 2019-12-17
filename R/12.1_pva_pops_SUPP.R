# 12 PVA populations

library(steps)
library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(magrittr)
library(raster)
library(sp)


load(file = "output/RData/00_comp_controls.RData")
load(file = "output/RData/11.1_pvas_lb.RData")
load(file = "output/RData/11.1_pvas_gg.RData")

source.functions("R/functions")

pops_supp <- bind_rows(
  simset_lb_supp,
  simset_gg_supp
)  %>%
  dplyr::select(
    species,
    scenario,
    rep,
    pva_pops,
    popsize,
    tmn
  ) %>%
  rowwise %>%
  mutate(
    emp = emp(pva_pops),
    pr.ex = pr.ex(pva_pops),
    all.emp = emp.all(pva_pops),
    mmp = mmp(pva_pops)
  ) %>%
  mutate(
    scenario = case_when(scenario == 1 ~ "BAU",
                         scenario == 4 ~ "No harvest",
                         scenario == 8 ~ "Cease harvest Y30")
  )

pops.all_supp <- pops %>%
  dplyr::select(
    species,
    scenario,
    rep,
    pva_pops,
    popsize,
    tmn,all.emp
  ) %>%
  unnest(all.emp) %>%
  rename(min.pop = all.emp)

save(
  pops_supp,
  pops.all_supp,
  file = "output/RData/12.1_pva_pops_supp.RData"
)