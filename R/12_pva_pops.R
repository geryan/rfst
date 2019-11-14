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
load(file = "output/RData/11_pvas.RData")
rm(simset_gg)

load(file = "output/RData/11_pvas_gg.RData")

source.functions("R/functions")

pops <- bind_rows(
  simset_lb,
  simset_gg
)  %>%
  dplyr::select(
    species,
    scenario,
    rep,
    pva_pops
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

pops.all <- pops %>%
  dplyr::select(
    species,
    scenario,
    rep,
    all.emp
  ) %>%
  unnest(all.emp) %>%
  rename(min.pop = all.emp)

save(
  pops,
  pops.all,
  file = "output/RData/12_pva_pops.RData"
)