# 02 Species occurrence

library(dplyr)
library(magrittr)
library(raster)
library(sf)
library(readr)
library(readxl)
library(tidyr)

load(file = "output/RData/00_comp_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")

source.functions("R/functions")

## LBP raw ---- 

lb_80 <- proc.vba("data/tabular/vba_lb_all_20190826.csv", project.crs = ch_proj, cutoff.date = "1980-01-01") %>%
  arrange(date)


lb_80 <- lb_80[ch_rfa, ]

lb_80 <- lb_80[!duplicated(lb_80),]

lb_09 <- lb_80 %>%
  filter(date > ymd("2009-03-01"))


## GG raw ----

gg0_ari <- read_excel(path = "data/tabular/BoA_SB_Combined_VBA_upload_v4.xls") %>%
  dplyr::select(-starts_with("leave")) %>%
  rename("lon" = `X-coordinate (easting or longitude)`, "lat" = `Y-coordinate (northing or latitude)`, date = `Start date`) %>%
  fill(lon, lat, .direction = "down") %>%
  fill(date, .direction = "down") %>%
  filter(`Taxon Name` == "Misc Target taxa not found") %>%
  dplyr::select(date, lon, lat) %>%
  mutate(date = as.Date(date),
         PA = 0) %>%
  st_as_sf(coords = c("lon", "lat"), crs = st_crs(28355)) %>%
  st_transform(crs = st_crs(ch_mask)) %>%
  dplyr::select(PA, date, geometry)

gg_vba_80 <- proc.vba("data/tabular/vba_gg_all_20190826.csv", project.crs = ch_proj, cutoff.date = "1980-01-01") %>%
  arrange(date)

gg_80 <- gg_vba_80 %>%
  rbind(gg0_ari) %>%
  dplyr::arrange(date)

gg_80 <- gg_80[ch_rfa, ]

gg_80 <- gg_80[!duplicated(gg_80),]

gg_09 <- gg_80 %>%
  filter(date > ymd("2009-03-01"))

## Arboreal mammal supp data ----

vba_petaurus <- proc.vba(x = "data/tabular/vba_petaurus_all_20190703.csv", project.crs = ch_proj, sm = TRUE)
vba_pseudocheirus <- proc.vba(x = "data/tabular/vba_pseudocheirus_all_20190703.csv", project.crs = ch_proj, sm = TRUE)
vba_tcunninghami <- proc.vba(x = "data/tabular/vba_trichosuruscunninghami_all_20190703.csv", project.crs = ch_proj, sm = TRUE)
vba_tvulpecula <- proc.vba(x = "data/tabular/vba_trichosurusvulpecula_all_20190703.csv", project.crs = ch_proj, sm = TRUE)

am <- rbind(vba_petaurus, vba_pseudocheirus, vba_tcunninghami, vba_tvulpecula)

am <- am[!duplicated(am),]

am <- am[ch_rfa,]

## Buffer and sample PA data

pa_lb_80 <- buff.sample.pa(
  x = lb_80,
  y = am,
  rfa = ch_rfa,
  cellsize = 500
)

st_write(
  obj = pa_lb_80,
  dsn = "output/pa/pa_lb_80_ch.shp",
  delete_dsn = TRUE
)

pa_lb_09 <- buff.sample.pa(
  x = lb_09,
  y = am,
  rfa = ch_rfa,
  cellsize = 500
)

st_write(
  obj = pa_lb_09,
  dsn = "output/pa/pa_lb_09_ch.shp",
  delete_dsn = TRUE
)

pa_gg_80 <- buff.sample.pa(
  x = gg_80,
  y = am,
  rfa = ch_rfa,
  cellsize = 500
)

st_write(
  obj = pa_gg_80,
  dsn = "output/pa/pa_gg_80_ch.shp",
  delete_dsn = TRUE
)

pa_gg_09 <- buff.sample.pa(
  x = gg_09,
  y = am,
  rfa = ch_rfa,
  cellsize = 500
)

st_write(
  obj = pa_gg_09,
  dsn = "output/pa/pa_gg_09_ch.shp",
  delete_dsn = TRUE
)


save(
  pa_lb_09,
  pa_lb_80,
  pa_gg_09,
  pa_gg_80,
  file = "output/RData/02_species_occurrences.RData"
)

## ARI data ----

# lb_ari <- read_csv(file = "data/tabular/ari/Leadbeaters010319.txt") %>%
#   st_as_sf(coords = c("LONGITUDED", "LATITUDEDD"), crs = st_crs(28355)) %>%
#   st_transform(crs = ch_proj) %>%
#   mutate(date = dmy(SURVEY_STA))
#   dplyr::select(PA, date, geometry)



