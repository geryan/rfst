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


# All VBA data --------------------------------------------------------------

vba.files <- list.files(
  path = "data/tabular/",
  pattern = "vba",
  recursive = FALSE
)

vba.dat <- lapply(
  X = paste0(
    "data/tabular/",
    vba.files
  ),
  FUN = proc.vba,
  project.crs = ch_proj,
  cutoff.date = "2009-03-01"
) %>%
  do.call(
    what = rbind,
    args = .
  )


## LBP raw ---- 

raw_lb <- proc.vba("data/tabular/vba_lb_all_20190826.csv", project.crs = ch_proj, cutoff.date = "2009-03-01") %>%
  arrange(date)


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

vba_petaurus      <- proc.vba(x = "data/tabular/vba_petaurus_all_20190703.csv",               project.crs = ch_proj, sm = TRUE, pattern = "spotlight|camera")
vba_pseudocheirus <- proc.vba(x = "data/tabular/vba_pseudocheirus_all_20190703.csv",          project.crs = ch_proj, sm = TRUE, pattern = "spotlight|camera")
vba_tcunninghami  <- proc.vba(x = "data/tabular/vba_trichosuruscunninghami_all_20190703.csv", project.crs = ch_proj, sm = TRUE, pattern = "spotlight|camera")
vba_tvulpecula    <- proc.vba(x = "data/tabular/vba_trichosurusvulpecula_all_20190703.csv",   project.crs = ch_proj, sm = TRUE, pattern = "spotlight|camera")

am <- rbind(vba_petaurus, vba_pseudocheirus, vba_tcunninghami, vba_tvulpecula)

am <- am[!duplicated(am),]

am <- am[ch_rfa,]

## Buffer and sample PA data

# LBP from 1980 without background

pa_lb_80x <- sample.pa(
  lb_80,
  ch_rfa,
  cellsize = 500
)

st_write(
  obj = pa_lb_80x,
  dsn = "output/pa/pa_lb_80x_ch.shp",
  delete_dsn = TRUE
)

# LBP from 1980 with background

pa_lb_80b <- buff.sample.pa(
  x = lb_80,
  y = am,
  rfa = ch_rfa,
  cellsize = 500,
  buff.dist = 500
)

st_write(
  obj = pa_lb_80b,
  dsn = "output/pa/pa_lb_80b_ch.shp",
  delete_dsn = TRUE
)

# LBP from 2009 without background

pa_lb_09x <- sample.pa(
  lb_09,
  ch_rfa,
  cellsize = 500
)

st_write(
  obj = pa_lb_09x,
  dsn = "output/pa/pa_lb_09x_ch.shp",
  delete_dsn = TRUE
)

# LBP from 2009 with background

pa_lb_09b <- buff.sample.pa(
  x = lb_09,
  y = am,
  rfa = ch_rfa,
  cellsize = 500,
  buff.dist = 500
)

st_write(
  obj = pa_lb_09b,
  dsn = "output/pa/pa_lb_09b_ch.shp",
  delete_dsn = TRUE
)



# GG from 1980 without background

pa_gg_80x <- sample.pa(
  gg_80,
  ch_rfa,
  cellsize = 500
)

st_write(
  obj = pa_gg_80x,
  dsn = "output/pa/pa_gg_80x_ch.shp",
  delete_dsn = TRUE
)

# GG from 1980 with background

pa_gg_80b <- buff.sample.pa(
  x = gg_80,
  y = am,
  rfa = ch_rfa,
  cellsize = 500,
  buff.dist = 500
)

st_write(
  obj = pa_gg_80b,
  dsn = "output/pa/pa_gg_80b_ch.shp",
  delete_dsn = TRUE
)

# GG from 2009 without background

pa_gg_09x <- sample.pa(
  gg_09,
  ch_rfa,
  cellsize = 500
)

st_write(
  obj = pa_gg_09x,
  dsn = "output/pa/pa_gg_09x_ch.shp",
  delete_dsn = TRUE
)

# GG from 2009 with background

pa_gg_09b <- buff.sample.pa(
  x = gg_09,
  y = am,
  rfa = ch_rfa,
  cellsize = 500,
  buff.dist = 500
)

st_write(
  obj = pa_gg_09b,
  dsn = "output/pa/pa_gg_09b_ch.shp",
  delete_dsn = TRUE
)

pa_gg_09bb <- buff.sample.pa(
  x = gg_09,
  y = am,
  rfa = ch_rfa,
  cellsize = 2000,
  buff.dist = 2000
)

st_write(
  obj = pa_gg_09bb,
  dsn = "output/pa/pa_lb_09bb_ch.shp",
  delete_dsn = TRUE
)


save(
  pa_lb_09b,
  pa_lb_80b,
  pa_gg_09b,
  pa_gg_09bb,
  pa_gg_80b,
  pa_lb_09x,
  pa_lb_80x,
  pa_gg_09x,
  pa_gg_80x,
  file = "output/RData/02_species_occurrences.RData"
)

## ARI data ----

# lb_ari <- read_csv(file = "data/tabular/ari/Leadbeaters010319.txt") %>%
#   st_as_sf(coords = c("LONGITUDED", "LATITUDEDD"), crs = st_crs(28355)) %>%
#   st_transform(crs = ch_proj) %>%
#   mutate(date = dmy(SURVEY_STA))
#   dplyr::select(PA, date, geometry)



