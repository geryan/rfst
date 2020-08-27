# 02 Species occurrence

source("R/spartan/spartan_settings.R")

library(magrittr)
library(raster)
library(sf)
library(readr)
library(readxl)
library(tidyr)
library(dplyr)
#library(future)
#library(future.apply)
library(lubridate)


load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")

source.functions("R/functions")


# All VBA data --------------------------------------------------------------

vba_files <- list.files(
  path = "data/tabular/",
  pattern = "vba",
  recursive = FALSE
)

vba_dat <- lapply(
  X = paste0(
    "data/tabular/",
    vba_files
  ),
  FUN = proc.vba,
  project.crs = ch_proj,
  cutoff.date = "2009-03-01"
) %>%
  do.call(
    what = rbind,
    args = .
  ) %>%
  distinct(
    species,
    PA,
    date,
    proj_id,
    survey_method,
    geometry
  ) #%>%
  # mutate(
  #   genus = sub(
  #     pattern = " .*",
  #     replacement = "",
  #     x = species
  #   )
  # ) %>%
  # dplyr::select(
  #   species,
  #   genus,
  #   everything()
  # )


# ARI Greater Glider Absences for Central Highlands -------------------------

gg0_ari_ch <- read_excel(path = "data/tabular/BoA_SB_Combined_VBA_upload_v4.xls") %>%
  dplyr::select(-starts_with("leave")) %>%
  rename(
    proj_id = `VBA Project ID`,
    lon = `X-coordinate (easting or longitude)`,
    lat = `Y-coordinate (northing or latitude)`,
    date = `Start date`
  ) %>%
  fill(lon, lat, .direction = "down") %>%
  fill(date, .direction = "down") %>%
  fill(proj_id, .direction = "down") %>%
  filter(`Taxon Name` == "Misc Target taxa not found") %>%
  mutate(
    date = as.Date(date),
    PA = 0,
    survey_method = "Spotlighting",
    species = "Petauroides volans",
    genus = "Petauroides"
  ) %>%
  st_as_sf(coords = c("lon", "lat"), crs = st_crs(28355)) %>%
  st_transform(crs = st_crs(ch_mask)) %>%
  dplyr::select(
    species,
    # genus,
    PA,
    date,
    proj_id,
    survey_method,
    geometry
  )

# Combine and subset to Central Highlands

vba_dat <- vba_dat %>%
  rbind(gg0_ari_ch)

vba_dat_ch <- vba_dat[ch_rfa,]

# Presence-absence-pseudo-absence

species_list <- c(
  "Gymnobelideus leadbeateri",
  "Petauroides volans",
  "Petaurus australis",
  #"Potorous longipes",
  "Sminthopsis leucopus",
  "Tyto tenebricosa",
  "Varanus varius"
)


# Leadbeater's possum - Gymnobelideus leadbeateri -------------------

vba_dat_ch %>%
  filter(species == species_list[1]) %$%
  table(survey_method, PA)

vba_dat_ch %>%
  filter(species == species_list[1]) %$%
  unique(survey_method)


sm_gyle <- c(
  "Camera - Surveillance/Remote",
  "Camera - Thermal imaging",
  "Nest box",
  "Owl census",
  "Spotlighting on foot",
  "Spotlighting",
  "Stag watching"
)

# Greater Glider - Petauroides volans ------------------
  
  vba_dat_ch %>%
    filter(species == species_list[2]) %$%
    table(survey_method, PA)
  
  vba_dat_ch %>%
    filter(species == species_list[2]) %$%
    unique(survey_method)

sm_pevo <- c(
  "Camera - Surveillance/Remote",
  "Camera - Thermal imaging",
  "Nest box",
  "Owl census",
  "Spotlighting on foot",
  "Spotlighting",
  "Stag watching"
)

# Yellow-bellied Glider - Petaurus Australis ------------------

vba_dat_ch %>%
  filter(species == species_list[3]) %$%
  table(survey_method, PA)

vba_dat_ch %>%
  filter(species == species_list[3]) %$%
  unique(survey_method)

sm_peau <- c(
  "Camera - Surveillance/Remote",
  "Camera - Thermal imaging",
  "Nest box",
  "Owl census",
  "Spotlighting on foot",
  "Spotlighting",
  "Stag watching"
)


# Long-footed Potoroo - Potorous longipes ------------------

# vba_dat_ch %>%
#   filter(species == species_list[4]) %$%
#   table(survey_method, PA)
# 
# vba_dat_ch %>%
#   filter(species == species_list[4]) %$%
#   unique(survey_method)
# 
# sm_pevo <- c(
#   "Camera - Surveillance/Remote"
# )

# White-footed dunnart - Sminthopsus leucopus ------------------

vba_dat_ch %>%
  filter(species == species_list[4]) %$%
  table(survey_method, PA)

vba_dat_ch %>%
  filter(species == species_list[4]) %$%
  unique(survey_method)

sm_smle <- c(
  "Camera - Surveillance/Remote",
  "Elliott trap"
)


# Sooty Owl - Tyto tenebricosa ------------------

vba_dat_ch %>%
  filter(species == species_list[5]) %$%
  table(survey_method, PA)

vba_dat_ch %>%
  filter(species == species_list[5]) %$%
  unique(survey_method)

sm_tyte <- c(
  "Bird count",
  "Bird transect",
  "Birds Australia 2ha search",
  "Birds Australia 500m area search",
  "Owl census",
  "Spotlighting on foot",
  "Spotlighting"
)

# Lace Monitor - Varanus varius ------------------

vba_dat_ch %>%
  filter(species == species_list[6]) %$%
  table(survey_method, PA)

vba_dat_ch %>%
  filter(species == species_list[6]) %$%
  unique(survey_method)

sm_vave <- c(
  "Camera - Surveillance/Remote",
  "Herp census - active",
  "Herp spot count",
  "Herp transect"
)

# Aggregate and get data --------------


pa_list <- tibble(
  species = species_list,
  survey_methods = list(
    sm_gyle,
    sm_pevo,
    sm_peau,
    sm_smle,
    sm_tyte,
    sm_vave
  )
) %>%
  mutate(
    gen = sub(
      pattern = " .*",
      replacement = "",
      x = species
    ) %>%
      tolower %>%
      substr(
        start = 1,
        stop = 2
      ),
    spe = sub(
      pattern = ".* ",
      replacement = "",
      x = species
    ) %>%
      substr(
        start = 1,
        stop = 2
      ),
    sp = paste0(
      gen,
      spe
    )
  ) %>%
  dplyr::select(
    species,
    sp,
    survey_methods
  )


#plan(multisession)

pad <- pa_list %$%
  #future_mapply(
  mapply(
    FUN = buff.sample.pa,
    species = species,
    survey_method = survey_methods,
    MoreArgs = list(
      x = vba_dat_ch,
      rfa = ch_rfa,
      cellsize = 200
    ),
    SIMPLIFY = FALSE
  )

#plan(sequential)


pa_data <- pa_list %>%
  bind_cols(
    tibble(
      pa_dat = pad
    )
  )

#pa_data


# Number of presences and absences for each species
lapply(
  X = pa_data$pa_dat,
  FUN = function(x){
    table(x$PA)
  }
)



# Write and save data --------------------------

pa_data %$%
  mapply(
    FUN = function(
      x,
      y
    ){
      st_write(
        obj = x,
        dsn = sprintf(
          "output/pa/pa_%s.shp",
          y
        ),
        delete_dsn = TRUE
      )
      
      st_write(
        obj = x,
        dsn = sprintf(
          "output/pa/pa_%s.csv",
          y
        ),
        layer_options = "GEOMETRY=AS_XY",
        delete_dsn = TRUE
      )
    },
    x = pa_dat,
    y = sp
  )



save(
  pa_data,
  file = "output/RData/02_species_occurrences.RData"
)

## ARI data ----

# lb_ari <- read_csv(file = "data/tabular/ari/Leadbeaters010319.txt") %>%
#   st_as_sf(coords = c("LONGITUDED", "LATITUDEDD"), crs = st_crs(28355)) %>%
#   st_transform(crs = ch_proj) %>%
#   mutate(date = dmy(SURVEY_STA))
#   dplyr::select(PA, date, geometry)



