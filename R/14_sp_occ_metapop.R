# 14 Species occurrence for metapopulation capacity models

source("R/spartan/spartan_settings.R")

library(magrittr)
library(raster)
library(sf)
library(readr)
library(readxl)
library(tidyr)
library(dplyr)
library(future)
library(future.apply)
library(lubridate)
library(ggplot2)


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
  cutoff.date = "2000-01-01"
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


vba_dat_ch <- vba_dat[ch_rfa,]

eg_rfa <- rfa %>%
  filter(NAME == "EAST GIPPSLAND")

vba_dat_eg <- vba_dat[eg_rfa, ]


# Presence-absence-pseudo-absence

species_list <- c(
  "Litoria aurea",
  "Litoria littlejohni",
  "Pseudophryne bibronii",
  "Pseudophryne dendyi",
  "Pseudophryne semimarmorata",
  "Accipiter novaehollandiae",
  "Calyptorhynchus lathami",
  "Cinclosoma punctatum",
  "Climacteris picumnus",
  "Lophoictinia isura",
  "Melanodryas cucullata",
  "Ninox connivens",
  "Ninox strenua",
  "Petroica rosea",
  "Pyrrholaemus sagittatus",
  "Tyto novaehollandiae",
  "Dasyurus maculatus maculatus",
  "Isoodon obesulus obesulus",
  "Mastacomys fuscus mordicus",
  "Myotis macropus",
  "Potorous tridactylus trisulcatus",
  "Pseudomys fumeus",
  "Eucalyptus mackintii",
  "Grevillea barklyana",
  "Leionema bilobum subsp. serrulatum",
  "Persoonia arborea",
  "Persoonia silvatica",
  "Phebalium squamulosum subsp. squamulosum",
  "Pomaderris discolor",
  "Tetratheca subaphylla",
  "Wittsteinia vacciniacea",
  "Zieria smithii subsp. smithii",
  "Morelia spilota spilota"
)


sl <- c(
  "liau",
  "lili",
  "psbi",
  "psde",
  "psse",
  "acno",
  "cala",
  "cipu",
  "clpi",
  "lois",
  "mecu",
  "nico",
  "nist",
  "pero",
  "pysa",
  "tyno",
  "dama",
  "isob",
  "mafu",
  "myma",
  "potr",
  "psfu",
  "euma",
  "grba",
  "lebi",
  "pear",
  "pesi",
  "phsq",
  "podi",
  "tesu",
  "wiva",
  "zism",
  "mosp"
)


# 01  Green and Golden Bell Frog  Litoria aurea ----
spn <- 1
print(species_list[spn])

# ggplot() +
#   geom_sf(
#     data = rfa,
#     aes(
#       col = NAME
#     )
#   ) +
#   geom_sf(
#     dat = vba_dat %>%
#       filter(species == species_list[spn])
#   )
# 
# print(species_list[spn])

print("CH")

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)

print("EG")

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)


sm_liau <- c(
  "Frog transect",
  "Frog census"
)

# 02  2Large Brown Tree Frog      Litoria littlejohni  ----
spn <- 2
print(species_list[spn])

# ggplot() +
#   geom_sf(
#     data = rfa,
#     aes(
#       col = NAME
#     )
#   ) +
#   geom_sf(
#     dat = vba_dat %>%
#       filter(species == species_list[spn])
#   )
# 
# print(species_list[spn])

print("CH")

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)

print("EG")

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)


sm_lili <- c(
  "Frog transect",
  "Frog census"
)

# 03  Bibron’s (Brown) Toadlet    Pseudophryne bibronii  ----
spn <- 3
print(species_list[spn])

# ggplot() +
#   geom_sf(
#     data = rfa,
#     aes(
#       col = NAME
#     )
#   ) +
#   geom_sf(
#     dat = vba_dat %>%
#       filter(species == species_list[spn])
#   )
# 
# print(species_list[spn])

print("CH")

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)

print("EG")

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)


sm_psbi <- c(
  NA
)

# 04  Dendy's Toadlet	            Pseudophryne dendyi ----
spn <- 4
print(species_list[spn])

# ggplot() +
#   geom_sf(
#     data = rfa,
#     aes(
#       col = NAME
#     )
#   ) +
#   geom_sf(
#     dat = vba_dat %>%
#       filter(species == species_list[spn])
#   )
# 
# print(species_list[spn])

print("CH")

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)

print("EG")

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)


sm_psde <- c(
  "Frog transect",
  "Frog census"
)

# 05  Southern Toadlet 	          Pseudophryne semimarmorata ----
spn <- 5
print(species_list[spn])

# ggplot() +
#   geom_sf(
#     data = rfa,
#     aes(
#       col = NAME
#     )
#   ) +
#   geom_sf(
#     dat = vba_dat %>%
#       filter(species == species_list[spn])
#   )
# 
# print(species_list[spn])

print("CH")

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)

print("EG")

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)


sm_psse <- c(
  "Frog transect",
  "Frog census"
)

# 06  Grey Goshawk 	              Accipiter novaehollandiae ----
spn <- 6
print(species_list[spn])

# ggplot() +
#   geom_sf(
#     data = rfa,
#     aes(
#       col = NAME
#     )
#   ) +
#   geom_sf(
#     dat = vba_dat %>%
#       filter(species == species_list[spn])
#   )
# 
# print(species_list[spn])

print("CH")

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)

print("EG")

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)


sm_acno <- c(
  "Bird count",
  "Bird transect",
  "Birds Australia 500m area search",
  "Birds Australia 2ha search",
  "Birds Australia 5km area search",
  "Birds Australia fixed route survey"
)

# 07  Glossy Black-Cockatoo	      Calyptorhynchus lathami ----
spn <- 7
print(species_list[spn])
# 
# ggplot() +
#   geom_sf(
#     data = rfa,
#     aes(
#       col = NAME
#     )
#   ) +
#   geom_sf(
#     dat = vba_dat %>%
#       filter(species == species_list[spn])
#   )
# 
# print(species_list[spn])

print("CH")

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)

print("EG")

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)


sm_cala <- c(
  "Bird count",
  "Bird transect",
  "Birds Australia 500m area search",
  "Birds Australia 2ha search",
  "Birds Australia 5km area search",
  "Birds Australia fixed route survey"
)

# 08  Spotted Quail-thrush 	      Cinclosoma punctatum ----
spn <- 8
print(species_list[spn])

ggplot() +
  geom_sf(
    data = rfa,
    aes(
      col = NAME
    )
  ) +
  geom_sf(
    dat = vba_dat %>%
      filter(species == species_list[spn])
  )

print(species_list[spn])

print("CH")

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)

print("EG")

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)


sm_cipu <- c(
  "Bird count",
  "Bird transect",
  "Birds Australia 500m area search",
  "Birds Australia 2ha search",
  "Birds Australia 5km area search",
  "Birds Australia fixed route survey"
)

# 09  Brown Treecreeper (SE ssp)	Climacteris picumnus ----
spn <- 9
print(species_list[spn])

# ggplot() +
#   geom_sf(
#     data = rfa,
#     aes(
#       col = NAME
#     )
#   ) +
#   geom_sf(
#     dat = vba_dat %>%
#       filter(species == species_list[spn])
#   )
# 
# print(species_list[spn])

print("CH")

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)

print("EG")

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)


sm_clpi <- c(
  "Bird count",
  "Bird transect",
  "Birds Australia 500m area search",
  "Birds Australia 2ha search",
  "Birds Australia 5km area search",
  "Birds Australia fixed route survey"
)

# 10  Square-tailed Kite	        Lophoictinia isura ----
spn <- 10
print(species_list[spn])

# ggplot() +
#   geom_sf(
#     data = rfa,
#     aes(
#       col = NAME
#     )
#   ) +
#   geom_sf(
#     dat = vba_dat %>%
#       filter(species == species_list[spn])
#   )
# 
# print(species_list[spn])

print("CH")

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)

print("EG")

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)


sm_lois <- c(
  "Bird count",
  "Bird transect",
  "Birds Australia 500m area search",
  "Birds Australia 2ha search",
  "Birds Australia 5km area search",
  "Birds Australia fixed route survey"
)

# 11  Hooded Robin	              Melanodryas cucullata ----
spn <- 11
print(species_list[spn])

# ggplot() +
#   geom_sf(
#     data = rfa,
#     aes(
#       col = NAME
#     )
#   ) +
#   geom_sf(
#     dat = vba_dat %>%
#       filter(species == species_list[spn])
#   )
# 
# print(species_list[spn])

print("CH")

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)

print("EG")

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)


sm_mecu <- c(
  "Bird count",
  "Bird transect",
  "Birds Australia 500m area search",
  "Birds Australia 2ha search",
  "Birds Australia 5km area search",
  "Birds Australia fixed route survey"
)

# 12  Barking Owl	                Ninox connivens connivens  ----
spn <- 12
print(species_list[spn])

# ggplot() +
#   geom_sf(
#     data = rfa,
#     aes(
#       col = NAME
#     )
#   ) +
#   geom_sf(
#     dat = vba_dat %>%
#       filter(species == species_list[spn])
#   )
# 
# print(species_list[spn])

print("CH")

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)

print("EG")

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)


sm_nico <- c(
  "Owl census",
  "Bird count",
  "Bird transect",
  "Birds Australia 500m area search",
  "Birds Australia 2ha search",
  "Birds Australia 5km area search",
  "Birds Australia fixed route survey"
)

# 13  Powerful Owl	              Ninox strenua ----
spn <- 13
print(species_list[spn])

# ggplot() +
#   geom_sf(
#     data = rfa,
#     aes(
#       col = NAME
#     )
#   ) +
#   geom_sf(
#     dat = vba_dat %>%
#       filter(species == species_list[spn])
#   )
# 
# print(species_list[spn])

print("CH")

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)

print("EG")

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)


sm_nist <- sm_nico <- c(
  "Owl census",
  "Bird count",
  "Bird transect",
  "Birds Australia 500m area search",
  "Birds Australia 2ha search",
  "Birds Australia 5km area search",
  "Birds Australia fixed route survey"
)

# 14  Rose robin	                Petroica rosea ----
spn <- 14
print(species_list[spn])

# ggplot() +
#   geom_sf(
#     data = rfa,
#     aes(
#       col = NAME
#     )
#   ) +
#   geom_sf(
#     dat = vba_dat %>%
#       filter(species == species_list[spn])
#   )
# 
# print(species_list[spn])

print("CH")

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)

print("EG")

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)


sm_pero <- c(
  "Bird count",
  "Bird transect",
  "Birds Australia 500m area search",
  "Birds Australia 2ha search",
  "Birds Australia 5km area search",
  "Birds Australia fixed route survey"
)

# 15  Speckled Warbler	          Pyrrholaemus sagittatus ----
spn <- 15
print(species_list[spn])

# ggplot() +
#   geom_sf(
#     data = rfa,
#     aes(
#       col = NAME
#     )
#   ) +
#   geom_sf(
#     dat = vba_dat %>%
#       filter(species == species_list[spn])
#   )
# 
# print(species_list[spn])

print("CH")

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)

print("EG")

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)


sm_pysa <- c(
  "Bird count",
  "Bird transect",
  "Birds Australia 500m area search",
  "Birds Australia 2ha search",
  "Birds Australia 5km area search",
  "Birds Australia fixed route survey"
)

# 16  Masked Owl	                Tyto novaehollandiae  ----
spn <- 16
print(species_list[spn])

# ggplot() +
#   geom_sf(
#     data = rfa,
#     aes(
#       col = NAME
#     )
#   ) +
#   geom_sf(
#     dat = vba_dat %>%
#       filter(species == species_list[spn])
#   )
# 
# print(species_list[spn])

print("CH")

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)

print("EG")

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)


sm_tyno <- c(
  "Owl census",
  "Bird count",
  "Bird transect",
  "Birds Australia 500m area search",
  "Birds Australia 2ha search",
  "Birds Australia 5km area search",
  "Birds Australia fixed route survey"
)

# 17  Spot-tailed Quoll	          Dasyurus maculatus maculatus  ----
spn <- 17
print(species_list[spn])

# ggplot() +
#   geom_sf(
#     data = rfa,
#     aes(
#       col = NAME
#     )
#   ) +
#   geom_sf(
#     dat = vba_dat %>%
#       filter(species == species_list[spn])
#   )
# 
# print(species_list[spn])

print("CH")

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)

print("EG")

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)


sm_dama <- c(
  "Camera - Surveillance/Remote"
)

# 18  Southern Brown Bandicoot	  Isoodon obesulus obesulus  ----
spn <- 18
print(species_list[spn])

# ggplot() +
#   geom_sf(
#     data = rfa,
#     aes(
#       col = NAME
#     )
#   ) +
#   geom_sf(
#     dat = vba_dat %>%
#       filter(species == species_list[spn])
#   )
# 
# print(species_list[spn])

print("CH")

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)

print("EG")

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)


sm_isob <- c(
  "Camera - Surveillance/Remote",
  "Large hair tubes",
  "Faunatech hair tubes(bait type in X)"
)

# 19  Broad-toothed Rat	          Mastacomys fuscus mordicus ----
spn <- 19
print(species_list[spn])

# ggplot() +
#   geom_sf(
#     data = rfa,
#     aes(
#       col = NAME
#     )
#   ) +
#   geom_sf(
#     dat = vba_dat %>%
#       filter(species == species_list[spn])
#   )
# 
# print(species_list[spn])

print("CH")

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)

print("EG")

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)


sm_mafu <- NA

# 20  Large-footed Myotis	        Myotis macropus ----
spn <- 20
print(species_list[spn])

# ggplot() +
#   geom_sf(
#     data = rfa,
#     aes(
#       col = NAME
#     )
#   ) +
#   geom_sf(
#     dat = vba_dat %>%
#       filter(species == species_list[spn])
#   )
# 
# print(species_list[spn])

print("CH")

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)

print("EG")

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)


sm_myma <- c(
  "Bat trap",
  "Bat detector"
)

# 21  Long-nosed Potoroo	        Potorous tridactylus trisulcatus ----
spn <- 21
print(species_list[spn])

# ggplot() +
#   geom_sf(
#     data = rfa,
#     aes(
#       col = NAME
#     )
#   ) +
#   geom_sf(
#     dat = vba_dat %>%
#       filter(species == species_list[spn])
#   )
# 
# print(species_list[spn])

print("CH")

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)

print("EG")

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)


sm_potr <- c(
  "Camera - Surveillance/Remote",
  "Large hair tubes"
)

# 22  Smoky Mouse	                Pseudomys fumeus ----
spn <- 22
print(species_list[spn])

# ggplot() +
#   geom_sf(
#     data = rfa,
#     aes(
#       col = NAME
#     )
#   ) +
#   geom_sf(
#     dat = vba_dat %>%
#       filter(species == species_list[spn])
#   )
# 
# print(species_list[spn])

print("CH")

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)

print("EG")

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)


sm_psfu <- sm_isob <- c(
  "Camera - Surveillance/Remote",
  "Faunatech hair tubes(bait type in X)"
)

# 23  Gippsland Stringybark	      Eucalyptus mackintii ----
spn <- 23
print(species_list[spn])

# ggplot() +
#   geom_sf(
#     data = rfa,
#     aes(
#       col = NAME
#     )
#   ) +
#   geom_sf(
#     dat = vba_dat %>%
#       filter(species == species_list[spn])
#   )
# 
# print(species_list[spn])

print("CH")

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)

print("EG")

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)


sm_euma <- c(
  "Quadrat"
)

# 24  Gully Grevillea	            Grevillea barklyana ----
spn <- 24
print(species_list[spn])

# ggplot() +
#   geom_sf(
#     data = rfa,
#     aes(
#       col = NAME
#     )
#   ) +
#   geom_sf(
#     dat = vba_dat %>%
#       filter(species == species_list[spn])
#   )
# 
# print(species_list[spn])

print("CH")

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)

print("EG")

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)


sm_grba <- c(
  "Quadrat"
)

# 25  Toothed Leionema	          Leionema bilobum subsp. serrulatum----
spn <- 25
print(species_list[spn])

# ggplot() +
#   geom_sf(
#     data = rfa,
#     aes(
#       col = NAME
#     )
#   ) +
#   geom_sf(
#     dat = vba_dat %>%
#       filter(species == species_list[spn])
#   )
# 
# print(species_list[spn])

print("CH")

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)

print("EG")

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)


sm_lebi <- c(
  "Quadrat"
)

# 26  Tree Geebung	              Persoonia arborea ----
spn <- 26
print(species_list[spn])

# ggplot() +
#   geom_sf(
#     data = rfa,
#     aes(
#       col = NAME
#     )
#   ) +
#   geom_sf(
#     dat = vba_dat %>%
#       filter(species == species_list[spn])
#   )

print(species_list[spn])

print("CH")

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)

print("EG")

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)


sm_pear <- c(
  "Quadrat"
)

# 27  Forest Geebung	            Persoonia silvatica ----
spn <- 27
print(species_list[spn])

# ggplot() +
#   geom_sf(
#     data = rfa,
#     aes(
#       col = NAME
#     )
#   ) +
#   geom_sf(
#     dat = vba_dat %>%
#       filter(species == species_list[spn])
#   )
# 
# print(species_list[spn])

print("CH")

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)

print("EG")

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)


sm_pesi <- c(
  "Quadrat"
)

# 28  Forest Phebalium	          Phebalium squamulosum squamulosum ----
spn <- 28
print(species_list[spn])

# ggplot() +
#   geom_sf(
#     data = rfa,
#     aes(
#       col = NAME
#     )
#   ) +
#   geom_sf(
#     dat = vba_dat %>%
#       filter(species == species_list[spn])
#   )
# 
# print(species_list[spn])

print("CH")

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)

print("EG")

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)


sm_phsq <- c(
  "Quadrat"
)

# 29  Eastern Pomaderris	        Pomaderris discolor ----
spn <- 29
print(species_list[spn])

# ggplot() +
#   geom_sf(
#     data = rfa,
#     aes(
#       col = NAME
#     )
#   ) +
#   geom_sf(
#     dat = vba_dat %>%
#       filter(species == species_list[spn])
#   )
# 
# print(species_list[spn])

print("CH")

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)

print("EG")

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)


sm_podi <- c(
  "Quadrat"
)

# 30  Leafless Pink-bells	        Tetratheca subaphylla ----
spn <- 30
print(species_list[spn])

# ggplot() +
#   geom_sf(
#     data = rfa,
#     aes(
#       col = NAME
#     )
#   ) +
#   geom_sf(
#     dat = vba_dat %>%
#       filter(species == species_list[spn])
#   )
# 
# print(species_list[spn])

print("CH")

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)

print("EG")

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)


sm_tesu <- c(
  "Quadrat"
)

# 31  Baw Baw Berry	              Wittsteinia vacciniacea ----
spn <- 31
print(species_list[spn])

# ggplot() +
#   geom_sf(
#     data = rfa,
#     aes(
#       col = NAME
#     )
#   ) +
#   geom_sf(
#     dat = vba_dat %>%
#       filter(species == species_list[spn])
#   )
# 
# print(species_list[spn])

print("CH")

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)

print("EG")

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)


sm_wiva <- c(
  "Quadrat"
)

# 32  Sandfly Zieria	            Zieria smithii smithii ----
spn <- 32
print(species_list[spn])

# ggplot() +
#   geom_sf(
#     data = rfa,
#     aes(
#       col = NAME
#     )
#   ) +
#   geom_sf(
#     dat = vba_dat %>%
#       filter(species == species_list[spn])
#   )
# 
# print(species_list[spn])

print("CH")

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)

print("EG")

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)


sm_zism <- c(
  "Quadrat"
)

# 33  Diamond Python	            Morelia spilota spilota ----
spn <- 33
print(species_list[spn])

# ggplot() +
#   geom_sf(
#     data = rfa,
#     aes(
#       col = NAME
#     )
#   ) +
#   geom_sf(
#     dat = vba_dat %>%
#       filter(species == species_list[spn])
#   )
# 
# print(species_list[spn])

print("CH")

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_ch %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)

print("EG")

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  table(survey_method, PA) %>%
  print %>%
  sum

vba_dat_eg %>%
  filter(species == species_list[spn]) %$%
  unique(survey_method)


sm_mosp <- c(
  "Herp census - active",
  "Herp transect - passive",
  "Herp spot count",
  "Herp transect"
)


# Aggregate and get data --------------

species_list_ch <- c(
  #"Litoria aurea",
  #"Litoria littlejohni",
  #"Pseudophryne bibronii",
  #"Pseudophryne dendyi",
  "Pseudophryne semimarmorata",
  "Accipiter novaehollandiae",
  #"Calyptorhynchus lathami",
  "Cinclosoma punctatum",
  #"Climacteris picumnus",
  "Lophoictinia isura",
  #"Melanodryas cucullata",
  "Ninox connivens",
  "Ninox strenua",
  "Petroica rosea",
  #"Pyrrholaemus sagittatus",
  "Tyto novaehollandiae",
  #"Dasyurus maculatus maculatus",
  "Isoodon obesulus obesulus",
  #"Mastacomys fuscus mordicus",
  "Myotis macropus",
  #"Potorous tridactylus trisulcatus",
  "Pseudomys fumeus",
  #"Eucalyptus mackintii",
  "Grevillea barklyana",
  "Leionema bilobum subsp. serrulatum",
  "Persoonia arborea",
  #"Persoonia silvatica",
  #"Phebalium squamulosum subsp. squamulosum",
  #"Pomaderris discolor",
  "Tetratheca subaphylla",
  "Wittsteinia vacciniacea"#,
  #"Zieria smithii subsp. smithii",
  #"Morelia spilota spilota"
)

species_list_eg <- c(
  "Litoria aurea",
  "Litoria littlejohni",
  #"Pseudophryne bibronii",
  "Pseudophryne dendyi",
  #"Pseudophryne semimarmorata",
  "Accipiter novaehollandiae",
  "Calyptorhynchus lathami",
  "Cinclosoma punctatum",
  "Climacteris picumnus",
  "Lophoictinia isura",
  "Melanodryas cucullata",
  #"Ninox connivens",
  "Ninox strenua",
  "Petroica rosea",
  "Pyrrholaemus sagittatus",
  "Tyto novaehollandiae",
  "Dasyurus maculatus maculatus",
  "Isoodon obesulus obesulus",
  #"Mastacomys fuscus mordicus",
  #"Myotis macropus",
  "Potorous tridactylus trisulcatus",
  #"Pseudomys fumeus",
  "Eucalyptus mackintii",
  #"Grevillea barklyana",
  #"Leionema bilobum subsp. serrulatum",
  #"Persoonia arborea",
  #"Persoonia silvatica",
  #"Phebalium squamulosum subsp. squamulosum",
  #"Pomaderris discolor",
  "Tetratheca subaphylla",
  #"Wittsteinia vacciniacea",
  #"Zieria smithii subsp. smithii",
  "Morelia spilota spilota"
)


sm_list <- list(
  sm_liau,
  sm_lili,
  sm_psbi,
  sm_psde,
  sm_psse,
  sm_acno,
  sm_cala,
  sm_cipu,
  sm_clpi,
  sm_lois,
  sm_mecu,
  sm_nico,
  sm_nist,
  sm_pero,
  sm_pysa,
  sm_tyno,
  sm_dama,
  sm_isob,
  sm_mafu,
  sm_myma,
  sm_potr,
  sm_psfu,
  sm_euma,
  sm_grba,
  sm_lebi,
  sm_pear,
  sm_pesi,
  sm_phsq,
  sm_podi,
  sm_tesu,
  sm_wiva,
  sm_zism,
  sm_mosp
)

sm_list_ch <- list(
  #sm_liau,
  #sm_lili,
  #sm_psbi,
  #sm_psde,
  sm_psse,
  sm_acno,
  #sm_cala,
  sm_cipu,
  #sm_clpi,
  sm_lois,
  #sm_mecu,
  sm_nico,
  sm_nist,
  sm_pero,
  #sm_pysa,
  sm_tyno,
  #sm_dama,
  sm_isob,
  #sm_mafu,
  sm_myma,
  #sm_potr,
  sm_psfu,
  #sm_euma,
  sm_grba,
  sm_lebi,
  sm_pear,
  #sm_pesi,
  #sm_phsq,
  #sm_podi,
  sm_tesu,
  sm_wiva#,
  #sm_zism,
  #sm_mosp
)

sm_list_eg <- list(
  sm_liau,
  sm_lili,
  #sm_psbi,
  sm_psde,
  #sm_psse,
  sm_acno,
  sm_cala,
  sm_cipu,
  sm_clpi,
  sm_lois,
  sm_mecu,
  #sm_nico,
  sm_nist,
  sm_pero,
  sm_pysa,
  sm_tyno,
  sm_dama,
  sm_isob,
  #sm_mafu,
  #sm_myma,
  sm_potr,
  #sm_psfu,
  sm_euma,
  #sm_grba,
  #sm_lebi,
  #sm_pear,
  #sm_pesi,
  #sm_phsq,
  #sm_podi,
  sm_tesu,
  #sm_wiva,
  #sm_zism,
  sm_mosp
)

pa_list_ch <- tibble(
  species = species_list_ch,
  survey_methods = sm_list_ch
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

pa_list_eg <- tibble(
  species = species_list_eg,
  survey_methods = sm_list_eg
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





plan(multisession)

pad_ch <- pa_list_ch %$%
  future_mapply(
  #mapply(
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

plan(sequential)


pa_data_ch <- pa_list_ch %>%
  bind_cols(
    tibble(
      pa_dat = pad_ch
    )
  )

#pa_data_ch


# Number of presences and absences for each species
lapply(
  X = pa_data_ch$pa_dat,
  FUN = function(x){
    table(x$PA)
  }
)



# Write and save data --------------------------

pa_data_ch %$%
  mapply(
    FUN = function(
      x,
      y
    ){
      st_write(
        obj = x,
        dsn = sprintf(
          "output/pa/pa_ch_%s.shp",
          y
        ),
        delete_dsn = TRUE
      )
      
      st_write(
        obj = x,
        dsn = sprintf(
          "output/pa/pa_ch_%s.csv",
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
  pa_data_ch,
  file = "output/RData/14_sp_occ_metapop.RData"
)

