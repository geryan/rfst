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
  "Melanodryas cucullata cucullata",
  "Ninox connivens connivens",
  "Ninox strenua",
  "Petroica rosea",
  "Pyrrholaemus sagittatus",
  "Tyto novaehollandiae novaehollandiae",
  "Dasyurus maculatus maculatus",
  "Isoodon obesulus obesulus",
  "Mastacomys fuscus mordicus",
  "Myotis macropus",
  "Potorous tridactylus tridactylus",
  "Pseudomys fumeus",
  "Eucalyptus mackintii",
  "Grevillea barklyana",
  "Leionema bilobum",
  "Persoonia arborea",
  "Persoonia silvatica",
  "Phebalium squamulosum squamulosum",
  "Pomaderris discolor",
  "Tetratheca subaphylla",
  "Wittsteinia vacciniacea",
  "Zieria smithii smithii",
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


sm_lois <- c(
  "Bird count",
  "Bird transect",
  "Birds Australia 500m area search",
  "Birds Australia 2ha search",
  "Birds Australia 5km area search",
  "Birds Australia fixed route survey"
)

# 11  Hooded Robin	              Melanodryas cucullata cucullata ----
spn <- 1
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


sm_gyle <- c(
  "Camera - Surveillance/Remote",
  "Camera - Thermal imaging"
)

# 12  Barking Owl	                Ninox connivens connivens  ----
spn <- 1
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


sm_gyle <- c(
  "Camera - Surveillance/Remote",
  "Camera - Thermal imaging"
)

# 13  Powerful Owl	              Ninox strenua ----
spn <- 1
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


sm_gyle <- c(
  "Camera - Surveillance/Remote",
  "Camera - Thermal imaging"
)

# 14  Rose robin	                Petroica rosea ----
spn <- 1
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


sm_gyle <- c(
  "Camera - Surveillance/Remote",
  "Camera - Thermal imaging"
)

# 15  Speckled Warbler	          Pyrrholaemus sagittatus ----
spn <- 1
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


sm_gyle <- c(
  "Camera - Surveillance/Remote",
  "Camera - Thermal imaging"
)

# 16  Masked Owl	                Tyto novaehollandiae novaehollandiae  ----
spn <- 1
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


sm_gyle <- c(
  "Camera - Surveillance/Remote",
  "Camera - Thermal imaging"
)

# 18  Spot-tailed Quoll	          Dasyurus maculatus maculatus  ----
spn <- 1
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


sm_gyle <- c(
  "Camera - Surveillance/Remote",
  "Camera - Thermal imaging"
)

# 19  Southern Brown Bandicoot	  Isoodon obesulus obesulus  ----
spn <- 1
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


sm_gyle <- c(
  "Camera - Surveillance/Remote",
  "Camera - Thermal imaging"
)

# 20  Broad-toothed Rat	          Mastacomys fuscus mordicus ----
spn <- 1
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


sm_gyle <- c(
  "Camera - Surveillance/Remote",
  "Camera - Thermal imaging"
)

# 21  Large-footed Myotis	        Myotis macropus ----
spn <- 1
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


sm_gyle <- c(
  "Camera - Surveillance/Remote",
  "Camera - Thermal imaging"
)

# 22  Long-nosed Potoroo	        Potorous tridactylus tridactylus ----
spn <- 1
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


sm_gyle <- c(
  "Camera - Surveillance/Remote",
  "Camera - Thermal imaging"
)

# 23  Smoky Mouse	                Pseudomys fumeus ----
spn <- 1
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


sm_gyle <- c(
  "Camera - Surveillance/Remote",
  "Camera - Thermal imaging"
)

# 24  Gippsland Stringybark	      Eucalyptus mackintii ----
spn <- 1
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


sm_gyle <- c(
  "Camera - Surveillance/Remote",
  "Camera - Thermal imaging"
)

# 25  Gully Grevillea	            Grevillea barklyana ----
spn <- 1
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


sm_gyle <- c(
  "Camera - Surveillance/Remote",
  "Camera - Thermal imaging"
)

# 26  Toothed Leionema	          Leionema bilobum ----
spn <- 1
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


sm_gyle <- c(
  "Camera - Surveillance/Remote",
  "Camera - Thermal imaging"
)

# 27  Tree Geebung	              Persoonia arborea ----
spn <- 1
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


sm_gyle <- c(
  "Camera - Surveillance/Remote",
  "Camera - Thermal imaging"
)

# 28  Forest Geebung	            Persoonia silvatica ----
spn <- 1
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


sm_gyle <- c(
  "Camera - Surveillance/Remote",
  "Camera - Thermal imaging"
)

# 29  Forest Phebalium	          Phebalium squamulosum squamulosum ----
spn <- 1
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


sm_gyle <- c(
  "Camera - Surveillance/Remote",
  "Camera - Thermal imaging"
)

# 30  Eastern Pomaderris	        Pomaderris discolor ----
spn <- 1
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


sm_gyle <- c(
  "Camera - Surveillance/Remote",
  "Camera - Thermal imaging"
)

# 31  Leafless Pink-bells	        Tetratheca subaphylla ----
spn <- 1
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


sm_gyle <- c(
  "Camera - Surveillance/Remote",
  "Camera - Thermal imaging"
)

# 32  Baw Baw Berry	              Wittsteinia vacciniacea ----
spn <- 1
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


sm_gyle <- c(
  "Camera - Surveillance/Remote",
  "Camera - Thermal imaging"
)

# 33  Sandfly Zieria	            Zieria smithii smithii ----
spn <- 1
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


sm_gyle <- c(
  "Camera - Surveillance/Remote",
  "Camera - Thermal imaging"
)

# 34  Diamond Python	            Morelia spilota spilota ----
spn <- 1
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


sm_gyle <- c(
  "Camera - Surveillance/Remote",
  "Camera - Thermal imaging"
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



# save(
#   pa_data,
#   file = "output/RData/02_species_occurrences.RData"
# )

