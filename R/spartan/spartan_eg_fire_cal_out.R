

source("R/spartan/spartan_settings.R")

library(dplyr)
library(tibble)
library(raster)
library(rgeos)
library(sf)
library(tidyr)

source("R/functions/source.functions.R")
source.functions("R/functions")


ntimesteps <- 12

proj_path <- "/data/gpfs/projects/punim1340/eg_fc_out/"

###


eg_mask <- raster(x = "/data/gpfs/projects/punim1340/eg_fire_calibration/EG19_fc_3a_0/Ecoregion_EG.img") %>%
  round.extent

#eg_mask <- raster(x = "junk/Ecoregion_EG.img")


eg_mask[!is.na(eg_mask)] <- 1

eg_mask <- mask(x = eg_mask,
                mask = eg_mask,
                filename = sprintf(
                  "%s/eg_mask.grd",
                  proj_path
                ),
                overwrite = TRUE)


###
  
  
dl <- list.dirs(
  path = "/data/gpfs/projects/punim1340/eg_fire_calibration/",
  recursive = FALSE
)


scn_id <- sub(".*//", "", dl)

fi <- mapply(
  FUN = get.landis.fire,
  scn_path = dl,
  scn_id = scn_id,
  MoreArgs = list(
    proj_path = proj_path,
    out_path = "",
    proj_mask = eg_mask,
    timesteps = ntimesteps
  ),
  SIMPLIFY = FALSE
)


saveRDS(
  object = fi,
  file = sprintf(
    "%s/fire_sevs.Rds",
    proj_path
  )
)

# fi <- readRDS(
#   file = sprintf(
#     "%s/fire_sevs.Rds",
#     proj_path
#   )
# )


fip <- lapply(
  X = fi,
  FUN = function(z){
    result <- lapply(
      X = z,
      FUN = function(x){
        result <- rasterToPolygons(
          x = x,
          fun = function(y){y > 0},
          dissolve = TRUE
        )
        
        if(!is.null(result)){
          result <- result %>%
            st_as_sf %>%
            st_cast(to = "POLYGON")
        }
        
        return(result)
      }
    )
    
    return(result)
  }
)


saveRDS(
  object = fip,
  file = sprintf(
    "%s/fire_polys.Rds",
    proj_path
  )
)

# fip <- readRDS(
#   file = sprintf(
#     "%s/fire_polys.Rds",
#     proj_path
#   )
# )


fia <- lapply(
  X = fip,
  FUN = function(y){
    lapply(
      X = y,
      FUN = function(x){
        if(is.null(x)){
          NA
        }else{
          st_area(x)
        }
      }
    )
  }
)

saveRDS(
  object = fia,
  file = sprintf(
    "%s/fire_area_list.Rds",
    proj_path
  )
)


# fia <- readRDS(
#   file = sprintf(
#     "%s/fire_area_list.Rds",
#     proj_path
#   )
# )



fiat <- lapply(
  X = fia,
  FUN = function(x){
    rbind(x) %>%
      t %>%
      as_tibble %>%
      mutate(
        yr = 1:length(x)
      )
  }
) %>%
  rbind %>%
  t %>%
  as_tibble %>%
  mutate(
    rep = names(fia)
  ) %>%
  mutate(
    rep = sub(
      pattern = ".*/",
      replacement = "",
      x = rep
    )
  )

names(fiat) <- c("dat", "rep")

fire_area <- fiat %>%
  unnest(dat) %>%
  unnest(x) %>%
  rename(area_m2 = x) %>%
  mutate(area_ha = area_m2/10000) %>%
  filter(!is.na(area_ha))

saveRDS(
  object = fire_area,
  file = sprintf(
    "%s/fire_area.Rds",
    proj_path
  )
)


# fire_area <- readRDS(
#   file = sprintf(
#     "%s/fire_area.Rds",
#     proj_path
#   )
# )
