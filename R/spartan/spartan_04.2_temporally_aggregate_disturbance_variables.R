# 04.1 aggregate disturbance variables


source("R/spartan/spartan_settings.R")

library(dplyr)
library(purrr)
library(raster)
#library(doMC)
library(foreach)

load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")
load(file = "output/RData/04_disturbance_variables.RData")

source.functions("R/functions")

# ----------------------

ints <- matrix(
  data = c(
    1,  10,
    11, 20,
    21, 30,
    31, 40,
    41, 50
  ),
  byrow = TRUE,
  nrow = 5
)

nints <- 5

intnames <- c("19-28","29-38","39-48","49-58","59-68")


command_args <- commandArgs(trailingOnly = TRUE)

i <- as.numeric(command_args[1])


dv <- disturbance_variables$dist_vars[[i]]


    ag.fire <- foreach(k = 1:nints) %do% {
      
      ag.fire <- dv[ints[k,1]:ints[k,2]] %>%
        map(
          .f = `[[`,
          "fi"
        ) %>%
        stack %>%
        sum
      
      ag.fire[ag.fire > 0] <- 1
      
      
      ag.fire <- rst.op(
        input1 = ag.fire,
        op = "writeonly",
        proj_mask = ch_mask,
        filename = sprintf(
          "output/dist_aggregated/ag.fire_%s_%02d-%02d.grd",
          disturbance_variables$scn_id[i],
          ints[k,1],
          ints[k,2]
        ),
        layernames = sprintf("fire_20%s", intnames[k])
      )
      
      return(ag.fire)
      
    }
    
    ag.fire <- stack(ag.fire)
    
    
  
    ag.harvest <- foreach(k = 1:nints) %do% {
      
      ag.harvest <- dv[ints[k,1]:ints[k,2]] %>%
        map(
          .f = `[[`,
          "lo"
        ) %>%
        stack %>%
        sum
      
      ag.harvest[ag.harvest > 0] <- 1
      
      
      ag.harvest <- rst.op(
        input1 = ag.harvest,
        op = "writeonly",
        proj_mask = ch_mask,
        filename = sprintf(
          "output/dist_aggregated/ag.harvest_%s_%02d-%02d.grd",
          disturbance_variables$scn_id[i],
          ints[k,1],
          ints[k,2]
        ),
        layernames = sprintf("harvest_20%s", intnames[k])
      )
      
      return(ag.harvest)
      
    }
    
    ag.harvest <- stack(ag.harvest)
    
    
    ag.pb <- foreach(k = 1:nints) %do% {
      
      ag.pb <- dv[ints[k,1]:ints[k,2]] %>%
        map(
          .f = `[[`,
          "pb"
        ) %>%
        stack %>%
        sum
      
      ag.pb[ag.pb > 0] <- 1
      
      
      ag.pb <- rst.op(
        input1 = ag.pb,
        op = "writeonly",
        proj_mask = ch_mask,
        filename = sprintf(
          "output/dist_aggregated/ag.pb_%s_%02d-%02d.grd",
          disturbance_variables$scn_id[i],
          ints[k,1],
          ints[k,2]
        ),
        layernames = sprintf("PB_20%s", intnames[k])
      )
      
      return(ag.pb)
      
    }
    
    ag.pb <- stack(ag.pb)
    
    
    
ag_dist_vars <- tibble(
  fire_ag = list(ag.fire),
  harvest_ag = list(ag.harvest),
  pb_ag = list(ag.pb)
)

ag_dist <- bind_cols(
  disturbance_variables[i,] %>%
    dplyr::select(
      -landis_vars,
      -dist_vars
    ),
  ag_dist_vars
)


saveRDS(
  object = ag_dist,
  file = sprintf(
    "%s/ag_dist_%s.Rds",
    "/data/gpfs/projects/punim0995/rfst/output/spartan_RData/ag_dist",
    disturbance_variables$scn_id[i]
  )
)


    
  