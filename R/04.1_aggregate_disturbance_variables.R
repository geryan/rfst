# 04.1 aggregate disturbance variables

library(dplyr)
library(purrr)
library(raster)
library(doMC)
library(foreach)

load(file = "output/RData/00_comp_controls.RData")
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

registerDoMC(cores = ncores)

advs <- foreach (i = 1:length(scn_list)) %:%
  foreach (j = 1:length(rep_list)) %dopar% {
    
    dv <- get(
      sprintf(
        "dv_%s_%s",
        scn_list[i],
        rep_list[j]))
    
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
          "output/dist_aggregated/ag.fire_%s_%s_%02d-%02d.grd",
          scn_list[i],
          rep_list[j],
          ints[k,1],
          ints[k,2]
        ),
        layernames = "ag.fi"
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
          "output/dist_aggregated/ag.harvest_%s_%s_%02d-%02d.grd",
          scn_list[i],
          rep_list[j],
          ints[k,1],
          ints[k,2]
        ),
        layernames = "ag.lo"
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
          "output/dist_aggregated/ag.pb_%s_%s_%02d-%02d.grd",
          scn_list[i],
          rep_list[j],
          ints[k,1],
          ints[k,2]
        ),
        layernames = "ag.pb"
      )
      
      return(ag.pb)
      
    }
    
    ag.pb <- stack(ag.pb)
    
    adv <- list()
    
  }


for (i in 1:length(scn_list)) {
  for (j in 1:length(rep_list)){
    
    assign(
      x = sprintf("adv_%s_%s",
                  scn_list[i],
                  rep_list[j]),
      value = advs[[i]][[j]]
    )
    
  }
}

save(
  list = ls()[grep("adv_", ls())],
  file = "output/RData/04.01_aggregated_disturbance.RData"
)