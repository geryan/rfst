# 07 combine variables

library(dplyr)
library(raster)

load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")
load(file = "output/RData/03_LANDIS_variables.RData")
load(file = "output/RData/04_disturbance_variables.RData")
load(file = "output/RData/05_geophys_vars.RData")
load(file = "output/RData/06_climate_variables.RData")

source.functions("R/functions")


vn_lb_1 <- c("lbm_prop", "lbm_biom", "prop_old_150", "prop_oge_3h", "biom_oge_3h", "prec01", "prec07", "tmax01", "tmin07", "lvdaw", "lvdma", "lvdmi", "lvdsw", "ahr", "tho", "max_age")

vn_lb_2 <- c("prec01", "prec07", "tmax01", "tmin07", "lvdaw", "lvdma", "lvdmi", "lvdsw", "ahr", "tho", "max_age")

# AIM HERE IS TO USE THE BIOM_OGE PLAYERS WHEN PROCESSED

vn_gg_1 <- c( "ggf_prop", "ggf_biom", "ggd_prop_og", "ggd_biom_og","prop_old_150", "prop_oge_1k", "biom_oge_1k", "prec01", "prec07", "tmax01", "tmin07", "lvdaw", "lvdma", "lvdmi", "lvdsw", "ahr", "tho", "max_age")

vn_gg_2 <- c("prec01", "prec07", "tmax01", "tmin07", "lvdaw", "lvdma", "lvdmi", "lvdsw", "ahr", "tho", "max_age")



for (i in 1:length(scn_list)){
  for (j in 1:length(rep_list)){
    
    
    tx <- mapply(
      FUN = stack,
      get(
        sprintf(
          "lv_%s_%s",
          scn_list[i],
          rep_list[j])),
      get(
        sprintf(
          "dv_%s_%s",
          scn_list[i],
          rep_list[j])),
      gv,
      clim_vars_4.5
    )
    

    assign(
      x = sprintf("vars_%s_%s", scn_list[i], rep_list[j]),
      value = tx
    )
    
    lb1 <- mapply(
      FUN = raster::subset,
      tx,
      MoreArgs = list(
        subset = vn_lb_1
      )
    )

    assign(
      x = sprintf("vlb1_%s_%s", scn_list[i], rep_list[j]),
      value = lb1
    )

    lb2 <- mapply(
      FUN = raster::subset,
      tx,
      MoreArgs = list(
        subset = vn_lb_2
      )
    )

    assign(
      x = sprintf("vlb2_%s_%s", scn_list[i], rep_list[j]),
      value = lb2
    )

    gg1 <- mapply(
      FUN = raster::subset,
      tx,
      MoreArgs = list(
        subset = vn_gg_1
      )
    )

    assign(
      x = sprintf("vgg1_%s_%s", scn_list[i], rep_list[j]),
      value = gg1
    )

    gg2 <- mapply(
      FUN = raster::subset,
      tx,
      MoreArgs = list(
        subset = vn_gg_2
      )
    )

    assign(
      x = sprintf("vgg2_%s_%s", scn_list[i], rep_list[j]),
      value = gg2
    )

    
  }
}

save(
  vn_lb_1,
  vn_lb_2,
  vn_gg_1,
  vn_gg_2,
  list = c(
    ls()[grep("vars_", ls())],
    ls()[grep("vgg", ls())],
    ls()[grep("vlb", ls())]
           ),
  file = "output/RData/07_combined_variables.RData"
)
