get.landis.vars <- function(
  scn_path,
  proj_mask,
  scn_name,
  out_path = "/output/habitat_vars",
  cores = 3
){
  
  library(doMC)
  library(raster)
  library(dplyr)
  
  bmpath <- paste0(scn_path, "/output/biomass/")
  cspath <- paste0(scn_path, "/output/cohort-stats/")
  
  oupath <- paste0(out_path, scn_name)
  
  win1k <- focalWeight(raster(ncols=11,
                              nrows=11,
                              xmn = 0,
                              resolution = 100),
                       d = 564,
                       type = 'circle')
  
  win3h <- focalWeight(raster(ncols=3,
                              nrows=3,
                              xmn = 0,
                              resolution = 100),
                       d = 300,
                       type = 'rectangle')

  
  ### GET BIOMASS LAYERS FOR EACH SPECIES
  spbm <- list.files(path = bmpath,
                     pattern = "bio-") %>%
    sub("bio-", "", .) %>%
    sub("-.*", "", .) %>%
    unique
  
  spbm
  
  for(i in 1:length(spbm)){
    
    tempraster <- proj_mask
    
    tempraster[] <- sprintf("%sbio-%s-0.img", bmpath, spbm[i]) %>%
      raster %>%
      getValues
    
    names(tempraster) <- spbm[i]
    
    
    assign(sprintf("biomass_%s", spbm[i]), tempraster)
  }
  
  # ### GET MAX AGE LAYERS FOR EACH SPECIES
  # spma <- list.files(path = cspath) %>%
  #   sub("-.*", "", .) %>%
  #   unique
  # 
  # spma <- spma[which(spma!= "AGE")]
  # spma <- spma[which(spma!= "SPP")]
  # 
  # for(i in 1:length(spma)){
  #   
  #   tempraster <- proj_mask
  #   
  #   tempraster[] <- sprintf("%s%s-MAX-0.img", cspath, spma[i]) %>%
  #     raster %>%
  #     getValues
  #   
  #   names(tempraster) <- spma[i]
  #   
  #   assign(sprintf("maxage_%s", spma[i]), tempraster)
  #   
  # }
  
  ### GET OVERALL MAX AGE
  
  max_age <- proj_mask
  
  max_age[] <- paste0(cspath, "AGE-MAX-0.img") %>%
    raster %>%
    getValues
  
  
  ### INDICES
  max_age_150 <- max_age
  max_age_150[max_age_150 < 150] <- 0
  max_age_150[max_age_150 != 0] <- 1
  
  max_age_200 <- max_age
  max_age_200[max_age_200 < 200] <- 0
  max_age_200[max_age_200 != 0] <- 1
  
  prop_old_150 <- focal(max_age_150, win1k, na.rm = TRUE)
  prop_old_200 <- focal(max_age_200, win1k, na.rm = TRUE)
}