get.landis.vars.init <- function(
  scn_path,
  proj_mask
  ){
  
  library(raster)
  library(dplyr)
  
  bmpath <- paste0(scn_path, "/output/biomass/")
  cspath <- paste0(scn_path, "/output/cohort-stats/")
  
  
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
  
  biomass_TotalBiomass_vec <- getValues(biomass_TotalBiomass)
  
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
  
  #### MAX AGE
  max_age_150 <- max_age
  max_age_150[max_age_150 < 150] <- 0
  max_age_150[max_age_150 != 0] <- 1
  
  max_age_200 <- max_age
  max_age_200[max_age_200 < 200] <- 0
  max_age_200[max_age_200 != 0] <- 1
  
  prop_old_150 <- focal(max_age_150, win1k, na.rm = TRUE)
  prop_old_200 <- focal(max_age_200, win1k, na.rm = TRUE)
  
  
  #### CV TARGET EUCALYPTS
  biomass_eucs <- biomass_eucs <- stack(biomass_eucacype, biomass_eucadalr, biomass_eucadive, biomass_eucaradi, biomass_eucavimi)
  
  biomass_eucs_mat <- getValues(biomass_eucs)
  
  prop_bio_targ <- proj_mask
  prop_bio_targ[] <- rowSums(biomass_eucs_mat)/biomass_TotalBiomass_vec
  
  prop_bio_regn <- proj_mask
  prop_bio_regn[] <- getValues(biomass_eucaregn)/biomass_TotalBiomass_vec
  
  
  #### HOLLOW BEARING TREE INDEX
  hbt <- proj_mask
  
  hbt[] <- getValues(max_age)
  hbt[hbt <= 150] <- 0
  hbt[hbt > 150 & hbt < 200] <- (hbt[hbt > 150 & hbt < 200] - 150)/50
  hbt[hbt >= 200] <- 1
  
  hbt_3h <- focal(hbt, win3h, na.rm = TRUE)
  hbt_1k <- focal(hbt, win1k, na.rm = TRUE)
  
  #### GG DEN
  
  ggdvec <- stack(biomass_eucacype,
                  biomass_eucadalr,
                  biomass_eucadive,
                  biomass_eucaradi,
                  biomass_eucaregn,
                  biomass_eucavimi) %>%
    getValues %>%
    rowSums
    
  ggd <- proj_mask
  
  ggd[] <- ggdvec/biomass_TotalBiomass_vec
  
  ggd <- focal(ggd, win1k, na.rm = TRUE)
  
  #### GG FORAGE
  
  ggfvec <- stack(biomass_eucacype,
                  biomass_eucaglob,
                  biomass_eucaobli,
                  biomass_eucatric) %>%
    getValues %>%
    rowSums
  
  ggf <- proj_mask
  
  ggf[] <- ggfvec/biomass_TotalBiomass_vec
  
  ggf <- focal(ggf, win1k, na.rm = TRUE)

    
  #### LB MID-STOREY
  
  lbmvec <- stack(biomass_acacdeal,
                  biomass_acacmear,
                  biomass_acacobli,
                  biomass_eucapauh,
                  biomass_eucapaul,
                  #biomass_eucaacer,
                  biomass_leptgran,
                  biomass_nothcunn) %>%
    getValues %>%
    rowSums
  
  lbm <- proj_mask
  
  lbm[] <- lbmvec/biomass_TotalBiomass_vec
  
  lbm <- focal(lbm, win3h, na.rm = TRUE)
  
  result <- stack(lbm,
                  ggf,
                  ggd,
                  hbt_3h,
                  hbt_1k,
                  prop_bio_regn,
                  prop_bio_targ,
                  prop_old_150,
                  prop_old_200)
  
  names(result) <- c("lbm",
                     "ggf",
                     "ggd",
                     "hbt_3h",
                     "hbt_1k",
                     "prop_bio_regn",
                     "prop_bio_targ",
                     "prop_old_150",
                     "prop_old_200")
  
  return(result) 
}