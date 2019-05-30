get.landis.vars <- function(
  scn_path,
  proj_path,
  out_path = "/output/habitat_vars/",
  scn_id,
  proj_mask,
  timesteps,
  cores = cores,
  harvest_timber = TRUE
  ){
  
  library(doMC)
  library(raster)
  library(dplyr)
  
  bmpath <- paste0(scn_path, "/output/biomass/")
  cspath <- paste0(scn_path, "/output/cohort-stats/")
  hapath <- paste0(scn_path, "/harvest/")
  fipath <- paste0(scn_path, "/fire/")
  bapath <- paste0(scn_path, "/output/biomassAge/")
  
  win1k <- focalWeight(raster(ncols=11,
                              nrows=11,
                              xmn = 0,
                              resolution = 100),
                       d = 564,
                       type = 'circle')
  
  win1k <- ifelse(win1k > 0, 1, 0)
  
  win3h <- focalWeight(raster(ncols=3,
                              nrows=3,
                              xmn = 0,
                              resolution = 100),
                       d = 300,
                       type = 'rectangle')
  
  win3h <- ifelse(win3h > 0, 1, 0)
  

  registerDoMC(cores = cores)
  
  result <- foreach(j = 0:timesteps) %dopar% {
    
    #### BIOMASS
    
    spbm <- list.files(path = bmpath,
                       pattern = "bio-") %>%
      sub("bio-", "", .) %>%
      sub("-.*", "", .) %>%
      unique
    
    for(i in 1:length(spbm)){
      
      tempraster <- proj_mask
      
      tempraster[] <- sprintf("%s/bio-%s-%s.img", bmpath, spbm[i], j) %>%
        raster %>%
        getValues
      
      names(tempraster) <- spbm[i]
      
      
      assign(sprintf("biomass_%s", spbm[i]), tempraster)
    }
    
    #### BIOMASS BY AGE
    
    spba <- list.files(path = bapath) %>%
      sub("-.*", "", .) %>%
      unique
    
    for(i in 1:length(spba)){
      
      tempraster <- proj_mask
      
      tempraster[] <- sprintf("%s/%s-ageclass-%s.img", bapath, spba[i], j) %>%
        raster %>%
        getValues
      
      names(tempraster) <- spba[i]
      
      
      assign(sprintf("bioAge_%s", spba[i]), tempraster)
    }
    
    
    ### GET OVERALL MAX AGE
    
    max_age <- proj_mask
    
    max_age[] <- paste0(cspath, "AGE-MAX-", j,".img") %>%
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
    
    prop_old_150 <- focal(max_age_150,
                          win1k,
                          fun = mean,
                          na.rm = TRUE,
                          filename = sprintf("%s/%s/%s_prop_old_150_%03d.grd",
                                             proj_path,
                                             out_path,
                                             scn_id,
                                             j),
                          overwrite = TRUE)
    names(prop_old_150) <- "prop_old_150"
    
    prop_old_200 <- focal(max_age_200,
                          win1k,
                          fun = mean,
                          na.rm = TRUE,
                          filename = sprintf("%s/%s/%s_prop_old_200_%03d.grd",
                                             proj_path,
                                             out_path,
                                             scn_id,
                                             j),
                          overwrite = TRUE)
    names(prop_old_200) <- "prop_old_200"
    
    
    #### CV TARGET EUCALYPTS
    prop_bio_targ <- get.rst.prop(inputs = stack(biomass_eucacype,
                                                 biomass_eucadalr,
                                                 biomass_eucadive,
                                                 biomass_eucaradi,
                                                 biomass_eucavimi),
                                  total = biomass_TotalBiomass,
                                  proj_mask = proj_mask,
                                  filename = sprintf("%s/%s/%s_prop_bio_targ_%03d.grd",
                                                     proj_path,
                                                     out_path,
                                                     scn_id,
                                                     j))
    names(prop_bio_targ) <- "prop_bio_targ"
    
    
    ###### CV REGNANS
    prop_bio_regn <- get.rst.prop(inputs = biomass_eucaregn,
                                  total = biomass_TotalBiomass,
                                  proj_mask = proj_mask,
                                  filename = sprintf("%s/%s/%s_prop_bio_regn_%03d.grd",
                                                     proj_path,
                                                     out_path,
                                                     scn_id,
                                                     j))
    names(prop_bio_regn) <- "prop_bio_regn"
    
    ##### PROP OLD GROWTH EUCALYPTS
    
    prop_oge <- get.rst.prop(inputs = stack(bioAge_eucacama,
                                            bioAge_eucacype,
                                            bioAge_eucadalr,
                                            bioAge_eucadeli,
                                            bioAge_eucadent,
                                            bioAge_eucadive,
                                            bioAge_eucaglob,
                                            bioAge_eucanite,
                                            bioAge_eucaobli,
                                            bioAge_eucapauh,
                                            bioAge_eucapaul,
                                            bioAge_eucaradi,
                                            bioAge_eucaregn,
                                            bioAge_eucatric,
                                            bioAge_eucavimi),
                                  total = biomass_TotalBiomass,
                                  proj_mask = proj_mask,
                                  filename = sprintf("%s/%s/%s_prop_oge_%03d.grd",
                                                     proj_path,
                                                     out_path,
                                                     scn_id,
                                                     j))
    names(prop_oge) <- "prop_oge"
    
    prop_oge_3h <- get.rst.prop(inputs = stack(bioAge_eucacama,
                                            bioAge_eucacype,
                                            bioAge_eucadalr,
                                            bioAge_eucadeli,
                                            bioAge_eucadent,
                                            bioAge_eucadive,
                                            bioAge_eucaglob,
                                            bioAge_eucanite,
                                            bioAge_eucaobli,
                                            bioAge_eucapauh,
                                            bioAge_eucapaul,
                                            bioAge_eucaradi,
                                            bioAge_eucaregn,
                                            bioAge_eucatric,
                                            bioAge_eucavimi),
                             total = biomass_TotalBiomass,
                             proj_mask = proj_mask,
                             filename = sprintf("%s/%s/%s_prop_oge_3h_%03d.grd",
                                                proj_path,
                                                out_path,
                                                scn_id,
                                                j),
                             window = win3h)
    names(prop_oge_3h) <- "prop_oge_3h"
    
    prop_oge_1k <- get.rst.prop(inputs = stack(bioAge_eucacama,
                                            bioAge_eucacype,
                                            bioAge_eucadalr,
                                            bioAge_eucadeli,
                                            bioAge_eucadent,
                                            bioAge_eucadive,
                                            bioAge_eucaglob,
                                            bioAge_eucanite,
                                            bioAge_eucaobli,
                                            bioAge_eucapauh,
                                            bioAge_eucapaul,
                                            bioAge_eucaradi,
                                            bioAge_eucaregn,
                                            bioAge_eucatric,
                                            bioAge_eucavimi),
                             total = biomass_TotalBiomass,
                             proj_mask = proj_mask,
                             filename = sprintf("%s/%s/%s_prop_oge_1k_%03d.grd",
                                                proj_path,
                                                out_path,
                                                scn_id,
                                                j),
                             window = win1k)
    names(prop_oge_1k) <- "prop_oge_1k"
    
    
    
    #### HOLLOW BEARING TREE INDEX
    hbt <- proj_mask
    
    hbt[] <- getValues(max_age)
    hbt[hbt < 150] <- 0
    hbt[hbt > 200] <- 1
    hbt[hbt > 1] <- (hbt[hbt > 1] - 150)/50
    
    hbt_3h <- focal(hbt,
                    win3h,
                    fun = mean,
                    na.rm = TRUE,
                    filename = sprintf("%s/%s/%s_hbt_3h_%03d.grd",
                                       proj_path,
                                       out_path,
                                       scn_id,
                                       j),
                    overwrite = TRUE)
    names(hbt_3h) <- "hbt_3h"
    
    hbt_1k <- focal(hbt,
                    win1k,
                    fun = mean,
                    na.rm = TRUE,
                    filename = sprintf("%s/%s/%s_hbt_1k_%03d.grd",
                                       proj_path,
                                       out_path,
                                       scn_id,
                                       j),
                    overwrite = TRUE)
    names(hbt_1k) <- "hbt_1k"
    
    #### GG DEN
    
    ggd <-  get.rst.prop(inputs = stack(biomass_eucacype,
                                        biomass_eucadalr,
                                        biomass_eucadive,
                                        biomass_eucaradi,
                                        biomass_eucaregn,
                                        biomass_eucavimi),
                         total = biomass_TotalBiomass,
                         proj_mask = proj_mask,
                         filename = sprintf("%s/%s/%s_ggd_%03d.grd",
                                            proj_path,
                                            out_path,
                                            scn_id,
                                            j),
                         window = win1k)
    names(ggd) <- "ggd"
    
    #### GG FORAGE
    
    ggf <-  get.rst.prop(inputs = stack(biomass_eucacype,
                                        biomass_eucaglob,
                                        biomass_eucaobli,
                                        biomass_eucatric),
                         total = biomass_TotalBiomass,
                         proj_mask = proj_mask,
                         filename = sprintf("%s/%s/%s_ggf_%03d.grd",
                                            proj_path,
                                            out_path,
                                            scn_id,
                                            j),
                         window = win1k)
    names(ggf) <- "ggf"
    
    #### LB MID-STOREY
    
    lbm <- get.rst.prop(inputs = stack(biomass_acacdeal,
                                       biomass_acacmear,
                                       biomass_acacobli,
                                       biomass_eucapauh,
                                       biomass_eucapaul,
                                       biomass_eucaacer,
                                       biomass_leptgran,
                                       biomass_nothcunn),
                        total = biomass_TotalBiomass,
                        proj_mask = proj_mask,
                        filename = sprintf("%s/%s/%s_LBM_%03d.grd",
                                           proj_path,
                                           out_path,
                                           scn_id,
                                           j),
                        window = win3h)
    names(lbm) <- "lbm"
    

    # ##### FIRE SEVERITY

    if(j == 0){

      firesev <- proj_mask

      firesev[] <- 0

      firesev <- mask(x = firesev,
                      mask = proj_mask,
                      filename = sprintf("%s/%s/%s_firesev_%03d.grd",
                                         proj_path,
                                         out_path,
                                         scn_id,
                                         j),
                      overwrite = TRUE)


    } else {

      firesev <- paste0(fipath, "severity-", j,".img") %>%
        raster %>%
        get.rst.dat(proj_mask = proj_mask,
                    filename = sprintf("%s/%s/%s_firesev_%03d.grd",
                                       proj_path,
                                       out_path,
                                       scn_id,
                                       j),
                    sub1 = TRUE)
    }

    names(firesev) <- "firesev"


    ##### WOODY BIOMASS

    woody <- paste0(bmpath, "woody-", j,".img") %>%
      raster %>%
      get.rst.dat(proj_mask = proj_mask,
                  filename = sprintf("%s/%s/%s_woody_%03d.grd",
                                     proj_path,
                                     out_path,
                                     scn_id,
                                     j))

    names(woody) <- "woody"
    
    #### COMBINE VARIABLES
    
    
    
    #### HARVEST
    
    if(harvest_timber){
      
      if(j == 0){
        
        harvest <- proj_mask
        
        harvest[] <- 0
        
        harvest <- mask(x = harvest,
                        mask = proj_mask,
                        filename = sprintf("%s/%s/%s_harvest_%03d.grd",
                                           proj_path,
                                           out_path,
                                           scn_id,
                                           j),
                        overwrite = TRUE)
        
        
      } else {
        
        harvest <- paste0(hapath, "prescripts-", j,".img") %>%
          raster %>%
          get.rst.dat(proj_mask = proj_mask,
                      filename = sprintf("%s/%s/%s_harvest_%03d.grd",
                                         proj_path,
                                         out_path,
                                         scn_id,
                                         j),
                      sub1 = TRUE)
      }
      
      names(harvest) <- "harvest"
    } else {
      harvest <- proj_mask
      
      harvest[] <- 0
      
      harvest <- mask(x = harvest,
                      mask = proj_mask,
                      filename = sprintf("%s/%s/%s_harvest_%03d.grd",
                                         proj_path,
                                         out_path,
                                         scn_id,
                                         j),
                      overwrite = TRUE)
      names(harvest) <- "harvest"
    }
      

    
    stack(lbm,
          ggf,
          ggd,
          hbt_3h,
          hbt_1k,
          prop_bio_regn,
          prop_bio_targ,
          prop_old_150,
          prop_old_200,
          prop_oge,
          prop_oge_3h,
          prop_oge_1k,
          harvest,
          firesev,
          woody)
  }

  return(result) 
}