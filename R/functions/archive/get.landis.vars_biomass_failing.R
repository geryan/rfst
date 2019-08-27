get.landis.vars2 <- function(
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



    #### OUTPUT LAYERS


    ### INDICES
    
    #### MAX AGE
    
    
    
    
    
    prop_old_150 <- rst.op(input1 = max_age,
                           op = "lessthan",
                           proj_mask = proj_mask,
                           filename = sprintf("%s/%s/%s_prop_old_150_%03d.grd",
                                              proj_path,
                                              out_path,
                                              scn_id,
                                              j),
                           layernames = "prop_old_150",
                           window = win1k,
                           lessthan = 150)
    
    prop_old_200 <- rst.op(input1 = max_age,
                           op = "lessthan",
                           proj_mask = proj_mask,
                           filename = sprintf("%s/%s/%s_prop_old_200_%03d.grd",
                                              proj_path,
                                              out_path,
                                              scn_id,
                                              j),
                           layernames = "prop_old_200",
                           window = win1k,
                           lessthan = 200)
    
    
    #### CV TARGET EUCALYPTS
    prop_bio_targ <- rst.op(input1 = stack(biomass_eucacype,
                                           biomass_eucadalr,
                                           biomass_eucadive,
                                           biomass_eucaradi,
                                           biomass_eucavimi),
                            input2 = biomass_TotalBiomass,
                            op = "prop",
                            proj_mask = proj_mask,
                            filename = sprintf("%s/%s/%s_prop_bio_targ_%03d.grd",
                                               proj_path,
                                               out_path,
                                               scn_id,
                                               j),
                            layernames = "prop_bio_targ")
    
    
    ###### CV REGNANS
    prop_bio_regn <- rst.op(input1 = biomass_eucaregn,
                            input2 = biomass_TotalBiomass,
                            op = "prop",
                            proj_mask = proj_mask,
                            filename = sprintf("%s/%s/%s_prop_bio_regn_%03d.grd",
                                               proj_path,
                                               out_path,
                                               scn_id,
                                               j),
                            layernames = "prop_bio_regn")
    
    ##### PROP OLD GROWTH EUCALYPTS
    
    prop_oge <- rst.op(input1 = stack(bioAge_eucacama,
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
                       input2 = biomass_TotalBiomass,
                       op = "prop",
                       proj_mask = proj_mask,
                       filename = sprintf("%s/%s/%s_prop_oge_%03d.grd",
                                          proj_path,
                                          out_path,
                                          scn_id,
                                          j),
                       layernames = "prop_oge")
    
    prop_oge_3h <- rst.op(input1 = stack(bioAge_eucacama,
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
                          input2 = biomass_TotalBiomass,
                          op = "prop",
                          proj_mask = proj_mask,
                          filename = sprintf("%s/%s/%s_prop_oge_3h_%03d.grd",
                                             proj_path,
                                             out_path,
                                             scn_id,
                                             j),
                          layernames = "prop_oge_3h",
                          window = win3h)
    
    prop_oge_1k <- rst.op(input1 = stack(bioAge_eucacama,
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
                          input2 = biomass_TotalBiomass,
                          op = "prop",
                          proj_mask = proj_mask,
                          filename = sprintf("%s/%s/%s_prop_oge_1k_%03d.grd",
                                             proj_path,
                                             out_path,
                                             scn_id,
                                             j),
                          layernames = "prop_oge_1k",
                          window = win1k)
    
    #### HOLLOW BEARING TREE INDEX
    hbt <- proj_mask
    
    hbt[] <- getValues(max_age)
    hbt[hbt < 150] <- 0
    hbt[hbt > 200] <- 1
    hbt[hbt > 1] <- (hbt[hbt > 1] - 150)/50
    
    
    hbt_3h <- rst.op(input1 = max_age,
                     op = "lessthanscale",
                     proj_mask = proj_mask,
                     filename = sprintf("%s/%s/%s_hbt_3h_%03d.grd",
                                        proj_path,
                                        out_path,
                                        scn_id,
                                        j),
                     layernames = "hbt_3h",
                     window = win3h,
                     lessthan = 150,
                     scaleto = 200)
    
    hbt_1k <- rst.op(input1 = max_age,
                     op = "lessthanscale",
                     proj_mask = proj_mask,
                     filename = sprintf("%s/%s/%s_hbt_1k_%03d.grd",
                                        proj_path,
                                        out_path,
                                        scn_id,
                                        j),
                     layernames = "hbt_1k",
                     window = win1k,
                     lessthan = 150,
                     scaleto = 200)
    
    
    #### GG DEN
    
    ggd_prop <-  rst.op(input1 = stack(biomass_eucacype,
                                       biomass_eucadalr,
                                       biomass_eucadive,
                                       biomass_eucaradi,
                                       biomass_eucaregn,
                                      biomass_eucavimi),
                        input2 = biomass_TotalBiomass,
                        op = "prop",
                        proj_mask = proj_mask,
                        filename = sprintf("%s/%s/%s_ggd_prop_%03d.grd",
                                           proj_path,
                                           out_path,
                                           scn_id,
                                           j),
                        layernames = "ggd_prop",
                        window = win1k)
    
    ggd_prop_og <-  rst.op(input1 = stack(bioAge_eucacype,
                                          bioAge_eucadalr,
                                          bioAge_eucadive,
                                          bioAge_eucaradi,
                                          bioAge_eucaregn,
                                          bioAge_eucavimi),
                           input2 = biomass_TotalBiomass,
                           op = "prop",
                           proj_mask = proj_mask,
                           filename = sprintf("%s/%s/%s_ggd_prop_og_%03d.grd",
                                              proj_path,
                                              out_path,
                                              scn_id,
                                              j),
                           layernames = "ggd_prop_og",
                           window = win1k)
    
    ggd_biom <-  rst.op(input1 = stack(biomass_eucacype,
                                       biomass_eucadalr,
                                       biomass_eucadive,
                                       biomass_eucaradi,
                                       biomass_eucaregn,
                                       biomass_eucavimi),
                        op = "writeonly",
                        proj_mask = proj_mask,
                        filename = sprintf("%s/%s/%s_ggd_biom_%03d.grd",
                                           proj_path,
                                           out_path,
                                           scn_id,
                                           j),
                        layernames = "ggd_biom",
                        window = win1k)
    
    ggd_biom_og <-  rst.op(input1 = stack(bioAge_eucacype,
                                          bioAge_eucadalr,
                                          bioAge_eucadive,
                                          bioAge_eucaradi,
                                          bioAge_eucaregn,
                                          bioAge_eucavimi),
                           op = "writeonly",
                           proj_mask = proj_mask,
                           filename = sprintf("%s/%s/%s_ggd_biom_og_%03d.grd",
                                              proj_path,
                                              out_path,
                                              scn_id,
                                              j),
                           layernames = "ggd_biom_og",
                           window = win1k)
    
    #### GG FORAGE

    ggf_prop <-  rst.op(input1 = stack(biomass_eucacype,
                                       biomass_eucaglob,
                                       biomass_eucaobli,
                                       biomass_eucatric,
                                       biomass_eucavimi,
                                       biomass_eucadalr,
                                       biomass_eucaradi),
                        input2 = biomass_TotalBiomass,
                        op = "prop",
                        proj_mask = proj_mask,
                        filename = sprintf("%s/%s/%s_ggf_prop_%03d.grd",
                                           proj_path,
                                           out_path,
                                           scn_id,
                                           j),
                        layernames = "ggf_prop",
                        window = win1k)

    ggf_biom <-  rst.op(input1 = stack(biomass_eucacype,
                                       biomass_eucaglob,
                                       biomass_eucaobli,
                                       biomass_eucatric,
                                       biomass_eucavimi,
                                       biomass_eucadalr,
                                       biomass_eucaradi),
                        op = "writeonly",
                        proj_mask = proj_mask,
                        filename = sprintf("%s/%s/%s_ggf_biom_%03d.grd",
                                           proj_path,
                                           out_path,
                                           scn_id,
                                           j),
                        layernames = "ggf_biom",
                        window = win1k)

    #### LB MID-STOREY

    lbm_prop <-  rst.op(input1 = stack(biomass_acacdeal,
                                       biomass_acacmear,
                                       biomass_acacobli,
                                       biomass_eucapauh,
                                       biomass_eucapaul,
                                       biomass_eucaacer,
                                       biomass_leptgran,
                                       biomass_nothcunn),
                        input2 = biomass_TotalBiomass,
                        op = "prop",
                        proj_mask = proj_mask,
                        filename = sprintf("%s/%s/%s_lbm_prop_%03d.grd",
                                           proj_path,
                                           out_path,
                                           scn_id,
                                           j),
                        layernames = "lbm_prop",
                        window = win3h)

    lbm_biom <-  rst.op(input1 = stack(biomass_acacdeal,
                                       biomass_acacmear,
                                       biomass_acacobli,
                                       biomass_eucapauh,
                                       biomass_eucapaul,
                                       biomass_eucaacer,
                                       biomass_leptgran,
                                       biomass_nothcunn),
                        op = "writeonly",
                        proj_mask = proj_mask,
                        filename = sprintf("%s/%s/%s_lbm_biom_%03d.grd",
                                           proj_path,
                                           out_path,
                                           scn_id,
                                           j),
                        layernames = "lbm_biom",
                        window = win3h)

    #### LB acacias

    ac_prop <-  rst.op(input1 = stack(biomass_acacdeal,
                                      biomass_acacmear,
                                      biomass_acacobli),
                       input2 = biomass_TotalBiomass,
                       op = "prop",
                       proj_mask = proj_mask,
                       filename = sprintf("%s/%s/%s_ac_prop_%03d.grd",
                                          proj_path,
                                          out_path,
                                          scn_id,
                                          j),
                       layernames = "ac_prop",
                       window = win3h)

    ac_biom <-  rst.op(input1 = stack(biomass_acacdeal,
                                      biomass_acacmear,
                                      biomass_acacobli),
                       op = "writeonly",
                       proj_mask = proj_mask,
                       filename = sprintf("%s/%s/%s_ac_biom_%03d.grd",
                                          proj_path,
                                          out_path,
                                          scn_id,
                                          j),
                       layernames = "ac_biom",
                       window = win3h)

    ##### FIRE SEVERITY
    
    if(j == 0){
      
      firesev <- proj_mask
      
      names(firesev) <- "firesev"
      
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
        rst.op(op = "sub1",
               proj_mask = proj_mask,
               filename = sprintf("%s/%s/%s_firesev_%03d.grd",
                                  proj_path,
                                  out_path,
                                  scn_id,
                                  j),
               layernames = "firesev")
    }
    
    
    ##### WOODY BIOMASS
    
    woody <- paste0(bmpath, "woody-", j,".img") %>%
      raster %>%
      rst.op(op = "writeonly",
             proj_mask = proj_mask,
             filename = sprintf("%s/%s/%s_woody_%03d.grd",
                                proj_path,
                                out_path,
                                scn_id,
                                j),
             layernames = "woody")
    
    #### COMBINE VARIABLES
    
    
    
    #### HARVEST
    
    if(harvest_timber){
      
      if(j == 0){
        
        harvest <- proj_mask
        
        harvest[] <- 0
        
        names(harvest) <- "harvest"
        
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
          rst.op(op = "sub1",
                 proj_mask = proj_mask,
                 filename = sprintf("%s/%s/%s_harvest_%03d.grd",
                                    proj_path,
                                    out_path,
                                    scn_id,
                                    j),
                 layernames = "harvest")
      }
    } else {
      
      harvest <- proj_mask
      
      harvest[] <- 0
      
      names(harvest) <- "harvest"
      
      harvest <- mask(x = harvest,
                      mask = proj_mask,
                      filename = sprintf("%s/%s/%s_harvest_%03d.grd",
                                         proj_path,
                                         out_path,
                                         scn_id,
                                         j),
                      overwrite = TRUE)
    }
    
    max_age <- rst.op(input1 = max_age,
                      op = "writeonly",
                      proj_mask = proj_mask,
                      filename = sprintf("%s/%s/%s_max_age_%03d.grd",
                                         proj_path,
                                         out_path,
                                         scn_id,
                                         j),
                      layernames = "max_age")
    
    biomass_acacdeal <- rst.op(input1 = biomass_acacdeal,
                               op = "writeonly",
                               proj_mask = proj_mask,
                               filename = sprintf("%s/%s/%s_biomass_acacdeal_%03d.grd",
                                                  proj_path,
                                                  out_path,
                                                  scn_id,
                                                  j),
                               layernames = "biomass_acacdeal")
    
    biomass_acacmear <- rst.op(input1 = biomass_acacmear,
                               op = "writeonly",
                               proj_mask = proj_mask,
                               filename = sprintf("%s/%s/%s_biomass_acacmear_%03d.grd",
                                                  proj_path,
                                                  out_path,
                                                  scn_id,
                                                  j),
                               layernames = "biomass_acacmear")
    
    biomass_acacobli <- rst.op(input1 = biomass_acacobli,
                               op = "writeonly",
                               proj_mask = proj_mask,
                               filename = sprintf("%s/%s/%s_biomass_acacobli_%03d.grd",
                                                  proj_path,
                                                  out_path,
                                                  scn_id,
                                                  j),
                               layernames = "biomass_acacobli")
    
    biomass_eucaacer <- rst.op(input1 = biomass_eucaacer,
                               op = "writeonly",
                               proj_mask = proj_mask,
                               filename = sprintf("%s/%s/%s_biomass_eucaacer_%03d.grd",
                                                  proj_path,
                                                  out_path,
                                                  scn_id,
                                                  j),
                               layernames = "biomass_eucaacer")
    
    biomass_eucacama <- rst.op(input1 = biomass_eucacama,
                               op = "writeonly",
                               proj_mask = proj_mask,
                               filename = sprintf("%s/%s/%s_biomass_eucacama_%03d.grd",
                                                  proj_path,
                                                  out_path,
                                                  scn_id,
                                                  j),
                               layernames = "biomass_eucacama")
    
    biomass_eucacype <- rst.op(input1 = biomass_eucacype,
                               op = "writeonly",
                               proj_mask = proj_mask,
                               filename = sprintf("%s/%s/%s_biomass_eucacype_%03d.grd",
                                                  proj_path,
                                                  out_path,
                                                  scn_id,
                                                  j),
                               layernames = "biomass_eucacype")
    
    biomass_eucadalr <- rst.op(input1 = biomass_eucadalr,
                               op = "writeonly",
                               proj_mask = proj_mask,
                               filename = sprintf("%s/%s/%s_biomass_eucadalr_%03d.grd",
                                                  proj_path,
                                                  out_path,
                                                  scn_id,
                                                  j),
                               layernames = "biomass_eucadalr")
    
    biomass_eucadeli <- rst.op(input1 = biomass_eucadeli,
                               op = "writeonly",
                               proj_mask = proj_mask,
                               filename = sprintf("%s/%s/%s_biomass_eucadeli_%03d.grd",
                                                  proj_path,
                                                  out_path,
                                                  scn_id,
                                                  j),
                               layernames = "biomass_eucadeli")
    
    biomass_eucadent <- rst.op(input1 = biomass_eucadent,
                               op = "writeonly",
                               proj_mask = proj_mask,
                               filename = sprintf("%s/%s/%s_biomass_eucadent_%03d.grd",
                                                  proj_path,
                                                  out_path,
                                                  scn_id,
                                                  j),
                               layernames = "biomass_eucadent")
    
    biomass_eucadive <- rst.op(input1 = biomass_eucadive,
                               op = "writeonly",
                               proj_mask = proj_mask,
                               filename = sprintf("%s/%s/%s_biomass_eucadive_%03d.grd",
                                                  proj_path,
                                                  out_path,
                                                  scn_id,
                                                  j),
                               layernames = "biomass_eucadive")
    
    biomass_eucaglob <- rst.op(input1 = biomass_eucaglob,
                               op = "writeonly",
                               proj_mask = proj_mask,
                               filename = sprintf("%s/%s/%s_biomass_eucaglob_%03d.grd",
                                                  proj_path,
                                                  out_path,
                                                  scn_id,
                                                  j),
                               layernames = "biomass_eucaglob")
    
    biomass_eucanite <- rst.op(input1 = biomass_eucanite,
                               op = "writeonly",
                               proj_mask = proj_mask,
                               filename = sprintf("%s/%s/%s_biomass_eucanite_%03d.grd",
                                                  proj_path,
                                                  out_path,
                                                  scn_id,
                                                  j),
                               layernames = "biomass_eucanite")
    
    biomass_eucaobli <- rst.op(input1 = biomass_eucaobli,
                               op = "writeonly",
                               proj_mask = proj_mask,
                               filename = sprintf("%s/%s/%s_biomass_eucaobli_%03d.grd",
                                                  proj_path,
                                                  out_path,
                                                  scn_id,
                                                  j),
                               layernames = "biomass_eucaobli")
    
    biomass_eucapauh <- rst.op(input1 = biomass_eucapauh,
                               op = "writeonly",
                               proj_mask = proj_mask,
                               filename = sprintf("%s/%s/%s_biomass_eucapauh_%03d.grd",
                                                  proj_path,
                                                  out_path,
                                                  scn_id,
                                                  j),
                               layernames = "biomass_eucapauh")
    
    biomass_eucapaul <- rst.op(input1 = biomass_eucapaul,
                               op = "writeonly",
                               proj_mask = proj_mask,
                               filename = sprintf("%s/%s/%s_biomass_eucapaul_%03d.grd",
                                                  proj_path,
                                                  out_path,
                                                  scn_id,
                                                  j),
                               layernames = "biomass_eucapaul")
    
    biomass_eucaradi <- rst.op(input1 = biomass_eucaradi,
                               op = "writeonly",
                               proj_mask = proj_mask,
                               filename = sprintf("%s/%s/%s_biomass_eucaradi_%03d.grd",
                                                  proj_path,
                                                  out_path,
                                                  scn_id,
                                                  j),
                               layernames = "biomass_eucaradi")
    
    biomass_eucaregn <- rst.op(input1 = biomass_eucaregn,
                               op = "writeonly",
                               proj_mask = proj_mask,
                               filename = sprintf("%s/%s/%s_biomass_eucaregn_%03d.grd",
                                                  proj_path,
                                                  out_path,
                                                  scn_id,
                                                  j),
                               layernames = "biomass_eucaregn")
    
    biomass_eucatric <- rst.op(input1 = biomass_eucatric,
                               op = "writeonly",
                               proj_mask = proj_mask,
                               filename = sprintf("%s/%s/%s_biomass_eucatric_%03d.grd",
                                                  proj_path,
                                                  out_path,
                                                  scn_id,
                                                  j),
                               layernames = "biomass_eucatric")
    
    biomass_eucavimi <- rst.op(input1 = biomass_eucavimi,
                               op = "writeonly",
                               proj_mask = proj_mask,
                               filename = sprintf("%s/%s/%s_biomass_eucavimi_%03d.grd",
                                                  proj_path,
                                                  out_path,
                                                  scn_id,
                                                  j),
                               layernames = "biomass_eucavimi")
    
    biomass_leptgran <- rst.op(input1 = biomass_leptgran,
                               op = "writeonly",
                               proj_mask = proj_mask,
                               filename = sprintf("%s/%s/%s_biomass_leptgran_%03d.grd",
                                                  proj_path,
                                                  out_path,
                                                  scn_id,
                                                  j),
                               layernames = "biomass_leptgran")
    
    biomass_nothcunn <- rst.op(input1 = biomass_nothcunn,
                               op = "writeonly",
                               proj_mask = proj_mask,
                               filename = sprintf("%s/%s/%s_biomass_nothcunn_%03d.grd",
                                                  proj_path,
                                                  out_path,
                                                  scn_id,
                                                  j),
                               layernames = "biomass_nothcunn")
    
    
    bioAge_eucaacer <- rst.op(input1 = bioAge_eucaacer,
                              op = "writeonly",
                              proj_mask = proj_mask,
                              filename = sprintf("%s/%s/%s_bioAge_eucaacer_%03d.grd",
                                                 proj_path,
                                                 out_path,
                                                 scn_id,
                                                 j),
                              layernames = "bioAge_eucaacer")
    
    bioAge_eucacama <- rst.op(input1 = bioAge_eucacama,
                              op = "writeonly",
                              proj_mask = proj_mask,
                              filename = sprintf("%s/%s/%s_bioAge_eucacama_%03d.grd",
                                                 proj_path,
                                                 out_path,
                                                 scn_id,
                                                 j),
                              layernames = "bioAge_eucacama")
    
    bioAge_eucacype <- rst.op(input1 = bioAge_eucacype,
                              op = "writeonly",
                              proj_mask = proj_mask,
                              filename = sprintf("%s/%s/%s_bioAge_eucacype_%03d.grd",
                                                 proj_path,
                                                 out_path,
                                                 scn_id,
                                                 j),
                              layernames = "bioAge_eucacype")
    
    bioAge_eucadalr <- rst.op(input1 = bioAge_eucadalr,
                              op = "writeonly",
                              proj_mask = proj_mask,
                              filename = sprintf("%s/%s/%s_bioAge_eucadalr_%03d.grd",
                                                 proj_path,
                                                 out_path,
                                                 scn_id,
                                                 j),
                              layernames = "bioAge_eucadalr")
    
    bioAge_eucadeli <- rst.op(input1 = bioAge_eucadeli,
                              op = "writeonly",
                              proj_mask = proj_mask,
                              filename = sprintf("%s/%s/%s_bioAge_eucadeli_%03d.grd",
                                                 proj_path,
                                                 out_path,
                                                 scn_id,
                                                 j),
                              layernames = "bioAge_eucadeli")
    
    bioAge_eucadent <- rst.op(input1 = bioAge_eucadent,
                              op = "writeonly",
                              proj_mask = proj_mask,
                              filename = sprintf("%s/%s/%s_bioAge_eucadent_%03d.grd",
                                                 proj_path,
                                                 out_path,
                                                 scn_id,
                                                 j),
                              layernames = "bioAge_eucadent")
    
    bioAge_eucadive <- rst.op(input1 = bioAge_eucadive,
                              op = "writeonly",
                              proj_mask = proj_mask,
                              filename = sprintf("%s/%s/%s_bioAge_eucadive_%03d.grd",
                                                 proj_path,
                                                 out_path,
                                                 scn_id,
                                                 j),
                              layernames = "bioAge_eucadive")
    
    bioAge_eucaglob <- rst.op(input1 = bioAge_eucaglob,
                              op = "writeonly",
                              proj_mask = proj_mask,
                              filename = sprintf("%s/%s/%s_bioAge_eucaglob_%03d.grd",
                                                 proj_path,
                                                 out_path,
                                                 scn_id,
                                                 j),
                              layernames = "bioAge_eucaglob")
    
    bioAge_eucanite <- rst.op(input1 = bioAge_eucanite,
                              op = "writeonly",
                              proj_mask = proj_mask,
                              filename = sprintf("%s/%s/%s_bioAge_eucanite_%03d.grd",
                                                 proj_path,
                                                 out_path,
                                                 scn_id,
                                                 j),
                              layernames = "bioAge_eucanite")
    
    bioAge_eucaobli <- rst.op(input1 = bioAge_eucaobli,
                              op = "writeonly",
                              proj_mask = proj_mask,
                              filename = sprintf("%s/%s/%s_bioAge_eucaobli_%03d.grd",
                                                 proj_path,
                                                 out_path,
                                                 scn_id,
                                                 j),
                              layernames = "bioAge_eucaobli")
    
    bioAge_eucapauh <- rst.op(input1 = bioAge_eucapauh,
                              op = "writeonly",
                              proj_mask = proj_mask,
                              filename = sprintf("%s/%s/%s_bioAge_eucapauh_%03d.grd",
                                                 proj_path,
                                                 out_path,
                                                 scn_id,
                                                 j),
                              layernames = "bioAge_eucapauh")
    
    bioAge_eucapaul <- rst.op(input1 = bioAge_eucapaul,
                              op = "writeonly",
                              proj_mask = proj_mask,
                              filename = sprintf("%s/%s/%s_bioAge_eucapaul_%03d.grd",
                                                 proj_path,
                                                 out_path,
                                                 scn_id,
                                                 j),
                              layernames = "bioAge_eucapaul")
    
    bioAge_eucaradi <- rst.op(input1 = bioAge_eucaradi,
                              op = "writeonly",
                              proj_mask = proj_mask,
                              filename = sprintf("%s/%s/%s_bioAge_eucaradi_%03d.grd",
                                                 proj_path,
                                                 out_path,
                                                 scn_id,
                                                 j),
                              layernames = "bioAge_eucaradi")
    
    bioAge_eucaregn <- rst.op(input1 = bioAge_eucaregn,
                              op = "writeonly",
                              proj_mask = proj_mask,
                              filename = sprintf("%s/%s/%s_bioAge_eucaregn_%03d.grd",
                                                 proj_path,
                                                 out_path,
                                                 scn_id,
                                                 j),
                              layernames = "bioAge_eucaregn")
    
    bioAge_eucatric <- rst.op(input1 = bioAge_eucatric,
                              op = "writeonly",
                              proj_mask = proj_mask,
                              filename = sprintf("%s/%s/%s_bioAge_eucatric_%03d.grd",
                                                 proj_path,
                                                 out_path,
                                                 scn_id,
                                                 j),
                              layernames = "bioAge_eucatric")
    
    bioAge_eucavimi <- rst.op(input1 = bioAge_eucavimi,
                              op = "writeonly",
                              proj_mask = proj_mask,
                              filename = sprintf("%s/%s/%s_bioAge_eucavimi_%03d.grd",
                                                 proj_path,
                                                 out_path,
                                                 scn_id,
                                                 j),
                              layernames = "bioAge_eucavimi")
    
    
    stack(lbm_prop,
          lbm_biom,
          ac_prop,
          ac_biom,
          ggf_prop,
          ggf_biom,
          ggd_prop,
          ggd_prop_og,
          ggd_biom,
          ggd_biom_og,
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
          woody,
          biomass_acacdeal,
          biomass_acacmear,
          biomass_acacobli,
          biomass_eucaacer,
          biomass_eucacama,
          biomass_eucacype,
          biomass_eucadalr,
          biomass_eucadeli,
          biomass_eucadent,
          biomass_eucadive,
          biomass_eucaglob,
          biomass_eucanite,
          biomass_eucaobli,
          biomass_eucapauh,
          biomass_eucapaul,
          biomass_eucaradi,
          biomass_eucaregn,
          biomass_eucatric,
          biomass_eucavimi,
          biomass_leptgran,
          biomass_nothcunn,
          bioAge_eucaacer,
          bioAge_eucacama,
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
          bioAge_eucavimi,
          max_age
          )
  }
  
  return(result) 
}