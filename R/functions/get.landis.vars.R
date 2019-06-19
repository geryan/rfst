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

    
    ###### Biomass and proportion biomass of each species
    
    for(i in 1:length(spbm)){
      
      assign(sprintf("biomass_%s",
                     spbm[i]),
             rst.op(input1 = sprintf("biomass_%s",
                                     spbm[i]),
                    op = "writeonly",
                    proj_mask = proj_mask,
                    filename = sprintf("%s/%s/%s_biomass_%s_%03d.grd",
                                       proj_path,
                                       out_path,
                                       scn_id,
                                       spbm[i],
                                       j),
                    layernames = sprintf("biomass_%s",
                                         spbm[i])))
      
      assign(sprintf("prop_biomass_%s",
                     spbm[i]),
             rst.op(input1 = sprintf("biomass_%s",
                                     spbm[i]),
                    input2 = biomass_TotalBiomass,
                    op = "prop",
                    proj_mask = proj_mask,
                    filename = sprintf("%s/%s/%s_prop_biomass_%s_%03d.grd",
                                       proj_path,
                                       out_path,
                                       scn_id,
                                       spbm[i],
                                       j),
                    layernames = sprintf("prop_biomass_%s",
                                         spbm[i])))
      
    }
    
    #### BIOMASS BY AGE and proportion of each species
    
    for(i in 1:length(spba)){
      
      assign(sprintf("bioAge_%s",
                     spba[i]),
             rst.op(input1 = sprintf("bioAge_%s",
                                     spba[i]),
                    op = "writeonly",
                    proj_mask = proj_mask,
                    filename = sprintf("%s/%s/%s_bioAge_%s_%03d.grd",
                                       proj_path,
                                       out_path,
                                       scn_id,
                                       spba[i],
                                       j),
                    layernames = sprintf("bioAge_%s",
                                         spba[i])))
      
      assign(sprintf("bioAge_%s",
                     spba[i]),
             rst.op(input1 = sprintf("bioAge_%s",
                                     spba[i]),
                    input2 = biomass_TotalBiomass,
                    op = "prop",
                    proj_mask = proj_mask,
                    filename = sprintf("%s/%s/%s_prop_bioAge_%s_%03d.grd",
                                       proj_path,
                                       out_path,
                                       scn_id,
                                       spba[i],
                                       j),
                    layernames = sprintf("bioAge_%s",
                                         spba[i])))
      
    }
    
    
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

    ggd <-  rst.op(input1 = stack(biomass_eucacype,
                                   biomass_eucadalr,
                                   biomass_eucadive,
                                   biomass_eucaradi,
                                   biomass_eucaregn,
                                   biomass_eucavimi),
                   input2 = biomass_TotalBiomass,
                   op = "prop",
                   proj_mask = proj_mask,
                   filename = sprintf("%s/%s/%s_ggd_%03d.grd",
                                      proj_path,
                                      out_path,
                                      scn_id,
                                      j),
                   layernames = "ggd",
                   window = win1k)

    #### GG FORAGE

    ggf <-  rst.op(input1 = stack(biomass_eucacype,
                                   biomass_eucaglob,
                                   biomass_eucaobli,
                                   biomass_eucatric),
                   input2 = biomass_TotalBiomass,
                   op = "prop",
                   proj_mask = proj_mask,
                   filename = sprintf("%s/%s/%s_ggf_%03d.grd",
                                      proj_path,
                                      out_path,
                                      scn_id,
                                      j),
                   layernames = "ggf",
                   window = win1k)

    #### LB MID-STOREY

    lbm <-  rst.op(input1 = stack(biomass_acacdeal,
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
                   filename = sprintf("%s/%s/%s_lbm_%03d.grd",
                                      proj_path,
                                      out_path,
                                      scn_id,
                                      j),
                   layernames = "lbm",
                   window = win3h)

    # ##### FIRE SEVERITY

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
          woody,
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
          biomass_acacdeal,
          biomass_acacmear,
          biomass_acacobli,
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
          prop_bioAge_eucacama,
          prop_bioAge_eucacype,
          prop_bioAge_eucadalr,
          prop_bioAge_eucadeli,
          prop_bioAge_eucadent,
          prop_bioAge_eucadive,
          prop_bioAge_eucaglob,
          prop_bioAge_eucanite,
          prop_bioAge_eucaobli,
          prop_bioAge_eucapauh,
          prop_bioAge_eucapaul,
          prop_bioAge_eucaradi,
          prop_bioAge_eucaregn,
          prop_bioAge_eucatric,
          prop_bioAge_eucavimi,
          prop_biomass_acacdeal,
          prop_biomass_acacmear,
          prop_biomass_acacobli,
          prop_biomass_eucacama,
          prop_biomass_eucacype,
          prop_biomass_eucadalr,
          prop_biomass_eucadeli,
          prop_biomass_eucadent,
          prop_biomass_eucadive,
          prop_biomass_eucaglob,
          prop_biomass_eucanite,
          prop_biomass_eucaobli,
          prop_biomass_eucapauh,
          prop_biomass_eucapaul,
          prop_biomass_eucaradi,
          prop_biomass_eucaregn,
          prop_biomass_eucatric,
          prop_biomass_eucavimi,
          prop_biomass_leptgran,
          prop_biomass_nothcunn
          )
  }

  return(result) 
}