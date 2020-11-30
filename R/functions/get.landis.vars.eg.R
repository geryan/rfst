get.landis.vars.eg <- function(
  scn_path,
  proj_path,
  out_path = "/output/habitat_vars/",
  scn_id,
  proj_mask,
  timesteps,
  cores = cores,
  harvest_timber = TRUE
){
  
  #library(doMC)
  library(raster)
  library(dplyr)
  library(foreach)
  
  source(file = "R/functions/rst.op.R")
  
  
  bmpath <- paste0(scn_path, "/output/biomass/")
  #cspath <- paste0(scn_path, "/output/cohort-stats/")
  mapath <- paste0(scn_path, "/output/max-spp-age/")
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
  
  win3h <- matrix(rep(1, 9), nrow = 3)
  
  
  #registerDoMC(cores = cores)
  
  #result <- foreach(j = 0:timesteps) %dopar% {
  result <- foreach(j = 0:timesteps) %do% {
    
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
    
    # max_age <- proj_mask
    # 
    # max_age[] <- paste0(cspath, "AGE-MAX-", j,".img") %>%
    #   raster %>%
    #   getValues
    
    # max_age <- paste0(cspath, "AGE-MAX-", j,".img") %>%
    #   raster %>%
    #   rst.op(op = "writeonly",
    #          proj_mask = proj_mask,
    #          filename = sprintf("%s/%s/%s_max_age_%03d.grd",
    #                             proj_path,
    #                             out_path,
    #                             scn_id,
    #                             j),
    #          layernames = "max_age")
    max_age <- paste0(mapath, "AllSppMaxAge-", j,".img") %>%
      raster %>%
      rst.op(op = "writeonly",
             proj_mask = proj_mask,
             filename = sprintf("%s/%s_max_age_%03d.grd",
                                out_path,
                                scn_id,
                                j),
             layernames = "max_age")
    ### INDICES
    
    #### MAX AGE
    
    prop_old_150 <- rst.op(input1 = max_age,
                           op = "lessthan",
                           proj_mask = proj_mask,
                           filename = sprintf("%s/%s_prop_old_150_%03d.grd",
                                              out_path,
                                              scn_id,
                                              j),
                           layernames = "prop_old_150",
                           window = win1k,
                           lessthan = 150)
    
    prop_old_200 <- rst.op(input1 = max_age,
                           op = "lessthan",
                           proj_mask = proj_mask,
                           filename = sprintf("%s/%s_prop_old_200_%03d.grd",
                                              out_path,
                                              scn_id,
                                              j),
                           layernames = "prop_old_200",
                           window = win1k,
                           lessthan = 200)
    
    
    #### CV TARGET EUCALYPTS
    # prop_bio_targ <- rst.op(input1 = stack(biomass_eucacype,
    #                                        biomass_eucadalr,
    #                                        biomass_eucadive,
    #                                        biomass_eucaradi,
    #                                        biomass_eucavimi),
    #                         input2 = biomass_TotalBiomass,
    #                         op = "prop",
    #                         proj_mask = proj_mask,
    #                         filename = sprintf("%s/%s_prop_bio_targ_%03d.grd",
    #                                            out_path,
    #                                            scn_id,
    #                                            j),
    #                         layernames = "prop_bio_targ")
    
    
    ###### CV REGNANS
    prop_bio_regn <- rst.op(input1 = biomass_eucaregn,
                            input2 = biomass_TotalBiomass,
                            op = "prop",
                            proj_mask = proj_mask,
                            filename = sprintf("%s/%s_prop_bio_regn_%03d.grd",
                                               out_path,
                                               scn_id,
                                               j),
                            layernames = "prop_bio_regn")
    
    ##### PROP OLD GROWTH EUCALYPTS
    
    prop_oge <- rst.op(input1 = stack(bioAge_eucadeli,
                                      bioAge_eucanite,
                                      bioAge_eucadent,
                                      bioAge_eucaregn,
                                      bioAge_eucafast,
                                      bioAge_eucasieb,
                                      bioAge_eucaglob,
                                      bioAge_eucamuel,
                                      bioAge_eucaobli,
                                      bioAge_eucacype,
                                      bioAge_eucavimi,
                                      bioAge_eucarubi,
                                      bioAge_eucadalr),
                       input2 = biomass_TotalBiomass,
                       op = "prop",
                       proj_mask = proj_mask,
                       filename = sprintf("%s/%s_prop_oge_%03d.grd",
                                          out_path,
                                          scn_id,
                                          j),
                       layernames = "prop_oge")
    
    prop_oge_3h <- rst.op(input1 = stack(bioAge_eucadeli,
                                         bioAge_eucanite,
                                         bioAge_eucadent,
                                         bioAge_eucaregn,
                                         bioAge_eucafast,
                                         bioAge_eucasieb,
                                         bioAge_eucaglob,
                                         bioAge_eucamuel,
                                         bioAge_eucaobli,
                                         bioAge_eucacype,
                                         bioAge_eucavimi,
                                         bioAge_eucarubi,
                                         bioAge_eucadalr),
                          input2 = biomass_TotalBiomass,
                          op = "prop",
                          proj_mask = proj_mask,
                          filename = sprintf("%s/%s_prop_oge_3h_%03d.grd",
                                             out_path,
                                             scn_id,
                                             j),
                          layernames = "prop_oge_3h",
                          window = win3h)
    
    prop_oge_1k <- rst.op(input1 = stack(bioAge_eucadeli,
                                         bioAge_eucanite,
                                         bioAge_eucadent,
                                         bioAge_eucaregn,
                                         bioAge_eucafast,
                                         bioAge_eucasieb,
                                         bioAge_eucaglob,
                                         bioAge_eucamuel,
                                         bioAge_eucaobli,
                                         bioAge_eucacype,
                                         bioAge_eucavimi,
                                         bioAge_eucarubi,
                                         bioAge_eucadalr),
                          input2 = biomass_TotalBiomass,
                          op = "prop",
                          proj_mask = proj_mask,
                          filename = sprintf("%s/%s_prop_oge_1k_%03d.grd",
                                             out_path,
                                             scn_id,
                                             j),
                          layernames = "prop_oge_1k",
                          window = win1k)
    
    
    ##### Biomass OLD GROWTH EUCALYPTS
    
    biom_oge <- rst.op(input1 = stack(bioAge_eucadeli,
                                      bioAge_eucanite,
                                      bioAge_eucadent,
                                      bioAge_eucaregn,
                                      bioAge_eucafast,
                                      bioAge_eucasieb,
                                      bioAge_eucaglob,
                                      bioAge_eucamuel,
                                      bioAge_eucaobli,
                                      bioAge_eucacype,
                                      bioAge_eucavimi,
                                      bioAge_eucarubi,
                                      bioAge_eucadalr),
                       op = "writeonly",
                       proj_mask = proj_mask,
                       filename = sprintf("%s/%s_biom_oge_%03d.grd",
                                          out_path,
                                          scn_id,
                                          j),
                       layernames = "biom_oge")
    
    biom_oge_3h <- rst.op(input1 = stack(bioAge_eucadeli,
                                         bioAge_eucanite,
                                         bioAge_eucadent,
                                         bioAge_eucaregn,
                                         bioAge_eucafast,
                                         bioAge_eucasieb,
                                         bioAge_eucaglob,
                                         bioAge_eucamuel,
                                         bioAge_eucaobli,
                                         bioAge_eucacype,
                                         bioAge_eucavimi,
                                         bioAge_eucarubi,
                                         bioAge_eucadalr),
                          op = "writeonly",
                          proj_mask = proj_mask,
                          filename = sprintf("%s/%s_biom_oge_3h_%03d.grd",
                                             out_path,
                                             scn_id,
                                             j),
                          layernames = "biom_oge_3h",
                          window = win3h)
    
    biom_oge_1k <- rst.op(input1 = stack(bioAge_eucadeli,
                                         bioAge_eucanite,
                                         bioAge_eucadent,
                                         bioAge_eucaregn,
                                         bioAge_eucafast,
                                         bioAge_eucasieb,
                                         bioAge_eucaglob,
                                         bioAge_eucamuel,
                                         bioAge_eucaobli,
                                         bioAge_eucacype,
                                         bioAge_eucavimi,
                                         bioAge_eucarubi,
                                         bioAge_eucadalr),
                          input2 = biomass_TotalBiomass,
                          op = "writeonly",
                          proj_mask = proj_mask,
                          filename = sprintf("%s/%s_biom_oge_1k_%03d.grd",
                                             out_path,
                                             scn_id,
                                             j),
                          layernames = "biom_oge_1k",
                          window = win1k)
    
    #### GG DEN
 

    ##### FIRE SEVERITY
    
    if(j == 0){
      
      firesev <- proj_mask
      
      names(firesev) <- "firesev"
      
      firesev[] <- 0
      
      firesev <- mask(x = firesev,
                      mask = proj_mask,
                      filename = sprintf("%s/%s_firesev_%03d.grd",
                                         out_path,
                                         scn_id,
                                         j),
                      overwrite = TRUE)
      
      
    } else {
      
      firesev <- paste0(fipath, "severity-", j,".img") %>%
        raster %>%
        rst.op(op = "sub1",
               proj_mask = proj_mask,
               filename = sprintf("%s/%s_firesev_%03d.grd",
                                  out_path,
                                  scn_id,
                                  j),
               layernames = "firesev")
    }
    
    
 
    #### HARVEST
    
    if(harvest_timber){
      
      if(j == 0){
        
        harvest <- proj_mask
        
        harvest[] <- 0
        
        names(harvest) <- "harvest"
        
        harvest <- mask(x = harvest,
                        mask = proj_mask,
                        filename = sprintf("%s/%s_harvest_%03d.grd",
                                           out_path,
                                           scn_id,
                                           j),
                        overwrite = TRUE)
        
        
      } else {
        
        harvest <- paste0(hapath, "prescripts-", j,".img") %>%
          raster %>%
          rst.op(op = "sub1",
                 proj_mask = proj_mask,
                 filename = sprintf("%s/%s_harvest_%03d.grd",
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
                      filename = sprintf("%s/%s_harvest_%03d.grd",
                                         out_path,
                                         scn_id,
                                         j),
                      overwrite = TRUE)
    }
    
    stack(prop_bio_regn,
          prop_old_150,
          prop_old_200,
          prop_oge,
          prop_oge_3h,
          prop_oge_1k,
          biom_oge,
          biom_oge_3h,
          biom_oge_1k,
          harvest,
          firesev,
          max_age
    )
  }
  
  return(result) 
}