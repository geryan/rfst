static_path <- "/home/landis/landis_steps/data/"
landis_path <- "/home/landis/landis_ch_lbp/"
scevar_path <- "/home/landis/landis_steps/scenario_vars_lbp/"
proj_mask <- raster(x = "landis_steps/data/grids/ch_mask.tif")

make_landis_habitat_vars <- function(
  #static_path,
  landis_path,
  #scevar_path,
  sp_gg = TRUE,
  sp_lb = TRUE,
  region = "ch",
  proj_mask,
  
){

  gvrb <- function(img){
    getValues(raster(paste0(landis_path, "/output/biomass/", img)))
  }
  
  gvrc <- function(img){
    getValues(raster(paste0(landis_path, "/output/cohort-stats/", img)))
  }
  
  
  
  eco_rst <- raster(paste0(landis_path, "eco_v12.img"))
  
  ext <- extent(eco_rst)
  
  prj <- eco_rst@crs
  
  
  buffer_radius <- 564
  window <- focalWeight(raster(ncols=11, nrows=11, xmn=0, res = 100),
                        buffer_radius,
                        type = 'circle')
  
  if(sp_gg){
   biomass_tot <- mask
   biomass_tot[] <- gvrb("bio-TotalBiomass-0.img")
   biomass_tot <- biomass_tot * mask
   biomass_tot_vec <- getValues(biomass_tot)
   
   biomass_eucacype <- biomass_eucadalr <- biomass_eucadive <- biomass_eucaradi <- biomass_eucaregn <- biomass_eucavimi <- ch_rst
   
   biomass_eucacype[] <- gvrb("bio-eucacype-0.img")
   biomass_eucadalr[] <- gvrb("bio-eucadalr-0.img")
   biomass_eucadive[] <- gvrb("bio-eucadive-0.img")
   biomass_eucaradi[] <- gvrb("bio-eucaradi-0.img")
   biomass_eucaregn[] <- gvrb("bio-eucaregn-0.img")
   biomass_eucavimi[] <- gvrb("bio-eucavimi-0.img")
   
   biomass_eucs <- stack(biomass_eucacype, biomass_eucadalr, biomass_eucadive, biomass_eucaradi, biomass_eucavimi)
   
   biomass_eucs <- biomass_eucs * mask
   
   names(biomass_eucs) <- c("bio-eucacype", "bio-eucadalr", "bio-eucadive", "bio-eucaradi", "bio-eucavimi")
   
   
  }
  
  
  
  
}