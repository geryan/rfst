brtpredict <- function(variables,
                       model,
                       out_path = "output/habitat_pred",
                       scn_id,
                       varset,
                       species,
                       initial = TRUE,
                       ncores){
  
  library(gbm)
  library(raster)
  library(dismo)
  
  if(initial){
    
    result <- raster::predict(object = variables[[1]],
                              model = model,
                              type = "response",
                              n.trees = model$gbm.call$best.trees,
                              filename = sprintf("%s/brtpred_%s_%s_%s_000.grd",
                                                 out_path,
                                                 scn_id,
                                                 varset,
                                                 species),
                              overwrite = TRUE)
    
    names(result) <- "sdm_0"
    
    
  } else {
    
    #library(doMC)
    #library(foreach)
    
    #registerDoMC(cores = ncores)
    
    result <- foreach(i = seq_len(length(variables)), .packages = c("gbm", "raster", "dismo")) %do% {
      
      raster::predict(object = variables[[i]],
                      model = model,
                      type = "response",
                      n.trees = model$gbm.call$best.trees,
                      filename = sprintf("%s/brtpred_%s_%s_%s_%03d.grd",
                                         out_path,
                                         scn_id,
                                         varset,
                                         species,
                                         i-1),
                      overwrite = TRUE)
    }
    
   result <- stack(result)
     
   names(result) <- sprintf("sdm_%s", 0:(length(variables) - 1))
  }
 
  return(result)
   
}