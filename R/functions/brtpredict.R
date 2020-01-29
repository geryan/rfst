brtpredict <- function(
  variables,
  model,
  out_path = "output/habitat_pred",
  scn_id,
  varset,
  species,
  initial = TRUE,
  ncores = 1
){
  
  library(gbm)
  library(raster)
  library(dismo)
  
  vnames <- model$var.names
  
  vn1 <- names(variables[[1]])
  
  vars <- lapply(
    X = variables,
    FUN = function(x, vn1){
      result <- x
      
      names(result) <- vn1
      
      return(result)
    },
    vn1 = vn1
  )
  
  predvars <- lapply(
    X = vars,
    FUN = function(x, vnames){
      x[[vnames]]
    },
    vnames = vnames
  )
  
  if(initial){
    
    result <- raster::predict(object = predvars[[1]],
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
    
    library(foreach)
  
    
    result <- foreach(i = seq_len(length(predvars)), .packages = c("gbm", "raster", "dismo")) %dopar% {
      
      raster::predict(object = predvars[[i]],
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