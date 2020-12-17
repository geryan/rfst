brtpredict <- function(
  variables,
  model,
  out_path = "output/habitat_pred",
  scn_id,
  varset,
  species,
  initial = TRUE,
  pll = FALSE,
  ncores = 1,
  write = TRUE
){
  
  library(gbm)
  library(raster)
  library(dismo)
  library(foreach)
  
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
    
    if(write){
    
        result <- raster::predict(object = predvars[[1]],
                                model = model,
                                type = "response",
                                n.trees = model$gbm.call$best.trees,
                                filename = sprintf("%s/brt_initial_pred_%s_%s_%s.grd",
                                                   out_path,
                                                   scn_id,
                                                   varset,
                                                   species),
                                overwrite = TRUE)
      
      names(result) <- "sdm_00"
      
    } else {
      
      result <- raster::predict(
        object = predvars[[1]],
        model = model,
        type = "response",
        n.trees = model$gbm.call$best.trees
      )
      
    }
    
    
    
  } else {
    
    
    if(pll){
      
      library(foreach)
      library(doMC)
      
      registerDoMC(cores = ncores)
      
      result <- foreach(i = seq_len(length(predvars)), .packages = c("gbm", "raster", "dismo")) %dopar% {
        
        raster::predict(object = predvars[[i]],
                        model = model,
                        type = "response",
                        n.trees = model$gbm.call$best.trees)
      }
      
    } else{
      
      result <- foreach(i = seq_len(length(predvars)), .packages = c("gbm", "raster", "dismo")) %do% {
        
        raster::predict(object = predvars[[i]],
                        model = model,
                        type = "response",
                        n.trees = model$gbm.call$best.trees)
      }
      
    }
    
    
    result <- mapply(
      FUN = function(x,  y){
        
        names(x) <- y
        
        return(x)
        
      },
      x = result,
      y = sprintf(
        "sdm_%02d",
        0:(length(predvars) - 1)
      )
    )
    
    
   result <- brick(
     result %>% stack,
     filename = sprintf(
       "%s/brtpred_%s_%s_%s.grd",
       out_path,
       scn_id,
       varset,
       species
     ),
     overwrite = TRUE
   )
     
  }
 
  return(result)
   
}
