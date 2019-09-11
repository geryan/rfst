brt_auc <- function(x, res = "auc"){
  
  # calculate area under roc curve for BRT
  
  if(class(x) != "gbm"){stop("object must be of class gbm")}
  
  auc <- round(x$cv.statistics$discrimination.mean, 2)
  
  if(res == "auc"){
    return(auc)
  }
  
  err <- round(x$cv.statistics$discrimination.se, 2)
  
  if(res == "err"){
    return(err)
  }
  
  return(paste("Discrimination is", auc, "+/-", err))
  
  
  
  
}