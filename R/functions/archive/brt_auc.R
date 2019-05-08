brt_auc <- function(x){
  
  # calculate area under roc curve for BRT
  
  if(class(x) != "gbm"){stop("object must be of class gbm")}
  
  auc <- round(x$cv.statistics$discrimination.mean, 2)
  
  err <- round(x$cv.statistics$discrimination.se, 2)
  
  paste("Discrimination is", auc, "+/-", err)
}