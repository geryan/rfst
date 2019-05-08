brt_dev_exp <- function(x){
  
  # reports percentage reduction in deviance 
  
  if(class(x) != "gbm"){stop("object must be of class gbm")}
  
  
  dev <- round(100*(x$self.statistics$mean.null - x$cv.statistics$deviance.mean)/x$self.statistics$mean.null, digits = 2)
  
  err <- round(100*x$cv.statistics$deviance.se/x$self.statistics$mean.null, 2)
  
  
  return(paste("Percentage deviance reduction is", dev, "+/1", err))
  
}