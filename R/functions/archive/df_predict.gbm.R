df_predict.gbm <- function(x, y, ... ){
  predict.gbm(object =  x,  newdata = data.frame(y), n.trees = x$gbm.call$best.trees)
}