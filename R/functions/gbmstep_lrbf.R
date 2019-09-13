gbmstep_lrbf <- function(data,
                    lrbf,
                    vars,
                    tree.complexity = 5,
                    step.size = 1,
                    prev.stratify = FALSE,
                    verbose = FALSE,
                    max.trees = 2000){
  
  library(dismo)
  
  learning.rate <- as.numeric(substr(lrbf, 1, 5))
  bag.fraction <- as.numeric(substr(lrbf, 6, 9))
  
  
  if(missing(vars)){
    vars <- 2:ncol(data)
  }
  
  result <- gbm.step(data = data,
                     gbm.x = vars,
                     gbm.y = 1,
                     family = "bernoulli",
                     tree.complexity = tree.complexity,
                     learning.rate = learning.rate,
                     step.size = step.size,
                     bag.fraction = bag.fraction,
                     prev.stratify = prev.stratify,
                     verbose = verbose,
                     max.trees = max.trees)
  
  return(result)
  
}

