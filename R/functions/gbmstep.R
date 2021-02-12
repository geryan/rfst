gbmstep <- function(
  data,
  vars,
  tree.complexity = 5,
  learning.rate = 0.1,
  step.size = 50,
  bag.fraction = 0.5,
  prev.stratify = FALSE,
  verbose = FALSE,
  max.trees = 2000
){
  
  library(dismo)
  
  if(colnames(data)[length(colnames(data))] == "site.weights"){
   
    if(missing(vars)){
      vars <- 2:(ncol(data)-1)
    }
     
    result <- gbm.step(
      data = data,
      gbm.x = vars,
      gbm.y = 1,
      family = "bernoulli",
      tree.complexity = tree.complexity,
      learning.rate = learning.rate,
      step.size = step.size,
      bag.fraction = bag.fraction,
      prev.stratify = prev.stratify,
      verbose = verbose,
      max.trees = max.trees,
      site.weights = data$site.weights
    )
    
    return(result)
    
  } else {
    
    if(missing(vars)){
      vars <- 2:ncol(data)
    }
    
    result <- gbm.step(
      data = data,
      gbm.x = vars,
      gbm.y = 1,
      family = "bernoulli",
      tree.complexity = tree.complexity,
      learning.rate = learning.rate,
      step.size = step.size,
      bag.fraction = bag.fraction,
      prev.stratify = prev.stratify,
      verbose = verbose,
      max.trees = max.trees
    )
    
    return(result)
    
  }
  
  
  
}

