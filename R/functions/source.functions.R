source.functions <- function(x = "R/functions"){
  
  lf <- list.files(path = x, pattern = "\\.R$")
  
  fp <- paste0(x, "/", lf)
  
  for(i in 1:length(fp)){
    source(file = fp[i])
  }
  
  
}