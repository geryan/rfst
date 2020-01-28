clean.model.data <- function(
  x,
  y = NA,
  na.omit = FALSE
){
  
  library(dplyr)
  library(lubridate)
  
  result <- x %>%
    mutate(nd = as.numeric(ymd("2019-01-01") - date)/365,
           nd = ifelse(nd > 0, nd, 0)) %>%
    mutate(tsf = tsf - nd,
           tsl = tsl - nd,
           max_age = max_age - nd) %>%
    mutate(max_age = case_when(tsf < 0 ~ NA_real_,
                               tsl < 0 ~ NA_real_,
                               max_age < 0 ~ NA_real_,
                               TRUE ~ max_age)) %>%
    mutate(tsf = ifelse(tsf >= 0, tsf, NA),
           tsl = ifelse(tsl >= 0, tsl, NA))
  
  if(!is.na(varset)){
    result <- result %>%
      dplyr::select("PA", y)
  }
  
  
  if(na.omit){
    result <- result %>%
      na.omit
  }
  
  return(result)
  
}
