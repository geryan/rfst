mply.population_dynamics <- function(x, stoch){
  
  library(tibble)
  library(magrittr)
  library(dplyr)
  
  x <- simset_lb
  stoch <- 0.1
  
  gr <- lapply(
    X = x$tm,
    FUN = growth,
    global_stochasticity = stoch
  )
  
  
  gr <- tibble(gr = gr)
  
  x <- bind_cols(x, gr)
  
  pd <- lapply(
    X = x$gr,
    FUN = population_dynamics,
    modification = mortality(mortality_layer = "mortality")
  )
  
  
  pd <- tibble(pd = pd)
  
  result <- bind_cols(x, pd)
  
  return(result)
  
}
