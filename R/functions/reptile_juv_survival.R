reptile_juv_survival <- function(
  a, # age at first reproduction
  Sa, # adult survival
  n, # number of clutches per year
  c # number per clutch
){
  
  Sj = (2*(1- Sa)/(n*c))^(1/a)
  
  return(Sj)
}

# Formula from:
# Pike, D.A., Pizzatto, L., Pike, B.A. and Shine, R., 2008.
# Estimating survival rates of uncatchable animals: the myth of high juvenile mortality in reptiles.
# Ecology, 89(3), pp.607-611.
# https://doi.org/10.1890/06-2162.1