cc_fun <- function(
  x,
  carcap,
  z,
  threshold
) {
  
  ifelse(
    x < threshold,
    0,
    round(carcap * 1/(1 + exp(-10*(x - z))))
  )
}