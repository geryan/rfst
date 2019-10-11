cc_6_fun <- function(suitability) {
  6 - round(6 * dlogis(suitability, scale = 0.25))
}