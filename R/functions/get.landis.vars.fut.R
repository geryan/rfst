get.landis.vars.fut <- function(
  scn_path,
  proj_mask,
  timesteps,
  cores
  ){
  
 result <- .get.landis.vars.fut(scn_path = scn_path,
                                proj_mask = proj_mask,
                                timesteps = timesteps,
                                cores = cores)

 
 library(future)
 library(future.apply)
 
 plan(multiprocess, workers = cores)
 
 result <- future_lapply(result, function (x) {
   names(x) <- c("lbm",
                 "ggf",
                 "ggd",
                 "hbt_3h",
                 "hbt_1k",
                 "prop_bio_regn",
                 "prop_bio_targ",
                 "prop_old_150",
                 "prop_old_200")
   x})
  
  return(result) 
}