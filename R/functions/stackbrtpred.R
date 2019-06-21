stackbrtpred <- function(
  in_path = "output/habitat_pred/",
  scn_id,
  varset,
  species,
  ntimesteps
){
  
  files <- list.files(
    path = in_path,
    pattern = sprintf(
      "brtpred_%s_%s_%s",
      scn_id,
      varset,
      species
    )
  )
  
  n <- ifelse(
    missing(
      ntimesteps
      ),
    length(
      files
      ),
    ntimesteps + 1
    )
  
  files <- paste0(
    in_path,
    "/",
    files
  )
  
  result <- stack(
    files[1:n]
    )
  
  names(result) <- sprintf("sdm_%s", 0:(n - 1))
  
  return(result)
}