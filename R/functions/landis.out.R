landis.out <- function(scn_id){
  
  filepath.list <- list.files(
    path = sprintf(
      "~/%s",
      scn_id
    ),
    pattern = ".img",
    recursive = TRUE
  )
  
  
  object.list <- sub(
    pattern = "\\d+.*",
    replacement = "",
    x = filepath.list
  ) %>%
    sub(
      pattern = ".*\\/",
      replacement = "",
      x = .
    ) %>%
    unique
  
  result <- vector(
    mode = "list",
    length = length(object.list)
  )
  
  
  names(result) <- gsub(
    pattern = "-",
    replacement = ".",
    object.list
  )
  
  for(i in 1:length(object.list)){
    
    ofiles <- filepath.list[
      grep(
        pattern = object.list[i],
        x = filepath.list
      )
    ] %>%
      sprintf(
        "~/%s/%s",
        scn_id,
        .
      )
    
    nfiles <- length(ofiles)
    
    if(nfiles == 1){
      orast <- raster(ofiles)
    } else {
      orast <- stack(ofiles)
    }
    
    result[[i]] <- orast
    
  }
  
  return(result)
  
}

