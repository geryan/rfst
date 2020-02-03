master.dir <- "D:/landis_runs/landis_master_scenario/"
new.dir <- "D:/landis_runs/test"


make.landis <- function(
  master.dir,
  new.dir,
  th = c(
    "19",
    "30",
    "00"
  ),
  pb = TRUE,
  rcp = c(
    "45",
    "85"
  ),
  rep
){
  
  all.files <- list.files(
    path = master.dir,
    full.names = TRUE,
    recursive = TRUE,
    include.dirs = TRUE
  )
  
  if(dir.exists(new.dir)){
    stop("new.dir already exists.\nThis function will not overwrite it.\nPlease delete directory and begin again")
  }
  
  dir.create(
    path = new.dir
  )
  
  biomass_harvest_files <- 
  
  
}