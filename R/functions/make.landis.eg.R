make.landis.eg <- function(
  master.dir,
  new.dir,
  lsc = c(
    "EG19",
    "EG20"
  ),
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
  rep,
  overwrite = FALSE
){
   
  yr <- ifelse(lsc == "EG19", 2019, 2020)
  
  # read in files from master
  
  all.files <- list.files(
    path = master.dir,
    full.names = TRUE,
    recursive = TRUE,
    include.dirs = TRUE
  )
  
  if(!overwrite & dir.exists(new.dir)){
    stop("Directory at path `new.dir` already exists.\nSet `overwrite = TRUE` or delete directory and begin again.")
  }
  
  # create target directory
  
  dir.create(
    path = new.dir
  )
  
  
  file.copy(
    from = all.files,
    to = new.dir,
    overwrite = TRUE,
    recursive = TRUE
  )
  
  
  # edit and rename scenario file
  
  scenario_files <- list.files(
    path = master.dir,
    pattern = "Scenario",
    full.names = TRUE
  )
  
  
  
  scenario <- readLines(
    con = scenario_files[
      grep(
        pattern = sprintf(
          fmt = "%s_%s",
          yr,
          rcp
        ),
        x = scenario_files
      )
    ],
    
  )
  
  
  if(pb){
    scenario[21] <- sprintf(
      "\"Biomass Harvest\"\t            ./Biomass_Harvest_TH%s_PB.txt",
      th
    )
    
    
  } else{
    scenario[21] <- ">>\"Biomass Harvest\"\t            ./Biomass_Harvest.txt"
  }
  
  writeLines(
    text = scenario,
    con = paste0(
      new.dir,
      "/scenario.txt"
    ),
    sep = "\n"
  )
  
 
 
 
 print("Done")
 
}