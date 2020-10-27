make.landis <- function(
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
  
  # get list of specific files to choose from
  
  biomass_harvest_files <- list.files(
    path = master.dir,
    pattern = "Biomass_Harvest",
    full.names = TRUE
  )
  
  dynamic_input_files <- list.files(
    path = master.dir,
    pattern = "Dynamic_RCP",
    full.names = TRUE
  )
  
  fire_weather_files <- list.files(
    path = master.dir,
    pattern = "Fire_Weather",
    full.names = TRUE
  )
  
  
  
  # copy files going to all scenarios
  
  files_to_copy <- all.files[
    !all.files %in% biomass_harvest_files &
      !all.files %in% dynamic_input_files &
      !all.files %in% fire_weather_files
  ]
  
  file.copy(
    from = files_to_copy,
    to = new.dir,
    overwrite = TRUE,
    recursive = TRUE
  )
  
  
  # copy appropriate biomass harvest file and edit scenario file to point to that file
  
  scenario <- readLines(
    con = paste0(
      new.dir,
      "/scenario.txt"
    )
  )
  
  
  if(pb){
    scenario[21] <- sprintf(
      "\"Biomass Harvest\"\t            ./Biomass_Harvest_TH%s_PB.txt",
      th
    )
    
    file.copy(
      from = biomass_harvest_files[
        grep(
          pattern = sprintf(
            "TH%s_PB",
            th
          ),
          x = biomass_harvest_files
        )
      ],
      to = new.dir,
      overwrite = TRUE,
      recursive = TRUE
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
  
  # copy appropriate dynamic input file and edit biomass succession to point to that
  
  file.copy(
    from = dynamic_input_files[
      grep(
        pattern = sprintf(
          "%s_",
          rcp
        ),
        x = dynamic_input_files
      )
      ],
    to = new.dir,
    overwrite = TRUE,
    recursive = TRUE
  )
  
  biomass_succession <- readLines(
    con = paste0(
      new.dir,
      "/Succession_Biomass_EG.txt"
    )
  )
  
  biomass_succession[168] <- sprintf(
    "DynamicInputFile \t\t\t ./Dynamic_Input_%s_R%s.txt" ,
    rcp,
    rep
  )
  
  writeLines(
    text = biomass_succession,
    con  = paste0(
      new.dir,
      "/Succession_Biomass_EG.txt"
    ),
    sep = "\n"
  )
  
  # copy appropriate fire weather and edit dynamic fire to point to it
  
  file.copy(
    from = fire_weather_files[
      grep(
        pattern = rcp,
        x = fire_weather_files
      )
    ],
    to = new.dir,
    overwrite = TRUE,
    recursive = TRUE
  )
  
  file.copy(
    from = fire_weather_files[
      grep(
        pattern = "Fire_Weather_Current",
        x = fire_weather_files
      )
      ],
    to = new.dir,
    overwrite = TRUE,
    recursive = TRUE
  )
  
 
 
 print("Done")
 
}