dopvaplot <- function(
  x,
  ageClassNames,
  p0 = TRUE,
  stages = FALSE,
  cc = FALSE,
  emp = TRUE,
  index = 1,
  title = TRUE,
  subtitle = TRUE
){
  
  
  if(missing(ageClassNames)){
    
    if(x$sp[index] == "vava"){
      ageClassNames <- c('Hatchling','Juvenile', "Sub-adult",'Adult')
    } else if(x$sp[index] == "polo"){
      ageClassNames <- c("Newborn", "Adult")
    }  else if(x$sp[index] == "tyte"){
      ageClassNames <- c("Newborn", "Sub-adult", "Adult")
    }  else if(x$sp[index] == "smle"){
      ageClassNames <- c("Newborn", "Adult", "Dummy")
    } else {
      ageClassNames <- c('Newborn','Juvenile','Adult')
    }
  }
  
  
  p0 <- ifelse(
    test = p0,
    yes = ifelse(
      test = x$yearid[index] == "EG20",
      yes = x$init_pop[index],
      no = x$popsize[index]
    ),
    no = NULL
  )
  
  if(cc){
    cc <- x$lcc[[index]]
  } else {
    cc <- NA
  }
  
  title <- ifelse(
    test = title,
    yes = x$species_com[index] %>%
      as.character,
    no = NA
  )
  
  st1 <- ifelse(
    test = x$yearid[index] == "CH",
    yes = x$landscape[index] %>%
      as.character,
    no = sprintf(
      "%s %s",
      x$landscape[index],
      x$year[index]
    )
  )
  
  
  st <- sprintf(
    "%s, %s, %s",
    st1,
    x$scn[index] %>%
      as.character,
    x$climate_model[index]
  )
  
  popmat <- x$pva[[index]]
  
  p <- pvaplot(
    popmat = popmat,
    p0 = p0,
    stages = stages,
    yl = NA,
    ageClassNames = ageClassNames,
    cc = cc,
    emp = emp,
    title = title,
    subtitle = st
  )
  
  p
  
}