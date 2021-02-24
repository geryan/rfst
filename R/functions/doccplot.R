doccplot <- function(
  x,
  index = 1,
  title = TRUE,
  subtitle = TRUE
){
  
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
  
  
  p <- ccplot(
    cc = x$lcc[[index]],
    title = title,
    subtitle = st
  )
  
  return(p)
  
}