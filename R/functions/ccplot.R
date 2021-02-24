ccplot <- function(
  cc,
  title = NA,
  subtitle = NA,
  yl = NA
){
  
  
  cctb <- tibble(
    year = 1:length(cc),
    cc = cc
  )
  
  p <- ggplot() +
    ylim(0, yl) +
    cowplot::theme_cowplot() +
    cowplot::panel_border(remove = TRUE) +
    theme(
      legend.position = "none",
      strip.background = element_blank(),
      strip.text = element_text(hjust = 0, face = "bold"),
      axis.text.x = element_text(size = 8)
    ) +
    labs(
      x = "Year",
      y = "Carrying capacity"
    ) +
    
    geom_line(
      data = cctb,
      aes(
        x = year,
        y = cc
        )
      )
  
  if(!is.na(title)){
    if(!is.na(subtitle)){
      p <- p +
        labs(
          title = title,
          subtitle = subtitle,
          x = "Year",
          y = "Carrying capacity"
        )
    } else {
      p <- p +
        labs(
          title = title,
          x = "Year",
          y = "Carrying capacity"
        )
    }
  }
  
  
  p
  
}