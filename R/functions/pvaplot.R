# pvaplot(
#   popmat = x$pva[[1]],
#   p0 = x$popsize[[1]],
#   stages = FALSE,
#   yl = NA,
#   #ageClassNames,
#   cc = x$lcc[[1]],
#   emp = TRUE,
#   title = NA,
#   subtitle = NA
# )



pvaplot <- function(
  popmat,
  p0 = NULL,
  stages = FALSE,
  yl = NA,
  ageClassNames,
  cc = NA,
  emp = TRUE,
  title = NA,
  subtitle = NA
){
  
  if(!is.null(p0) & stages){stop ("Cannot plot stages and initial pop without stable states")}
  

  
  if(missing(ageClassNames)){
    ageClassNames <- c("Newborn", "Juvenile", "Adult")
  }
  
  if(dim(popmat)[2] != length(ageClassNames)){stop("ageClassNames different length to matrix classes (dim(popmat)[2]")}
  

  pop <- array(
    data = popmat,
    dim = dim(popmat),
    dimnames = list(
      "year" = 1:dim(popmat)[1],
      "stage" = ageClassNames,
      "replicate" = 1:dim(popmat)[3]
    )
  ) %>%
    as_tibble(
      rownames = "year"
    ) %>%
    pivot_longer(
      cols = -year,
      names_sep = "\\.",
      names_to = c("stage", "replicate"),
      values_to = "population"
    ) %>%
    mutate(
      year = as.integer(year),
      replicate = as.factor(replicate)
    )
  
  if(!stages){
    
    pop <- pop %>%
      group_by(year, replicate) %>%
      summarise(
        population = sum(population)
      ) %>%
      ungroup
    
    if(!is.null(p0)){
      
      if(!is.na(p0)){
        pop0 <- tibble(
          year = 0,
          replicate = 1:dim(popmat)[3] %>% as.factor,
          population = p0
        )
        
        pop <- bind_rows(pop0, pop)
      }
      
    }
    
    medpop <- pop %>%
      group_by(year) %>%
      summarise(population = median(population)) %>%
      ungroup
    
    cipop <- pop %>%
      group_by(year) %>%
      summarise(
        lo = quantile(population, probs = 0.025),
        hi = quantile(population, probs = 0.975)
      ) %>%
      ungroup
  } else{
    
    medpop <- pop %>%
      group_by(year, stage) %>%
      summarise(population = median(population)) %>%
      ungroup
    
    cipop <- pop %>%
      group_by(year, stage) %>%
      summarise(
        lo = quantile(population, probs = 0.025),
        hi = quantile(population, probs = 0.975)
      ) %>%
      ungroup
  }
  
    p <- ggplot() +
      ylim(0, yl) +
      geom_ribbon(
        data = cipop,
        aes(
          x = year,
          ymin = lo,
          ymax = hi
        ),
        fill = grDevices::grey(0.8)
      ) +
      geom_line(
        data = pop,
        aes(
          x = year,
          y = population,
          group = replicate
        ),
        colour = grDevices::grey(0.7),
        size = 0.2
      ) +
      geom_line(
        data = medpop,
        aes(
          x = year,
          y = population
        ),
        colour = grDevices::grey(0.2)
      ) +
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
        y = "Population size"
      )
    
    if(stages){
      p <- p +
        facet_wrap(
          facets = ~ stage,
          nrow = dim(popmat)[2],
          strip.position = "right",
          scales = "free_y"
        )
    }
    
    if(emp){
      if(stages){stop("no emp with stages")}
      
      exmp <- pop %>%
        group_by(replicate) %>%
        summarise(exmp = min(population)) %>%
        pull(exmp) %>%
        quantile(probs = 0.05)
      
      p <- p +
        geom_hline(
          yintercept = exmp,
          colour = ifelse(exmp == 0, "red", grDevices::grey(0.2)),
          linetype = "dotted"
        )
    }
    
    if(!is.na(cc)){
      
      cctb <- tibble(
        year = unique(pop$year)[order(unique(pop$year))],
        cc = cc[1:length(unique(pop$year))]
      )
      
      p <- p +
        geom_line(
          data = cctb,
          aes(
            x = year,
            y = cc
          ),
          linetype = "dashed",
          colour = grDevices::grey(0.2)
        )
      
    }
    
    if(!is.na(title)){
      if(!is.na(subtitle)){
        p <- p +
          labs(
            title = title,
            subtitle = subtitle,
            x = "Year",
            y = "Population size"
          )
      } else {
        p <- p +
          labs(
            title = title,
            x = "Year",
            y = "Population size"
          )
      }
    }
    
    
  p
  
}


