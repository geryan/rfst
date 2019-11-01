psrm <- function(
  popmat,
  ageClassNames,
  stages = NULL,
  emp = TRUE,
  extx = TRUE,
  ...
){
  
  
  if(missing(ageClassNames)){
    ageClassNames <- c("Newborn", "Juvenile", "Adult")
  }
  
  
  graph.pal <- c(
    "#6da36b",
    "#eb7d75",
    "#80b1d3",
    "#bebada",
    "#f0ab7e",
    "#969696"
  )

  npops <- length(popmat)
  
  total_stages <- dim(popmat[[1]])[2]
  reps <- dim(popmat[[1]])[3]
  stage_names <- ageClassNames
  
  pop <- popmat
  pop.mn <- lapply(pop, FUN = function(x){round(apply(x, c(1,2), mean), 0)})
  
  quants <- lapply(pop, FUN = function(x){t(apply(apply(x, 3, rowSums),1, stats::quantile, c(0.025, 0.5, 0.975)))})
  
  tpop <- lapply(pop, FUN = function(x){t(apply(x, MARGIN = 3, FUN = rowSums))})
  
  xaxs <- seq_len(nrow(pop[[1]][ , , 1]))
  
  graphics::par(mar = c(5.1, 4.1, 4.1, 2.1), mfrow = c(npops,1))
  
  for(i in seq_len(npops)){
    
    # draw the 95% CI polygon (if available) and median line
   
    graphics::plot(quants[[i]][, 2], #rowSums(pop[ , , 1]),
                   type = 'l',
                   ylab = "Total Population (all stages)",
                   xlab = "Timesteps",
                   #lwd = 3,
                   col = 'black',
                   ylim=range(pretty(quants[[i]])),
                   xaxt = 'n',
                   ...)
    axis(side = 1, at = unique(c(c(1, seq(0, length(quants[[i]][, 2]), by = round(length(quants[[i]][, 2]) / 10))[-1]), length(quants[[i]][, 2]))))
    
    
    graphics::polygon(x = c(xaxs, rev(xaxs)),
                      y = c(quants[[i]][, 1], rev(quants[[i]][, 3])),
                      col = grDevices::grey(0.9),
                      border = NA)
    
    
    for (j in 1:reps) {
      graphics::lines(tpop[[i]][j , ],
                      col = grDevices::grey(0.8),
                      lwd = 1)
    }
    
    graphics::lines(quants[[i]][, 2] ~ xaxs,
                    #lwd = 2,
                    col = grDevices::grey(0.2))
    
    
    
    if (emp) {
      
      empcol <- ifelse(round(min(apply(pop[[i]], 3, function(x) min(rowSums(x)))), 0) == 0, "red", "black")
      
      graphics::abline(h = round(min(apply(pop[[i]], 3, function(x) min(rowSums(x)))), 0), lwd = 1, lty = 2, col = empcol)
      
    }
    
    if(extx){
      text(
        x = dim(popmat[[i]])[1]/2,
        y = range(pretty(quants[[i]]))[2]/2,
        labels = "ILLUSTRATIVE ONLY",
        col = "red"
      )
    }
    
  }

    
    
    
  
}
