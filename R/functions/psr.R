psr <- function(
  popmat,
  ageClassNames,
  stages = NULL,
  emp = TRUE,
  ...
){
  
  
  if(missing(ageClassNames)){
    ageClassNames <- c("Newborn", "Juvenile", "Adult")
  }
  
  total_stages <- dim(popmat)[2]
  reps <- dim(popmat)[3]
  stage_names <- ageClassNames
  
  pop <- popmat
  pop.mn <- round(apply(pop, c(1,2), mean), 0)
  
  
  if (is.null(stages)) {
    
    graphics::par(mar=c(5.1, 4.1, 4.1, 2.1), mfrow=c(1, total_stages))
    
    for (i in seq_len(total_stages)) {
      
      graphics::plot(pop.mn[, i],
                     type = 'l',
                     ylab = paste("Total Population: ", stage_names[i]),
                     xlab = "Timesteps",
                     #lwd = 3,
                     col = graph.pal[i],
                     ylim=range(pretty(pop)),
                     xaxt = 'n',
                     ...)
      axis(side = 1, at = unique(c(c(1, seq(0, length(pop.mn[, i]), by = round(ifelse(length(pop.mn[, i]) < 10, 10, length(pop.mn[, i])) / 10))[-1]), length(pop.mn[, i]))))
      
      for (j in 1:reps) {
        graphics::lines(pop[ , i, j],
                        col = 'grey',
                        lwd = 1)
      }
      
      graphics::lines(pop.mn[, i],
                      lwd = 3,
                      col = graph.pal[i])
      
    }
    
  }
  
  if(!is.null(stages) && stages == 0) {
    
    graphics::par(mar = c(5.1, 4.1, 4.1, 2.1))
    
    # draw the 95% CI polygon (if available) and median line
    quants <- t(apply(apply(pop, 3, rowSums),1, stats::quantile, c(0.025, 0.5, 0.975)))
    
    xaxs <- seq_len(nrow(pop[ , , 1]))
    
    graphics::plot(quants[, 2], #rowSums(pop[ , , 1]),
                   type = 'l',
                   ylab = "Total Population (all stages)",
                   xlab = "Timesteps",
                   #lwd = 3,
                   col = 'black',
                   ylim=range(pretty(quants)),
                   xaxt = 'n',
                   ...)
    axis(side = 1, at = unique(c(c(1, seq(0, length(quants[, 2]), by = round(length(quants[, 2]) / 10))[-1]), length(quants[, 2]))))
    
    # for (j in seq_along(x)[-1]) {
    #   graphics::lines(rowSums(pop[ , , j]),
    #                   col = 'gray')
    # }
    
    graphics::polygon(x = c(xaxs, rev(xaxs)),
                      y = c(quants[, 1], rev(quants[, 3])),
                      col = grDevices::grey(0.9),
                      border = NA)
    
    graphics::lines(quants[, 2] ~ xaxs,
                    #lwd = 2,
                    col = grDevices::grey(0.4))
    
    if (emp) {
      graphics::abline(h = round(mean(apply(pop, 3, function(x) min(rowSums(x)))), 0), lwd = 1, lty = 2)
    }
    
  }
  
  if (!is.null(stages) && stages > 0) {
    
    graphics::par(mar=c(5.1, 4.1, 4.1, 2.1), mfrow=c(1, length(stages)))
    
    graphics::plot(pop.mn[ , stages],
                   type = 'l',
                   ylab = paste("Total Population: ", stage_names[stages]),
                   xlab = "Timesteps",
                   #lwd = 3,
                   col = graph.pal[stages],
                   xaxt = 'n',
                   ...)
    axis(side = 1, at = unique(c(c(1, seq(0, length(pop.mn[ , stages]), by = round(length(pop.mn[ , stages]) / 10))[-1]), length(pop.mn[ , stages]))))
    
    for (j in 1:reps) {
      graphics::lines(pop[ , stages, j],
                      col = 'grey',
                      lwd = 1)
    }
    
    graphics::lines(pop.mn[ , stages],
                    lwd = 3,
                    col = graph.pal[stages])    
    
  }
  
}
