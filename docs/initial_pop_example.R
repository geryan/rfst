library(raster)
set.seed(33)

# create habitat raster with values of 0-1 and some NA cells
habitat_values <- runif(25)

habitat_values[c(1:2, 22:25)] <- NA

habitat <- raster(matrix(habitat_values, nrow = 5, byrow = TRUE))

#plot(habitat, zlim = c(0,1))

# population size to scatter
popsize <- 50

# carrying capacity of cell with value of 1
carr_capacity <- 10

ncells <- length(habitat_values)

# set up empty vector with population corresponding to cells
population_values <- ifelse(is.na(habitat_values), NA_real_, 0)

sum(habitat_values*carr_capacity, na.rm = TRUE)

# fill population vector until popsize is reached
while(sum(population_values, na.rm = TRUE) < popsize){
  
  # randomly select a cell
  z <- sample(1:ncells, 1) 
  
  # bernoulli trial with probability of success the habitat suitability value in that cell
  x <- suppressWarnings(rbinom(n = 1, size = 1, p = habitat_values[z]))
  
  # if the outcome of the trial is successful and the carrying capacity is not yet reached in that cell, then add an individual to that cell in the population vector
  if(!is.na(x)){
    if(x == 1){
      if(population_values[z] <= (carr_capacity * habitat_values[z] - 1)){
        population_values[z] <- population_values[z] + 1
      }
    }
  }
  
}

population_values

#plot(raster(matrix(population_values, nrow = 5, byrow = TRUE)))