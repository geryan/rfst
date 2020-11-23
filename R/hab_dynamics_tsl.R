library(steps)
library(raster)

#### Prepare items for simulation ####
tsl_list <- list.files("tsl", pattern = ".grd")
tsl_stack <- stack(lapply(1:length(tsl_list), FUN = function(x) raster(paste0("tsl/", tsl_list[x]))))
tsl_stack <- raster::aggregate(tsl_stack, fact = 10)
names(tsl_stack) <- paste0('TSL_', 1:10)

hab <- tsl_stack[[1]]
hab[] <- 1

hab_stack <- stack(replicate(10, hab))
names(hab_stack) <- paste0('Habitat_', 1:10)

max_ind <- 25
k_function <- function(landscape, timestep) {
  
  suit <- landscape$suitability[[timestep]]
  
  max_ind / (1 + exp(-(suit - 0.5) / 0.05))
}

trans_mat <- matrix(c(0.000,0.425,0.425,
                      0.500,0.000,0.000,
                      0.000,0.850,0.850),
                    nrow = 3, ncol = 3, byrow = TRUE)
colnames(trans_mat) <- rownames(trans_mat) <- c('Newborn','Juvenile','Adult')
stable_states <- abs( eigen(trans_mat)$vectors[,1] / sum(eigen(trans_mat)$vectors[,1]) )

pop_stack <- stack(replicate(3, hab))
pop_stack[] <- 0
pop_stack[sample(1:ncell(pop_stack), 5000)] <- runif(5000, 0, 10)
pop_stack <- stack(lapply(1:nlayers(pop_stack), FUN = function(x) focal(pop_stack[[x]],
                                                                        w = matrix(1,9,9),
                                                                        fun = mean,
                                                                        na.rm = TRUE)))
pop_stack[is.na(pop_stack[])] <- 0
names(pop_stack) <- c('Newborn','Juvenile','Adult')


#### Basic simulation with no habitat dynamics ####
landscape <- landscape(population = pop_stack,
                       suitability = hab_stack,
                       carrying_capacity = k_function,
                       "disturbance" = tsl_stack)

pop_dynamics <- population_dynamics(
  change = growth(transition_matrix = trans_mat,
                  global_stochasticity = 0.01,
                  # transition_function = list(modified_transition(survival_layer = "hab_stack",
                  #                                                fecundity_layer = "hab_stack")
                  # )
  ),
  dispersal = cellular_automata_dispersal(max_cells = c(0, 10, 10)
  ),
  modification = NULL,
  density_dependence = NULL
)

set.seed(333)
results <- simulation(landscape = landscape,
                      population_dynamics = pop_dynamics,
                      habitat_dynamics = NULL,#list(disturbance(disturbance_layers = "fires")),
                      timesteps = 10,
                      replicates = 1)

plot(results)
hab_output <- stack(lapply(1:10, FUN = function(x) results[[1]][[x]][[2]]))
plot(hab_output)


#### Function for habitat modification ####
min_tsl <- min(cellStats(tsl_stack, min))
max_tsl <- max(cellStats(tsl_stack, max))

min_tsl <- 0
max_tsl <- 200

hyperbolic_dist <- function(x, shift, offset) (x - shift)^2 + offset
plot(seq(min_tsl, max_tsl, length.out = 100), hyperbolic_dist(seq(min_tsl, max_tsl, length.out = 100), mean(c(min_tsl, max_tsl)), 100), type = 'l')



logistic_dist <- function(x, shift, offset, shape) offset / (1 + exp(-(x - shift) / shape)) # Note, this can be a straight line also by changin parameters
plot(seq(min_tsl, max_tsl, length.out = 100), logistic_dist(seq(min_tsl, max_tsl, length.out = 100), mean(c(min_tsl, max_tsl)), 10, 10), type = 'l')

# This is where the magic happens - you just need to add this somewhere so it gets loaded into your environment
custom_dist <- function (disturbance_layers, disturbance_function) {
  
  dist_fun <- function (landscape, timestep) {
    
    if (raster::nlayers(landscape$suitability) > 1) {
      original_habitat <- landscape$suitability[[timestep]]
    } else {
      original_habitat <- landscape$suitability
    }
    
    if (raster::nlayers(landscape[[disturbance_layers]]) < timestep ) {
      stop("The number of disturbance layers must match the number of timesteps in the simulation")
    }
    
    # process disturbance layer
    dist_layer <- landscape[[disturbance_layers]][[timestep]]
    idx <- which(!is.na(dist_layer[]))
    
    mod_values <- disturbance_function(x = dist_layer[idx])
    mod_values_rescaled <- (mod_values - min(mod_values)) / (max(mod_values) - min(mod_values))

    modified_habitat <- original_habitat
    modified_habitat[idx] <- original_habitat[idx] * mod_values_rescaled
    
    names(modified_habitat) <- paste0("Habitat_", timestep)
    
    if (raster::nlayers(landscape$suitability) > 1) {
      landscape$suitability[[timestep]] <- modified_habitat
    } else {
      landscape$suitability <- modified_habitat
    }
    
    landscape
    
  }
}

#### Hyperbolic function example ####
set.seed(333)
results <- simulation(landscape = landscape,
                      population_dynamics = pop_dynamics,
                      habitat_dynamics = list(custom_dist(disturbance_layers = "disturbance",
                                                          disturbance_function = function(x,
                                                                                          shift = mean(c(min_tsl, max_tsl)),
                                                                                          offset = 100) {
                                                            (x - shift)^2 + offset} )),
                      timesteps = 10,
                      replicates = 1)

plot(results)
hab_output <- stack(lapply(1:10, FUN = function(x) results[[1]][[x]][[2]]))
plot(hab_output)
plot(tsl_stack[[1]])
plot(hab_output[[1]])


#### Logistic function example ####
set.seed(333)
results <- simulation(landscape = landscape,
                      population_dynamics = pop_dynamics,
                      habitat_dynamics = list(custom_dist(disturbance_layers = "disturbance",
                                                          disturbance_function = function(x,
                                                                                          shift = mean(c(min_tsl, max_tsl)),
                                                                                          offset = 10,
                                                                                          shape = 10) {
                                                            offset / (1 + exp(-(x - shift) / shape))} )),
                      timesteps = 10,
                      replicates = 1)

plot(results)
hab_output <- stack(lapply(1:10, FUN = function(x) results[[1]][[x]][[2]]))
plot(hab_output)
par(mfrow = c(1,2))
plot(tsl_stack[[1]])
plot(hab_output[[1]])
