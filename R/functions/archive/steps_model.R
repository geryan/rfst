library(steps)
library(raster)
library(viridis)
library(doMC)
library(foreach)
library(future)
plan(multiprocess)
plan(sequential)

############ MODEL INPUTS ###############

#Note, this is an age-structured matrix and is not generic
gg_trans_mat <- matrix(c(0.00,0.00,0.50,
                         0.50,0.00,0.00,
                         0.00,0.85,0.85),
                         nrow = 3, ncol = 3, byrow = TRUE)
colnames(gg_trans_mat) <- rownames(gg_trans_mat) <- c('Newborn','Juvenile','Adult')

gg_stable_states <- abs( eigen(gg_trans_mat)$vectors[,1] / base::sum(eigen(gg_trans_mat)$vectors[,1]) ) 

gg_hab_suit <- raster("output/gg_pred_map_glm.tif")
#gg_hab_suit <- aggregate(gg_hab_suit, fact = 10, fun = mean) # aggregate to reduce computation time

names(gg_hab_suit) <- "Habitat"
#plot(gg_hab_suit, box = FALSE, axes = FALSE, col = viridis(100))

gg_hab_k <- calc(gg_hab_suit, fun = function(x) rbinom(prob = x, size = 3, n = 1))
gg_hab_k[is.na(gg_hab_k)] <- 0
names(gg_hab_k) <- "Carrying Capacity"
#plot(gg_hab_k, box = FALSE, axes = FALSE, col = viridis(3))

gg_popN <- stack(replicate(ncol(gg_trans_mat), floor(gg_hab_suit*3)))
#gg_popN <- stack(replicate(ncol(gg_trans_mat), floor(gg_hab_suit*200)))

gg_popN <- gg_popN*gg_stable_states

registerDoMC(cores=4)
gg_pop <- stack(
  foreach(i = 1:nlayers(gg_popN)) %dopar% {
    m <- ceiling(cellStats(gg_popN[[i]], max, na.rm=T))
    pop <- calc(gg_popN[[i]], fun = function(x) rbinom(prob = (x/m), size = m, n = 1))
    pop[sample(ncell(pop), ncell(pop)*0.995)] <- 0
    pop
  })
names(gg_pop) <- colnames(gg_trans_mat)
#plot(gg_pop, box = FALSE, axes = FALSE, col = viridis(100))

TotpopN <- sum(cellStats(gg_pop, 'sum', na.rm = T)) # Get total population size to check sensible
init_pop_size <- sum(gg_pop)
#plot(init_pop_size, box = FALSE, axes = FALSE, col = viridis(25))

# read in landis fire predictions
gg_dist_fires <- stack(foreach(i = 1:50) %do% {
  r <- raster(paste0("data/grids/landis/GG_FIRE_GDA9455_", sprintf("%02d", i), ".tif"))
  #aggregate(r, fact = 10, fun = max)
  crop(r, extent(gg_hab_suit))
})

# read in landis logging predictions
gg_dist_logging <- stack(foreach(i = 1:50) %do% {
  r <- raster(paste0("data/grids/landis/GG_LOGGING_GDA9455_", sprintf("%02d", i), ".tif"))
  #aggregate(r, fact = 10, fun = max)
  crop(r, extent(gg_hab_suit))
})

gg_landscape <- landscape(population = gg_pop,
                          suitability = gg_hab_suit,
                          carrying_capacity = gg_hab_k,
                          "fires" = gg_dist_fires,
                          "logging" = gg_dist_logging)

gg_pop_dynamics <- population_dynamics(change = growth(transition_matrix = gg_trans_mat,
                                                       global_stochasticity = 0.05),
                                       # dispersal = cellular_automata_dispersal(dispersal_distance = c(0, 8, 0),
                                       #                                         dispersal_kernel = exponential_dispersal_kernel(distance_decay = 0.1),
                                       #                                         dispersal_proportion = c(0, 0.5, 0)),
                                       dispersal = kernel_dispersal(dispersal_kernel = exponential_dispersal_kernel(distance_decay = 8000),
                                                                    dispersal_distance = 8000,
                                                                    dispersal_proportion = c(0, 1, 0),
                                                                    arrival_probability = "carrying_capacity"
                                                                    ),
                                       modification = NULL,
                                       density_dependence = ceiling_density(stages = c(2,3)))

gg_results <- simulation(landscape = gg_landscape,
                         population_dynamics = gg_pop_dynamics,
                         habitat_dynamics = list(fire_effects(fire_layers = "fires",
                                                              lag = 5)),
                         timesteps = 50,
                         replicates = 1)

plot(gg_results)
plot(gg_results[1], type = "raster", stages = 0, timesteps = c(5,10,15,20,25,30,35,40,45))
plot(gg_results[1], type = "raster", stages = 0, animate = TRUE, timesteps = c(5,10,15,20,25,30,35,40,45,50))
