do_pva <- function(
  hab,
  ntimresteps = 50,
  nreps = 1,
  index = 1,
  static = FALSE,
  ip_raster = FALSE,
  mod_habitat = FALSE,
  eg = FALSE,
  fire_scar = NULL
){
  
  
  habitat_map <- hab$hab_map[[index]]
  
  mortality_map <- hab$mort_map[[index]]
  
  
  if(static){
    habitat_map <- habitat_map[[1]]
    
    mortality_map <- stack(
      rep(
        list(mortality_map[[1]]),
        ntimesteps
      )
    )
  }
  
  
  if(mod_habitat){
    if(!is.na(hab$habfun[[index]])){
      habitat_map <- modify_habitat(
        habitat_map,
        disturbance_map = hab$dist_map[[index]],
        habfun = hab$habfun[[index]]
      )
    }
  }
  
  
  initial_population <- initpop5(
    hs = habitat_map[[1]],
    popsize = hab$popsize[index],
    cc = hab$cc[index],
    ss = hab$ss[[index]],
    pp = hab$pp[[index]],
    z = hab$z[[index]]
  )
  
  
  if(eg){
    if(hab$yearid[[index]] == "EG20"){
      initial_population <- initial_population*fire_scar
    }
  }
  
  
  lsc <- landscape(
    population = initial_population,
    suitability = habitat_map,
    "mortality" = mortality_map,
    carrying_capacity = hab$ccfun[[index]]
  )
  
  
  
  disp <- cellular_automata_dispersal(
    max_cells = hab$max_cells[[index]],
    dispersal_proportion = set_proportion_dispersing(
      proportions = hab$dp[[index]]
    ),
    # dispersal_proportion = density_dependence_dispersing(
    #   maximum_proportions = hab$dp[[index]]
    # ),
    barriers = NULL,
    use_suitability = TRUE,
    carrying_capacity = "carrying_capacity"
  )
  
  grow <- growth(
    transition_matrix = hab$tm[[index]],
    global_stochasticity = hab$stoch[index],
    transition_function = competition_density(
      #R_max = 1.3
    )
  )
  
  pop_dyn <- population_dynamics(
    change = grow,
    dispersal = disp,
    #density_dependence = ceiling_density(),
    modification = mortality(mortality_layer = "mortality")
  )
  
  
  simres <- simulation(
    landscape = lsc,
    population_dynamics = pop_dyn,
    demo_stochasticity = "full",
    timesteps = ntimesteps,
    replicates = nreps,
    verbose = ifelse(nreps <= 10, TRUE, FALSE)
  )
  
  
  ip <- initial_population %>%
    getValues %>%
    sum(na.rm = TRUE)
  
  result <- list(
    simres = simres,
    ip = ip
  )
  
  if(ip_raster){
    result$ip_raster <- initial_population
  }
  
  return(result)
  
  
}