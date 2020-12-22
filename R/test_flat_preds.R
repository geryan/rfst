


load("output/RData/10_predict_SDMs_2.RData")

pred_out_path <- "/data/scratch/projects/punim0995/habitat_pred/"

agg_out_path <- "/data/gpfs/projects/punim0995/rfst/output/habitat_pred_aggregated/"

##############

vars <- var_set$all_vars[[1]]

maxage1 <- vars[[1]]$max_age

vv <- lapply(
  X = vars,
  FUN = function(x, y){
    x$max_age <- y
    
    return(x)
  },
  y = maxage1
)


vs <- vv[c(1, 10, 20, 30, 40, 51)]

tmax_pred <- brtpredict(
  variables = vs,
  model = sdm_results$brt.fit[[1]],
  out_path = "output/",
  scn_id = "", # NB needs cscnid not scn_id if multiple climate scenarios with each scenario
  varset = "test_pevo_flatmax",
  species = "pevo",
  initial = FALSE,
  pll = FALSE,
  ncores = 1
)

vars <- var_set$all_vars[[1]]

## 

prec_djf1 <- vars[[1]]$prec_djf_2019

vv <- lapply(
  X = vars,
  FUN = function(x, y){
    x[[32]] <- y
    
    return(x)
  },
  y = prec_djf1
)


vs <- vv[c(1, 10, 51)]

prec_djf_pred <- brtpredict(
  variables = vs,
  model = sdm_results$brt.fit[[1]],
  out_path = "output/",
  scn_id = "", # NB needs cscnid not scn_id if multiple climate scenarios with each scenario
  varset = "test_pevo_flat",
  species = "pevo",
  initial = FALSE,
  pll = FALSE,
  ncores = 1
)



prec_djf1 <- vars[[1]]$prec_djf_2019

vv <- lapply(
  X = vars,
  FUN = function(x, y){
    x[[32]] <- y
    
    return(x)
  },
  y = prec_djf1
)

##

prec_jja1 <- vars[[1]]$prec_jja_2019

vv <- lapply(
  X = vars,
  FUN = function(x, y){
    x[[33]] <- y
    
    return(x)
  },
  y = prec_jja1
)


vs <- vv[c(1, 10, 51)]

prec_jja_pred <- brtpredict(
  variables = vs,
  model = sdm_results$brt.fit[[1]],
  out_path = "output/",
  scn_id = "", # NB needs cscnid not scn_id if multiple climate scenarios with each scenario
  varset = "test_pevo_flat_jja",
  species = "pevo",
  initial = FALSE,
  pll = FALSE,
  ncores = 1
)

##
tmax_djf1 <- vars[[1]]$tmax_djf_2019

vv <- lapply(
  X = vars,
  FUN = function(x, y){
    x[[34]] <- y
    
    return(x)
  },
  y = tmax_djf1
)


vs <- vv[c(1, 10, 51)]

tmax_djf_pred <- brtpredict(
  variables = vs,
  model = sdm_results$brt.fit[[1]],
  out_path = "output/",
  scn_id = "", # NB needs cscnid not scn_id if multiple climate scenarios with each scenario
  varset = "test_pevo_flat_tmax_djf",
  species = "pevo",
  initial = FALSE,
  pll = FALSE,
  ncores = 1
)


##

tmin_jja1 <- vars[[1]]$tmin_jja_2019

vv <- lapply(
  X = vars,
  FUN = function(x, y){
    x[[35]] <- y
    
    return(x)
  },
  y = tmin_jja1
)


vs <- vv[c(1, 10, 51)]

tmin_jja_pred <- brtpredict(
  variables = vs,
  model = sdm_results$brt.fit[[1]],
  out_path = "output/",
  scn_id = "", # NB needs cscnid not scn_id if multiple climate scenarios with each scenario
  varset = "test_pevo_flat_tmin_jja",
  species = "pevo",
  initial = FALSE,
  pll = FALSE,
  ncores = 1
)


tmin_jja1 <- vars[[1]]$tmin_jja_2019

##

vv <- lapply(
  X = vars,
  FUN = function(x){
    z <- x[[-c(32, 33, 34, 35)]]
    
    return(z)
  }
)


vs <- vv[c(1, 10, 51)]

nc_pred <- brtpredict(
  variables = vs,
  model = sdm_results$brt.fit[[1]],
  out_path = "output/",
  scn_id = "", # NB needs cscnid not scn_id if multiple climate scenarios with each scenario
  varset = "test_pevo_flat_nc",
  species = "pevo",
  initial = FALSE,
  pll = FALSE,
  ncores = 1
)

##

ny <- names(vars[[1]])

vv <- lapply(
  X = vars,
  FUN = function(x, y){
    names(x) <- y
    
    return(x)
  },
  y = ny
)


vs <- vv[c(1, 10, 51)]

ny_pred <- brtpredict(
  variables = vs,
  model = sdm_results$brt.fit[[1]],
  out_path = "output/",
  scn_id = "", # NB needs cscnid not scn_id if multiple climate scenarios with each scenario
  varset = "test_pevo_flat_ny",
  species = "pevo",
  initial = FALSE,
  pll = FALSE,
  ncores = 1
)

