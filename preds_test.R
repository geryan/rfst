preds_gyle <- brtpredict(
  variables = var_set$all_vars[[1]][1:5],
  model = sdm_results$brt.fit[[1]],
  out_path = "output",
  scn_id = "TEST_brick",
  species = "test",
  varset = "XX",
  initial = FALSE,
  pll = TRUE,
  ncores = 5
)

ip_gyle <- brtpredict(
       variables = var_set$all_vars[[1]],
       model = sdm_results$brt.fit[[1]],
       out_path = "output",
       scn_id = "TEST",
       species = "test",
       varset = "XX",
       initial = TRUE
   )

ip_pevo <- brtpredict(
  variables = var_set$all_vars[[2]],
  model = sdm_results$brt.fit[[2]],
  out_path = "output",
  scn_id = "TEST2",
  species = "test",
  varset = "XX",
  initial = TRUE
)

ip_peau <- brtpredict(
  variables = var_set$all_vars[[3]],
  model = sdm_results$brt.fit[[3]],
  out_path = "output",
  scn_id = "TEST3",
  species = "test",
  varset = "XX",
  initial = TRUE
)

ip_smle <- brtpredict(
  variables = var_set$all_vars[[4]],
  model = sdm_results$brt.fit[[4]],
  out_path = "output",
  scn_id = "TEST4",
  species = "test",
  varset = "XX",
  initial = TRUE
)

ip_tyte <- brtpredict(
  variables = var_set$all_vars[[5]],
  model = sdm_results$brt.fit[[5]],
  out_path = "output",
  scn_id = "TEST5",
  species = "test",
  varset = "XX",
  initial = TRUE
)

ip_vava <- brtpredict(
  variables = var_set$all_vars[[6]],
  model = sdm_results$brt.fit[[6]],
  out_path = "output",
  scn_id = "TEST6",
  species = "test",
  varset = "XX",
  initial = TRUE
)

lp(ip_gyle)
lp(ip_pevo)
lp(ip_peau)
lp(ip_smle)
lp(ip_tyte)
lp(ip_vava)


lp(stack(qq,
ip_pevo,
ip_peau,
ip_smle,
ip_tyte,
ip_vava))