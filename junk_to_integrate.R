saveGIF({
  for (i in 1:51){
    plot(s1_p_gg_1[[i]], zlim = c(0,1))
    title(main = paste0("Year ", i-1))
  }
}, interval = 0.2, movie.name = "pred_gg_s1_1.gif")

saveGIF({
  for (i in 1:51){
    plot(s1_p_gg_1_ps[[i]], zlim = c(0,1))
    title(main = paste0("Year ", i-1))
  }
}, interval = 0.2, movie.name = "pred_gg_s1_1ps.gif")

saveGIF({
  for (i in 1:51){
    plot(s1_p_gg_4[[i]], zlim = c(0,1))
    title(main = paste0("Year ", i-1))
  }
}, interval = 0.1, movie.name = "pred_gg_s1_4.gif")


saveGIF({
  for (i in 1:51){
    plot(s1_p_gg_4_ps[[i]], zlim = c(0,1))
    title(main = paste0("Year ", i-1))
  }
}, interval = 0.2, movie.name = "pred_gg_s1_4ps.gif")


saveGIF({
  for (i in 1:51){
    plot(s2_p_gg_1[[i]], zlim = c(0,1))
    title(main = paste0("Year ", i-1))
  }
}, interval = 0.2, movie.name = "pred_gg_s2_1.gif")

saveGIF({
  for (i in 1:51){
    plot(s2_p_gg_1_ps[[i]], zlim = c(0,1))
    title(main = paste0("Year ", i-1))
  }
}, interval = 0.2, movie.name = "pred_gg_s2_1ps.gif")

saveGIF({
  for (i in 1:51){
    plot(s3_p_gg_1[[i]], zlim = c(0,1))
    title(main = paste0("Year ", i-1))
  }
}, interval = 0.2, movie.name = "pred_gg_s3_1.gif")

saveGIF({
  for (i in 1:51){
    plot(s3_p_gg_1_ps[[i]], zlim = c(0,1))
    title(main = paste0("Year ", i-1))
  }
}, interval = 0.2, movie.name = "pred_gg_s3_1ps.gif")

saveGIF({
  for (i in 1:51){
    plot(s1_p_lb_1[[i]], zlim = c(0,1))
    title(main = paste0("Year ", i-1))
  }
}, interval = 0.2, movie.name = "pred_lb_s1_1.gif")

saveGIF({
  for (i in 1:51){
    plot(s1_p_lb_1_ps[[i]], zlim = c(0,1))
    title(main = paste0("Year ", i-1))
  }
}, interval = 0.2, movie.name = "pred_lb_s1_1ps.gif")

saveGIF({
  for (i in 1:51){
    plot(s1_p_lb_3[[i]], zlim = c(0,1))
    title(main = paste0("Year ", i-1))
  }
}, interval = 0.2, movie.name = "pred_lb_s1_3.gif")

saveGIF({
  for (i in 1:51){
    plot(s1_p_lb_3_ps[[i]], zlim = c(0,1))
    title(main = paste0("Year ", i-1))
  }
}, interval = 0.2, movie.name = "pred_lb_s1_3ps.gif")


saveGIF({
  for (i in 1:51){
    plot(s2_p_lb_1[[i]], zlim = c(0,1))
    title(main = paste0("Year ", i-1))
  }
}, interval = 0.2, movie.name = "pred_lb_s2_1.gif")

saveGIF({
  for (i in 1:51){
    plot(s2_p_lb_1_ps[[i]], zlim = c(0,1))
    title(main = paste0("Year ", i-1))
  }
}, interval = 0.2, movie.name = "pred_lb_s2_1ps.gif")

saveGIF({
  for (i in 1:51){
    plot(s3_p_lb_1[[i]], zlim = c(0,1))
    title(main = paste0("Year ", i-1))
  }
}, interval = 0.2, movie.name = "pred_lb_s3_1.gif")

saveGIF({
  for (i in 1:51){
    plot(s3_p_lb_1_ps[[i]], zlim = c(0,1))
    title(main = paste0("Year ", i-1))
  }
}, interval = 0.2, movie.name = "pred_lb_s3_1ps.gif")


###
pop0_lb_1_1 <- colSums(getValues(p0_lb_1_1), na.rm = TRUE)
pop0_lb_1_1_ps <- colSums(getValues(p0_lb_1_1_ps), na.rm = TRUE)
pop0_lb_1_1_pl <- colSums(getValues(p0_lb_1_1_pl), na.rm = TRUE)
pop0_lb_1_1_ph <- colSums(getValues(p0_lb_1_1_ph), na.rm = TRUE)
pop0_lb_1_1_ps_ph <- colSums(getValues(p0_lb_1_1_ps_ph), na.rm = TRUE)
pop0_lb_1_1_ps_pl <- colSums(getValues(p0_lb_1_1_ps_pl), na.rm = TRUE)
pop0_lb_1_1_ps_fl <- colSums(getValues(p0_lb_1_1_ps_fl), na.rm = TRUE)
pop0_lb_1_3 <- colSums(getValues(p0_lb_1_3), na.rm = TRUE)
pop0_lb_1_3_ps <- colSums(getValues(p0_lb_1_3_ps), na.rm = TRUE)
pop0_lb_2_1 <- colSums(getValues(p0_lb_2_1), na.rm = TRUE)
pop0_lb_2_1_ps <- colSums(getValues(p0_lb_2_1_ps), na.rm = TRUE)
pop0_lb_3_1 <- colSums(getValues(p0_lb_2_1), na.rm = TRUE)
pop0_lb_3_1_ps <- colSums(getValues(p0_lb_2_1_ps), na.rm = TRUE)

popsize_gg_1_1             
popsize_gg_1_1_dl
popsize_gg_1_1_fl
popsize_gg_1_1_ps
popsize_gg_1_1_ps_dl
popsize_gg_1_1_ps_fl
popsize_gg_1_4
popsize_gg_1_4_ps
popsize_gg_2_1
popsize_gg_2_1_ps
popsize_gg_3_1
popsize_gg_3_1_ps



popsize_lb_1_1
popsize_lb_1_1_ph
popsize_lb_1_1_pl
popsize_lb_1_1_ps
popsize_lb_1_1_ps_ph
popsize_lb_1_1_ps_pl
popsize_lb_1_3
popsize_lb_1_3_ps
popsize_lb_2_1
popsize_lb_2_1_ps
popsize_lb_3_1
popsize_lb_3_1_ps


comp_gg_sm <- list(list(pop0_gg_1_1,    popsize_gg_1_1,    "BAU"),
                   list(pop0_gg_1_1_ps, popsize_gg_1_1_ps, "BAU_ps"),
                   list(pop0_gg_2_1,    popsize_gg_2_1,    "RCP8.5"),
                   list(pop0_gg_2_1_ps, popsize_gg_2_1_ps, "RCP8.5_ps"),
                   list(pop0_gg_3_1,    popsize_gg_3_1,    "NoCC"),
                   list(pop0_gg_3_1_ps, popsize_gg_3_1_ps, "NoCCps"),
                   list(pop0_gg_1_4,    popsize_gg_1_4,    "BAU-alt"),
                   list(pop0_gg_1_4_ps, popsize_gg_1_4_ps, "BAU-alt_ps"))

comp_lb_sm <- list(list(pop0_lb_1_1,    popsize_lb_1_1,    "BAU"),
                   list(pop0_lb_1_1_ps, popsize_lb_1_1_ps, "BAU_ps"),
                   list(pop0_lb_2_1,    popsize_lb_2_1,    "RCP8.5"),
                   list(pop0_lb_2_1_ps, popsize_lb_2_1_ps, "RCP8.5_ps"),
                   list(pop0_lb_3_1,    popsize_lb_3_1,    "NoCC"),
                   list(pop0_lb_3_1_ps, popsize_lb_3_1_ps, "NoCC_ps"),
                   list(pop0_lb_1_3,    popsize_lb_1_3,    "BAU-alt"),
                   list(pop0_lb_1_3_ps, popsize_lb_1_3_ps, "BAU-alt_ps"))


comp_gg_lh <- list(list(pop0_gg_1_1,       popsize_gg_1_1,       "BAU"),
                   list(pop0_gg_1_1_ps,    popsize_gg_1_1_ps,    "BAU_ps"),
                   list(pop0_gg_1_1_fl,    popsize_gg_1_1_fl,    "fecund-l"),
                   list(pop0_gg_1_1_ps_fl, popsize_gg_1_1_ps_fl, "fecund-l_ps"),
                   list(pop0_gg_1_1,      popsize_gg_1_1_dl,       "dispers-l"),
                   list(pop0_gg_1_1_ps,    popsize_gg_1_1_ps_dl,    "dispers-l_ps"))

comp_lb_lh <- list(list(pop0_lb_1_1,       popsize_lb_1_1,       "BAU"),
                   list(pop0_lb_1_1_ps,    popsize_lb_1_1_ps,    "BAU_ps"),
                   list(pop0_lb_1_1_ph,    popsize_lb_1_1_ph,    "pop-h"),
                   list(pop0_lb_1_1_ps_ph, popsize_lb_1_1_ps_ph, "pop-h_ps"),
                   list(pop0_lb_1_1_pl,    popsize_lb_1_1_pl,    "pop-l"),
                   list(pop0_lb_1_1_ps_pl, popsize_lb_1_1_ps_pl, "pop-l_ps"))


ce_gg_sm <- compareemp(comp_gg_sm)
ce_lb_sm <- compareemp(comp_lb_sm)
ce_gg_lh <- compareemp(comp_gg_lh)
ce_lb_lh <- compareemp(comp_lb_lh)

png(file = "output/plots/ce_gg_sm.png",
    width = width.png,
    height = height.png, res = res)
ce_gg_sm
dev.off()

png(file = "output/plots/ce_gg_lh.png",
    width = width.png,
    height = height.png, res = res)
ce_gg_lh
dev.off()

png(file = "output/plots/ce_lb_sm.png",
    width = width.png,
    height = height.png, res = res)
ce_lb_sm
dev.off()

png(file = "output/plots/ce_lb_lh.png",
    width = width.png,
    height = height.png, res = res)
ce_lb_lh
dev.off()




