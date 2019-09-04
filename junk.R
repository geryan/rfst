xx <- foreach(i = 1:3) %:%
        foreach(j = c(100, 1000, 10000)) %do% {
                i+j
        }

h100 <- lv_1_01[[1]][[18]]
h125 <- lv_1_01[[26]][[18]]
h150 <- lv_1_01[[51]][[18]]

h400 <- lv_4_01[[1]][[18]]
h425 <- lv_4_01[[26]][[18]]
h450 <- lv_4_01[[51]][[18]]

h800 <- lv_4_01[[1]][[18]]
h825 <- lv_4_01[[26]][[18]]
h850 <- lv_4_01[[51]][[18]]


lp(h100, h125, h150)

lp(h400, h425, h450)


lp(h800, h825, h850)


f100 <- lv_1_01[[1]][[19]]
f125 <- lv_1_01[[26]][[19]]
f150 <- lv_1_01[[51]][[19]]

f400 <- lv_4_01[[1]][[19]]
f425 <- lv_4_01[[26]][[19]]
f450 <- lv_4_01[[51]][[19]]

f800 <- lv_4_01[[1]][[19]]
f825 <- lv_4_01[[26]][[19]]
f850 <- lv_4_01[[51]][[19]]

aa <- h150

aa[aa < 5] <- NA


aa <- rst.op(input1 = h125, op = "pb", proj_mask = ch_mask, filename = "test.grd", layernames = "pb")

bb  <- rst.op(input1 = h125, op = "harvest", proj_mask = ch_mask, filename = "test2.grd", layernames = "logging")

lapply(get(sprintf("lv_%s_%s",
                   scn_list[i],
                   rep_list[j])),
       FUN = function(x){x[["harvest"]]})


aa <- get.dist(fire_history = ch_fire_history,
               logging_history = ch_logging_history,
               fs = lapply(lv_1_01, FUN = function(x){x[["firesev"]]}),
               ha = lapply(lv_1_01, FUN = function(x){x[["harvest"]]}),
               out_path = "output/landscape_vars",
               scn_id = "1_01",
               proj_mask = ch_mask,
               timesteps = 5,
               year0 = 2019)
