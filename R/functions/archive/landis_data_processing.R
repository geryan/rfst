library(raster)
library(rgdal)
library(rgeos)
library(dismo)
library(viridis)
library(foreach)
library(maptools)

# Load spatial mask layer
ch_rst <- raster("working/eco_v12.img")

# Gather spatial information from mask layer
ch_res <- res(ch_rst)
ch_extent <- extent(ch_rst)
ch_proj <- ch_rst@crs

# Create clipping mask that is X kilometers larger than extent in both directions
km_offset <- 10
ch_extent_offset <- extent(ch_rst) + km_offset*(1000)

# Create window for focal operations
buffer_radius <- 564
window <- focalWeight(raster(ncols=11, nrows=11, xmn=0),
                      buffer_radius,
                      type = 'circle')

# Load and process raw spatial data
biomass_tot <- ch_rst
biomass_tot[] <- getValues(raster("working/bio-TotalBiomass-1.img"))
plot(biomass_tot)

biomass_eucacype <- biomass_eucadalr <- biomass_eucadive <- biomass_eucaradi <- biomass_eucaregn <- biomass_eucavimi <- ch_rst

biomass_eucacype[] <- getValues(raster("working/bio-eucacype-1.img"))
biomass_eucadalr[] <- getValues(raster("working/bio-eucadalr-1.img"))
biomass_eucadive[] <- getValues(raster("working/bio-eucadive-1.img"))
biomass_eucaradi[] <- getValues(raster("working/bio-eucaradi-1.img"))
biomass_eucaregn[] <- getValues(raster("working/bio-eucaregn-1.img"))
biomass_eucavimi[] <- getValues(raster("working/bio-eucavimi-1.img"))

biomass_vec <- getValues(biomass_tot)

biomass_eucs <- stack(biomass_eucacype, biomass_eucadalr, biomass_eucadive, biomass_eucaradi, biomass_eucaregn, biomass_eucavimi)

biomass_eucs_mat <- getValues(biomass_eucs)

# eucacype_vec <- getValues(biomass_eucacype)
# eucadalr_vec <- getValues(biomass_eucadalr)
# eucadive_vec <- getValues(biomass_eucadive)
# eucaradi_vec <- getValues(biomass_eucaradi)
# eucaregn_vec <- getValues(biomass_eucaregn)
# eucavimi_vec <- getValues(biomass_eucavimi)

idx_euc <- apply(biomass_eucs_mat, 1, function(x) unname(which(x == max(x))))
idx_euc <- unlist(lapply(idx_euc, function(x) if (length(x) == 1) x else 0))

dominant_eucs <- stack(foreach (i = seq_len(nlayers(biomass_eucs))) %do% {
  temp <- biomass_eucs[[i]]
  temp[] <- 0
  temp[idx_euc == i] <- 1
  #names(temp) <- names(biomass_eucs[[i]])
  temp
})

names(dominant_eucs) <- gsub(".img", "", list.files("working", pattern = "bio-euca"))

plot(dominant_eucs)

for (i in seq_len(nlayers(dominant_eucs))) {
  temp <- focal(dominant_eucs[[i]], window, na.rm = TRUE)
  temp[is.na(temp)] <- 0
  assign(paste0("PROP_", toupper(strsplit(names(dominant_eucs[[i]]), '[.]')[[1]][2])), temp)
  writeRaster(temp, file = paste0("working/PROP_", toupper(strsplit(names(dominant_eucs[[i]]), '[.]')[[1]][2]), "-00", ".tif"), overwrite = TRUE)
}

max_age_rst <- ch_rst
max_age_rst[] <- getValues(raster("working/AGE-MAX-1.img"))
max_age_rst[max_age_rst < 200] <- 0
max_age_rst[max_age_rst != 0] <- 1

max_age_rst <- focal(max_age_rst, window, na.rm = TRUE)
PROP_OLD <- max_age_rst
writeRaster(PROP_OLD, file = "PROP_OLD-00.tif", overwrite = TRUE)

gg_env_vars <- stack(PROP_OLD, PROP_EUCACYPE, PROP_EUCADALR, PROP_EUCADIVE, PROP_EUCARADI, PROP_EUCAREGN, PROP_EUCAVIMI)
names(gg_env_vars) <- c("PROP_OLD", "PROP_EUCACYPE", "PROP_EUCADALR", "PROP_EUCADIVE", "PROP_EUCARADI", "PROP_EUCAREGN", "PROP_EUCAVIMI")

# Load in Greater Glider presence and absence data and reproject
gg_pres_abs <- read.csv(file="data/tabular/vicAtlasGGpresabs.csv")

coords <- data.frame(X = gg_pres_abs$LongitudeGDA94,
                     Y = gg_pres_abs$LatitudeGDA94)
coordinates(coords) <- c("X", "Y")
proj4string(coords) <- CRS("+init=epsg:4283") # GDA94
GG_POINTS <- data.frame("PA" = gg_pres_abs[, 1], spTransform(coords, ch_proj)@coords)

# Sample covariate values - be sure to use select correct data
cov_sample <- as.data.frame(extract(gg_env_vars, GG_POINTS[, 2:3]))

# Create modelling dataset - be sure to use select correct data
gg_model_data <- cbind("PA" = GG_POINTS[, 1], cov_sample)

# Check for NA values and omit accordingly:
sum(is.na(gg_model_data))
gg_model_data <- na.omit(gg_model_data)

# Review pairwise correlation of covariates:
cor(gg_model_data[, 2:8])

# Review covariate data types
str(gg_model_data)

# Review data ranges
apply(gg_model_data[, 2:8], 2, range)

# Split the data into training and testing data
set.seed(123)
gg_sample <- sample(nrow(gg_model_data), round(0.75 * nrow(gg_model_data))) # the row indexes of our 75% sample
gg_model_data_75 <- gg_model_data[gg_sample, ] # 75% of the data used to train the model
gg_model_data_25 <- gg_model_data[-gg_sample, ] # 25% of the data used to test the model

# Write the modelling data out to csv files and as an R object
write.csv(gg_model_data, file = "data/gg_model_data.csv")
write.csv(gg_model_data_25, file = "data/gg_model_data_25.csv")
write.csv(gg_model_data_75, file = "data/gg_model_data_75.csv")
save("gg_model_data", "gg_model_data_25", "gg_model_data_75", file = "data/gg_model_datasets")

########### GLM ##################
gg_glm <- glm(formula = PA ~ ., family=binomial(link = "logit"), data = gg_model_data)  # Fit regression model

summary(gg_glm)  # Examine fit of regression model

dev(gg_glm)  # Report reduction in deviance

roc(gg_model_data[, 1],
    as.vector(predict(gg_glm, gg_model_data, type = "response")),
    plot = TRUE,
    legacy.axes = TRUE)

cv.glm(gg_model_data, gg_glm, K = 10)$delta

varImp(gg_glm)

calc_AUC <- function(pred, act) {
  u <- prediction(act, pred)
  return(performance(u, "auc")@y.values[[1]])
}

cvFit(gg_glm, data = gg_model_data, y = gg_model_data$PA, 
      cost = calc_AUC, predictArgs = "response", K = 10)

# Fit the model using the 75% training dataset
gg_glm_75 <- glm(formula = PA ~ ., family=binomial(link = "logit"), data = gg_model_data_75)

# Assess how well the model predicts the 25% of data that were held out
roc(gg_model_data_25[, 1],
    as.vector(predict(gg_glm_75, gg_model_data_25, type = "response")),
    plot = TRUE,
    legacy.axes = TRUE,
    col = "darkred")

# Add the model performance using all of the data (black dashed line)
plot.roc(gg_model_data[, 1],
         as.vector(predict(gg_glm, gg_model_data, type = "response")),
         legacy.axes = TRUE,
         lty = 2,
         add =TRUE)

val.pred.glm <- predict(gg_glm_75, gg_model_data_25, type="link")  #Make predictions with regression model fit on link scale

summary(glm(gg_model_data_25$PA ~ val.pred.glm, family = binomial(link = "logit")))  #slope is close to one therefore model is well calibrated to held-out data



########### Random Forest ##################
(gg_rf <- randomForest(formula = as.factor(PA) ~ ., data = gg_model_data, mtry=5, importance = TRUE, sampsize = 100))  #Fit random forest model
gg_rf$importance

predictions=as.vector(gg_rf$votes[,2])
pred=prediction(predictions,gg_model_data$PA)

perf_AUC=performance(pred,"auc") #Calculate the AUC value
AUC=perf_AUC@y.values[[1]]

perf_ROC=performance(pred,"tpr","fpr") #plot the actual ROC curve
plot(perf_ROC, main="ROC plot")
text(0.5,0.5,paste("AUC = ",format(AUC, digits=5, scientific=FALSE)))

roc(gg_model_data[, 1],
    as.vector(gg_rf$votes[, 2]),
    plot = TRUE,
    legacy.axes = TRUE)

calc_AUC <- function(pred, act) {
  u <- prediction(act, pred)
  return(performance(u, "auc")@y.values[[1]])
}

cvFit(gg_rf, data = gg_model_data, y = gg_model_data$PA, 
      cost = calc_AUC, predictArgs = "prob", K = 10)

########### Boosted Regression Trees ##################
set.seed(333) #Set random seed to make results of gradient boosted regressions identical for each run

gg_brt <- gbm.step(data = gg_model_data, gbm.x = 2:8, gbm.y = 1, family = "bernoulli", tree.complexity = 5, learning.rate = 0.15, bag.fraction = 0.5, prev.stratify = FALSE) #Create boosted regression tree model
summary(gg_brt)

#Report reduction in deviance on null model (percent of error explained by model)
(brt.devexp <- paste(round(((gg_brt[["self.statistics"]][["mean.null"]] - gg_brt[["cv.statistics"]][["deviance.mean"]])/gg_brt[["self.statistics"]][["mean.null"]])*100,2)," +/- ",round((gg_brt[["cv.statistics"]][["deviance.se"]]/gg_brt[["self.statistics"]][["mean.null"]])*100,2),sep=""))

#Report discrimination performance of model in area under receiver operator characteristic curve
(brt.roc <- paste(round(gg_brt[["cv.statistics"]][["discrimination.mean"]],2)," +/- ",round(gg_brt[["cv.statistics"]][["discrimination.se"]],2),sep=""))

summary(glm(gg_model_data$PA ~ predict(gg_brt, gg_model_data, n.trees=gg_brt$gbm.call$best.trees, type="link"), family = binomial(link = "logit")))  #slope is close to one therefore model is well calibrated to held-out data

preds <- predict.gbm(gg_brt, gg_model_data, n.trees=gg_brt$gbm.call$best.trees, type="response")
calc.deviance(obs = gg_model_data[, 1], pred = preds, calc.mean = TRUE)
d <- cbind(gg_model_data[, 1], preds)
pres <- d[d[,1]==1, 2]
abs <- d[d[,1]==0, 2]
e <- evaluate(p=pres, a=abs)
e

gg_pred_glm <- predict(gg_env_vars, gg_glm, type="response")
plot(gg_pred_glm)

gg_pred_rf <- 1 - predict(gg_env_vars, gg_rf, type="prob")
plot(gg_pred_rf)

gg_pred_brt <- predict(gg_env_vars, gg_brt, type="response", n.trees=gg_brt$gbm.call$best.trees)
plot(gg_pred_brt)


for (i in seq_len(nlayers(dominant_eucs))) {
  temp <- focal(dominant_eucs[[i]], window, na.rm = TRUE)
  temp[is.na(temp)] <- 0
  writeRaster(temp, file = paste0("PROP_", toupper(strsplit(names(dominant_eucs[[i]]), '[.]')[[1]][2]), "-", sprintf("%02d", i), ".tif"))
}
