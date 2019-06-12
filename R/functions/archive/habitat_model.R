library(raster)
library(viridis)
library(pROC)
library(boot)
library(caret)
library(ROCR)
library(cvTools)
library(randomForest)
library(dismo)
library(gbm)
source("R/functions.R")

# Load modelling datasets
load(file = "data/gg_model_datasets")

########### GLM ##################
gg_glm <- glm(formula = PA ~ PROP_WF + PROP_OG + PROP_TF, family=binomial(link = "logit"), data = gg_model_data)  # Fit regression model

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
(gg_rf <- randomForest(formula = as.factor(PA) ~ ., data = gg_model_data, mtry=2, importance = TRUE, sampsize = 50))  #Fit random forest model
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

gg_brt <- gbm.step(data = gg_model_data, gbm.x = 2:4, gbm.y = 1, family = "bernoulli", tree.complexity = 2, learning.rate = 0.15, bag.fraction = 0.5, prev.stratify = FALSE) #Create boosted regression tree model
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

########### Prediction and Mapping ##################
load(file="data/gg_env_vars")

gg_pred_map <- predict(gg_env_vars, gg_glm, type="response")
plot(gg_pred_map)
writeRaster(gg_pred_map, "output/gg_pred_map_glm.tif", overwrite = TRUE)

gg_pred_map <- 1 - predict(gg_env_vars, gg_rf, type="prob")
plot(gg_pred_map)
writeRaster(gg_pred_map, "output/gg_pred_map_rf.tif", overwrite = TRUE)

gg_pred_map <- predict(gg_env_vars, gg_brt, type="response", n.trees=gg_brt$gbm.call$best.trees)
plot(gg_pred_map)
writeRaster(gg_pred_map, "output/gg_pred_map_brt.tif", overwrite = TRUE)

gg_pred_map_t <- gg_pred_map
gg_pred_map_t[gg_pred_map_t < .9] <- NA
plot(gg_pred_map_t)
