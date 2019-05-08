# The function below is adapted from Reineking Bjoern and based on the Biomod tutorial by Wilfried Thuiller 
varImpMaxnet <- function(model, varnames, data, n=100, link = "cloglog") {
  varImp <- numeric(length(varnames))
  names(varImp) <- varnames
  base.pred <- predict(model, newdata=data, type = link)
  for (var in varnames) {
    varimp.test <- data
    tmp <- numeric(n)
    for (i in 1:n) {
      varimp.test[[var]] <- sample(varimp.test[[var]])
      tmp.pred <- predict(model, newdata=varimp.test, type = link)
      tmp[i] <- cor(base.pred, tmp.pred)
    }
    varImp[var] <- mean(tmp)
  }
  varImpI <- 1 - varImp
  RelvarImp <- sort(varImpI/sum(varImpI))
  print(RelvarImp)
  par(mar=c(5.1, 7.1, 2.1, 2.1))
  barplot(RelvarImp,
          horiz = TRUE,
          las=1,
          xlim = c(0,round(max(RelvarImp),1)),
          xlab = "Relative Importance of Variable")
  par(mar=c(5.1, 4.1, 4.1, 2.1))
}

# roc <- function (obsdat, preddat) {
#   if (length(obsdat) != length(preddat)) 
#     stop("obs and preds must be equal lengths")
#   n.x <- length(obsdat[obsdat == 0])
#   n.y <- length(obsdat[obsdat == 1])
#   xy <- c(preddat[obsdat == 0], preddat[obsdat == 1])
#   rnk <- rank(xy)
#   roc <- ((n.x * n.y) + ((n.x * (n.x + 1))/2) - sum(rnk[1:n.x]))/(n.x * n.y)
#   return(round(roc, 4))
# }

dev <- function (model) {
  round(((model$null.deviance - model$deviance) / model$null.deviance)*100, 2)
}


calc_AUC <- function(pred, act) {
  u <- prediction(act, pred)
  return(performance(u, "auc")@y.values[[1]])
}