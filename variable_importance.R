set.seed(1234)
source("varimp_reference.R")

varimp_function <- function(train) {
  is.factor(train$winner)
  is.ordered(train$winner)
  train$winner <- ordered(train$winner, levels = c("odd1", "oddX", "odd2"))
  train_small <- train[,c(1:12,46)]
  
  RF_ordinal <- cforest(winner ~ ., data = train_small, control = cforest_unbiased(ntree = 1000))
  RF_ordinal@responses@is_ordinal
  
  RF_classification <- cforest(factor(winner, ordered = FALSE) ~ ., data = train, control = cforest_unbiased(ntree = 1000))
  RF_classification@responses@is_nominal
  
  
  ER_VI <- varimp(RF_ordinal)
  RPS_VI <- varimpRPS(RF_ordinal)
  MAE_VI <- varimpMAE(RF_ordinal)
  MSE_VI <- varimpMSE(RF_ordinal)
  
  RPS_VI_class <- varimpRPS(RF_classification)
  
  
  par(mfrow = c(1, 4))
  barplot(ER_VI, ylab = "Error rate based variable importance", las = 2)
  barplot(RPS_VI, ylab = "RPS-based variable importance", las = 2)
  barplot(MAE_VI, ylab = "MAE-based variable importance", las = 2)
  barplot(MSE_VI, ylab = "MSE-based variable importance", las = 2)
  mtext(side = 3, text = "Variable importance by different measures", outer = TRUE, line = -2)
  
}


















