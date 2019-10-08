model_report <- function(modeltype = "GLMNET", n_of_inputs = n, Comment = "Basic + Shin",TrainSet, TestSet, trainStart, testStart, predictions, testRPS){
  
  predict <- predictions[["predictions"]]
  predict <- predict[, RPS := calculate_rps(odd1,oddX,odd2,winner), by = 1:nrow(predict)]
  averageRPS <- mean(predict$RPS)
  
  TestRPS_min <- min(testRPS$var)
  TestRPS_ave <- mean(testRPS$var)

  df <- read.csv("Model_Outputs.csv")
 
  df[nrow(df) + 1,] = list(modeltype,n_of_inputs,Comment,TrainSet,TestSet,trainStart,testStart,round(averageRPS,5),round(TestRPS_min,5),round(TestRPS_ave,5))
  write.csv(df, file = "Model_Outputs.csv", row.names = FALSE, quote = FALSE)
  
  averageRPS
}

model_report_next_week <- function(modeltype = "GLMNET", n_of_inputs = n, Comment = "Basic + Shin",TrainSet, TestSet, trainStart, testStart, predictions, testRPS){
  
  predict <- predictions[["predictions"]]
  predict[,c("winner") := NULL]
  merge(predict, matches[,c("matchId", "winner")], on = "matchId")
  predict <- predict[, RPS := calculate_rps(odd1,oddX,odd2,winner), by = 1:nrow(predict)]
  averageRPS <- mean(predict$RPS)
  
  TestRPS_min <- min(testRPS$var)
  TestRPS_ave <- mean(testRPS$var)
  
  df <- read.csv("Model_Outputs.csv")
  
  df[nrow(df) + 1,] = list(modeltype,n_of_inputs,Comment,TrainSet,TestSet,trainStart,testStart,round(averageRPS,5),round(TestRPS_min,5),round(TestRPS_ave,5))
  write.csv(df, file = "Model_Outputs.csv", row.names = FALSE, quote = FALSE)
  
  averageRPS
}
