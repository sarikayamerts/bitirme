#requires matches, last, wide_last

library(caret)
library(e1071)

train_model <- function(weekNumber, seasonYear){
  test_match_ids <- matches[week == weekNumber][season == seasonYear]$matchId
  test_data <- last[matchId %in% test_match_ids]
  wide_test <- widening(test_data[,-4], bookiesToKeep)
  test_features <- wide_test
  min_date <- min(matches[matchId %in% test_match_ids]$date)
  train_features <- wide_last[date < min_date]
  

  train <- train_features[,-c("matchId", "date", "week", "season")]
  #train$winner <- factor(convert(train$winner))
  train <- train[complete.cases(train)]
  test_x <- test_features[,-c("matchId", "winner", "date", "week", "season")]
  
  control <- trainControl(method="repeatedcv", number=10, repeats=3, search = "random")
  seed <- 7
  metric <- "Accuracy"
  set.seed(seed)
  mtry <- sqrt(ncol(train))
  tunegrid <- expand.grid(.mtry=mtry)
  rf_default <- train(factor(convert(winner))~., data=train, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
  
  
  output_prob <- predict(rf_default, test_x, "prob")
  colnames(output_prob) <- c("odd1", "oddX", "odd2")
  output_prob$winner <- test_features$winner
  
  output_prob <- as.data.table(output_prob)[, RPS := calculate_rps(odd1, oddX, odd2, winner), by = 1:nrow(output_prob)]
  
  testRPS <- lastrps[matchId %in% test_match_ids][, .(var = mean(Shin_RPS, na.rm = TRUE)), by = c("bookmaker")]
  testRPS <- testRPS[order(testRPS$var),]
  ourRPS <- mean(output_prob$RPS)
  x <- data.frame("***IE 492***", ourRPS)
  names(x) <- names(testRPS)
  testRPS <- rbind(testRPS, x)
  testRPS <- testRPS[order(testRPS$var),]
  print(testRPS)
}

#convert regression to classification
convert_1x2 <- function(arr){
  n = length(arr)
  arr_copy <- copy(arr)
  for (i in 1:n){
    if (arr_copy[i] < 1.80) {arr_copy[i] <- "odd1"}
    else if ((arr_copy[i] >= 1.80) & (arr_copy[i] <= 2.20)) {arr_copy[i] <- "oddX"}
    else {arr_copy[i] <- "odd2"}
  }
  as.vector(arr_copy)
}


output <- predict(rf_default, test_x)
convert_1x2(output)
convert(as.integer(output))
test_features$winner
table(convert_1x2(output), test_features$winner)
