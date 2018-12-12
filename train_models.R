library(caret)
library(e1071)
library(rpart)
library(gbm)

# matches_df = matches[week == 48][season == '2018-2019']
# details_df =  lastrps[,-c("Shin_RPS")]
models <- function(matches_df, details_df, 
                   model_type = c("randomforest", "multinomial"),
                   rf_metric = "Accuracy", rf_imp = TRUE,
                   ordered = FALSE){
  
  test_match_ids <- matches_df$matchId
  test_data <- details_df[matchId %in% test_match_ids]
  
  #wide_test <- widening(test_data[,-c("norm_prob")], bookiesToKeep)
  wide_test <- widening_withwinner(test_data, bookiesToKeep)
  min_date <- min(matches[matchId %in% test_match_ids]$date)
  if (length(test_match_ids) < 12) {prev_date = 180}
  if (length(test_match_ids) < 30) {prev_date = 400}
  if (length(test_match_ids) >= 30) {prev_date = 1000}
  lower_date <- as.Date(min_date) - prev_date
  train_match_ids <- matches[date < min_date][date > lower_date]$matchId
  train_data <- details_df[matchId %in% train_match_ids]
  #wide_train <- widening(train_data[,-c("norm_prob")], bookiesToKeep)
  wide_train <- widening_withwinner(train_data, bookiesToKeep)
  wide_train <- wide_train[complete.cases(wide_train)]
  wide_test <- wide_test[complete.cases(wide_test)]

  train <- wide_train[,-c("matchId", "date", "week", "season")]
  test <- wide_test[,-c("matchId", "winner", "date", "week", "season")]

  if (ordered){train$winner <- ordered(train$winner, levels = c("odd1", "oddX", "odd2"))}
    
  if (model_type == "random_forest") {
    random_forest(train, test, wide_test, metric_name = rf_metric, varimpTF = rf_imp, is_ordered = ordered)
  }
  if (model_type == "multinomial") {
    train_glmnet(train, test, wide_test)
  }
  if (model_type == "gradient_boosting") {
    gradient_boosting(train, test, wide_test)
  }
  if (model_type == "decision_tree") {
    decision_tree(train, test, wide_test)
  }
}


#train and test are necessary, requires defined lastrps (maybe use only last?)
#returns output_prob, testRPS
random_forest <- function(train, test, wide_test,
                          control_method = "repeatedcv", control_number = 10, repeat_number = 3, 
                          metric_name = "Accuracy", varimpTF = TRUE, is_ordered = FALSE){
  
  control <- trainControl(method = control_method, number = control_number, repeats = repeat_number)
  #metric can be Accuracy, ROC, RMSE, logLoss
  set.seed(7)
  mtry <- sqrt(ncol(train))
  tunegrid <- expand.grid(.mtry=mtry)
  
  if (!is_ordered){rf_default <- train(factor(convert(winner))~., data=train, method="rf", metric=metric_name, tuneGrid=tunegrid, trControl=control, importance = varimpTF)}
  else{rf_default <- train(winner~., data=train, method="rf", metric=metric_name, tuneGrid=tunegrid, trControl=control, importance = varimpTF)}
  
  varImp(rf_default)
  output_prob <- predict(rf_default, test, "prob")
  colnames(output_prob) <- c("odd1", "oddX", "odd2")
  output_prob$winner <- wide_test$winner
  output_prob$matchId <- wide_test$matchId
  setcolorder(output_prob, c("matchId", "odd1", "oddX", "odd2", "winner"))
  output_prob <- as.data.table(output_prob)[, RPS := calculate_rps(odd1, oddX, odd2, winner), by = 1:nrow(output_prob)]
  print(output_prob)
  
  testRPS <- lastrps[matchId %in% wide_test$matchId][, .(var = mean(Shin_RPS, na.rm = TRUE)), by = c("bookmaker")]
  testRPS <- testRPS[order(testRPS$var),]
  ourRPS <- mean(output_prob$RPS)
  x <- data.frame("***IE 492***", ourRPS)
  names(x) <- names(testRPS)
  testRPS <- rbind(testRPS, x)
  testRPS <- testRPS[order(testRPS$var),]
  print(testRPS)
  return(ourRPS)
}


#trace prints out that sentence
#max includes column for maximum oddtype
train_glmnet <- function(train, test, 
                         alpha=1,nlambda=50, tune_lambda=T,nofReplications=2,
                         nFolds=10,trace=T, max = F){
  set.seed(1)
  train <- train[complete.cases(train)]
  train$winner <- convert(train$winner)
  train_class <- as.numeric(train$winner)
  #adds new column for maximum observation (adds odd1 oddX or odd2)
  if (max) {
    glm_train_data$max <- do.call(pmax, glm_train_data)
    glm_test_data$max <- do.call(pmax, glm_test_data)    
  }
  if(tune_lambda){
    # to set lambda parameter, cross-validation will be performed and lambda is selected based on RPS performance
    cvindices <- generateCVRuns(train_class,nofReplications,nFolds,stratified=TRUE)
    
    # first get lambda sequence for all data
    glmnet_alldata <- glmnet(as.matrix(train), as.factor(train_class), family="multinomial", alpha = alpha, nlambda=nlambda)
    lambda_sequence <- glmnet_alldata$lambda
    
    cvresult=vector('list',nofReplications*nFolds)
    iter=1
    for(i in 1:nofReplications) {
      thisReplication=cvindices[[i]]
      for(j in 1:nFolds){
        if(trace){
          cat(sprintf('Iteration %d: Fold %d of Replication %d\n',iter,j,i))
        }
        testindices <- order(thisReplication[[j]])
        
        cvtrain <- train[-testindices]  
        cvtrainclass <- train_class[-testindices]   
        cvtest <- train[testindices]
        cvtestclass <- train_class[testindices] 
        
        inner_cv_glmnet_fit <- glmnet(data.matrix(cvtrain),as.factor(cvtrainclass),family="multinomial", alpha = alpha,lambda=lambda_sequence)
        valid_pred <- predict(inner_cv_glmnet_fit, data.matrix(cvtest), s = lambda_sequence, type = "response")
        
        #check order of predictions
        order_of_class <- attr(valid_pred,'dimnames')[[2]]
        new_order <- c(which(order_of_class=='1'),which(order_of_class=='2'),which(order_of_class=='3'))
        foldresult <- rbindlist(lapply(c(1:length(lambda_sequence)),function(x) { data.table(repl=i,fold=j,lambda=lambda_sequence[x],valid_pred[,new_order,x],result=cvtestclass)}))
        cvresult[[iter]]=foldresult
        iter=iter+1
      }
    }
    
    cvresult <- rbindlist(cvresult)
    cvresult$result <- convert(cvresult$result)
    names(cvresult) <- c("repl", "fold", "lambda", "odd1", "oddX", "odd2", "result")
    
    # creating actual targets for rps calculations
    cvresult[,pred_id:=1:.N]
    outcome_for_rps <- data.table::dcast(cvresult,pred_id~result,value.var='pred_id')
    outcome_for_rps[,pred_id:=NULL]
    outcome_for_rps[is.na(outcome_for_rps)]=0
    outcome_for_rps[outcome_for_rps>0]=1
    setcolorder(outcome_for_rps, c("odd1", "oddX", "odd2"))
    
    # calculate RPS
    cvresult <- cvresult[, RPS := calculate_rps(odd1, oddX, odd2, result), by = 1:nrow(cvresult)]
    overall_results <- data.table(cvresult[,list(repl,fold,lambda,RPS)])
    
    # summarize performance for each lambda
    overall_results_summary <- overall_results[,list(RPS=mean(RPS)),list(repl,fold,lambda)]
    
    # find best lambdas as in glmnet based on RPS
    overall_results_summary <- overall_results_summary[,list(meanRPS=mean(RPS),sdRPS=sd(RPS)),list(lambda)]
    overall_results_summary[,RPS_mean_lb := meanRPS - sdRPS]
    overall_results_summary[,RPS_mean_ub := meanRPS + sdRPS]
    
    cv_lambda_min <- overall_results_summary[which.min(meanRPS)]$lambda
    
    semin <- overall_results_summary[lambda==cv_lambda_min]$RPS_mean_ub
    cv_lambda.1se <- max(overall_results_summary[meanRPS<semin]$lambda)
    
    cvResultsSummary = list(lambda.min =cv_lambda_min, lambda.1se = cv_lambda.1se,
                            meanRPS_min=overall_results_summary[lambda==cv_lambda_min]$meanRPS,
                            meanRPS_1se=overall_results_summary[lambda==cv_lambda.1se]$meanRPS)
  }
  
  
  final_glmnet_fit <- glmnet(as.matrix(train),as.factor(train_class),family="multinomial", alpha = alpha,lambda=cvResultsSummary$lambda.min)
  predicted_probabilities <- predict(final_glmnet_fit, as.matrix(test), type = "response")
  
  #check order of predictions
  order_of_class <- attr(predicted_probabilities,'dimnames')[[2]]
  new_order <- c(which(order_of_class=='odd1'),which(order_of_class=='oddX'),which(order_of_class=='odd2'))
  
  #  final_result=data.table(test_features[,list(matchId,winner)],predicted_probabilities[,new_order,1])
  final_result <- data.table(test_features[,list(matchId,winner)],predicted_probabilities[,,])
  names(final_result) <- c("matchId", "winner", "odd1", "oddX", "odd2")
  
  return(list(predictions=final_result,cv_stats=cvResultsSummary))
  
}


gradient_boosting <- function(train, test, wide_test){
  fitControl <- trainControl(method = "cv", number = 30)
  tune_Grid <-  expand.grid(interaction.depth = 5,
                            n.trees = 1000,
                            shrinkage = 0.1,
                            n.minobsinnode = 50)
  set.seed(1234)
  fit <- train(winner ~ ., data = train,
               method = "gbm",
               trControl = fitControl,
               verbose = FALSE,
               tuneGrid = tune_Grid)

  output_prob <- predict(fit,test,type= "prob")
  colnames(output_prob) <- c("odd1", "oddX", "odd2")
  output_prob$winner <- wide_test$winner
  output_prob$matchId <- wide_test$matchId
  setcolorder(output_prob, c("matchId", "odd1", "oddX", "odd2", "winner"))
  output_prob <- as.data.table(output_prob)[, RPS := calculate_rps(odd1, oddX, odd2, winner), by = 1:nrow(output_prob)]
  print(output_prob)
  
  testRPS <- lastrps[matchId %in% wide_test$matchId][, .(var = mean(Shin_RPS, na.rm = TRUE)), by = c("bookmaker")]
  testRPS <- testRPS[order(testRPS$var),]
  ourRPS <- mean(output_prob$RPS)
  x <- data.frame("***IE 492***", ourRPS)
  names(x) <- names(testRPS)
  testRPS <- rbind(testRPS, x)
  testRPS <- testRPS[order(testRPS$var),]
  print(testRPS)
  return(ourRPS)}


decision_tree <- function(train, test, wide_test){
  set.seed(1234)
  fit <- rpart(train$winner ~ ., 
               data = train, 
               method="class",
               control = rpart.control(minsplit = 50, minbucket = 20, cp = 0.01))
  output_prob <- data.table(predict(fit,test,type= "prob"))
  colnames(output_prob) <- c("odd1", "oddX", "odd2")
  output_prob$winner <- wide_test$winner
  output_prob$matchId <- wide_test$matchId
  setcolorder(output_prob, c("matchId", "odd1", "oddX", "odd2", "winner"))
  output_prob <- as.data.table(output_prob)[, RPS := calculate_rps(odd1, oddX, odd2, winner), by = 1:nrow(output_prob)]
  print(output_prob)
  
  testRPS <- lastrps[matchId %in% wide_test$matchId][, .(var = mean(Shin_RPS, na.rm = TRUE)), by = c("bookmaker")]
  testRPS <- testRPS[order(testRPS$var),]
  ourRPS <- mean(output_prob$RPS)
  x <- data.frame("***IE 492***", ourRPS)
  names(x) <- names(testRPS)
  testRPS <- rbind(testRPS, x)
  testRPS <- testRPS[order(testRPS$var),]
  print(testRPS)
  return(ourRPS)}







bet_on <- function(){
  best_bookmaker <- testRPS$bookmaker[1]
  portfolio_df <- last[bookmaker == best_bookmaker][matchId %in% predictions[["predictions"]]$matchId][,-4]
  bookmaker_pred <- widening(portfolio_df, best_bookmaker)[,c(1,5,2,4,3)]
  our_pred <- predictions[["predictions"]]
  colnames(bookmaker_pred) <- colnames(our_pred)
  differences <- merge(our_pred, bookmaker_pred, on = c("matchId"))
  differences$odd1_diff <- differences[, odd1.x-odd1.y, by = 1:nrow(differences)]$V1
  differences$oddX_diff <- differences[, oddX.x-oddX.y, by = 1:nrow(differences)]$V1
  differences$odd2_diff <- differences[, odd2.x-odd2.y, by = 1:nrow(differences)]$V1
  we_bet_on <- convert(max.col(differences[,c(10:12)], 'first'))
  actual_outcome <- differences$winner.x
  results <- as.data.table(cbind(differences$matchId, we_bet_on, actual_outcome, matrix(1, 10, 3)))
  colnames(results) <- c("matchId", "we_bet_on", "actual_outcome", "odd", "bet_amount", "on_hand")
  
  for (i in 1:nrow(results)){
    results$odd[i] <- last(details[(matchId == results$matchId[i]) & (bookmaker == "12BET") & (oddtype == results$we_bet_on[i])])$odd
    if(results$we_bet_on[i] == results$actual_outcome[i]){
      results$on_hand[i] <- as.integer(results$bet_amount[i]) * as.double(results$odd[i])
    }
    else{
      results$on_hand[i] <- 0
    }
  }
  print(results)
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


### Bunlar fonksiyon disindaydi comment outladim simdilik
#output <- predict(rf_default, test_x)
#convert_1x2(output)
#convert(as.integer(output))
#test_features$winner
#table(convert_1x2(output), test_features$winner)
