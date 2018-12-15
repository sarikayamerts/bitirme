# Continuation Ratio Model for Ordinal Data - vglmContRatio
# Cumulative Probability Model for Ordinal Data - vglmCumulative
# Penalized Ordinal Regression - ordinalNet

rpsCaret<- function (data, lev = NULL, model = NULL) { 
  require(verification)
  if (!all(levels(data[, "pred"]) == levels(data[, "obs"]))) 
    stop("levels of observed and predicted data do not match")
  rownames(data) <- NULL
  obs <- as.vector(as.numeric(data$obs))
  pred <- as.matrix(subset(data, select = levels(data$obs)))
  rpsObject <- verification::rps(obs, pred)
  out<- (-1) * rpsObject$rps 
  names(out) <- c("rpsScore")
  out 
}
environment(rpsCaret) <- asNamespace('caret')

#unique(matches[season == '2018-2019']$week)
# matches_df = matches[week == 44][season == '2018-2019']
# details_df = lastrps[,-c("Shin_RPS")]
models <- function(matches_df, details_df, fit_model = NULL){
  
  test_match_ids <- matches_df$matchId
  test_data <- details_df[matchId %in% test_match_ids]
  
  #wide_test <- widening(test_data[,-c("norm_prob")], bookiesToKeep)
  wide_test <- widening_withwinner(test_data, bookiesToKeep)
  min_date <- min(matches[matchId %in% test_match_ids]$date)
  #if (length(test_match_ids) < 12) {prev_date = 180}
  if (length(test_match_ids) < 30) {prev_date = 365}
  lower_date <- as.Date(min_date) - prev_date
  train_match_ids <- matches[date < min_date][date > lower_date]$matchId
  train_data <- details_df[matchId %in% train_match_ids]
  #train_data <- train_data[!(matchId %in% risky_matches)]
  #wide_train <- widening(train_data[,-c("norm_prob")], bookiesToKeep)
  wide_train <- widening_withwinner(train_data, bookiesToKeep)
  wide_train <- wide_train[complete.cases(wide_train)]
  wide_test <- wide_test[complete.cases(wide_test)]

  train <- wide_train[,-c("matchId", "date", "week", "season")]
  test <- wide_test[,-c("matchId", "winner", "date", "week", "season")]

  if (ordered){train$winner <- ordered(train$winner, levels = c("odd1", "oddX", "odd2"))}
    
  if (model_type == "random_forest") {
    fit <- random_forest(train, test, wide_test, NULL, is_ordered = ordered)
  }
  if (model_type == "glmnet") {
    fit <- train_glmnet(train, test, wide_test, fit_model, is_ordered = ordered)
  }
  if (model_type == "gradient_boosting") {
    fit <- gradient_boosting(train, test, wide_test, fit_model, is_ordered = ordered)
  }
  if (model_type == "decision_tree") {
    fit <- decision_tree(train, test, wide_test)
  }
  prev_model <- fit[1][[1]]
  prev_rps <- fit[2][[1]]
  
  week_number <- unique(matches_df$week)
  if (length(week_number) > 1) {week_number = paste(length(week_number), "weeks")}
  season_number <- unique(matches_df$season)
  test_size <- nrow(matches_df)
  ourRPS <- fit[1][[1]]
  minRPS <- fit[2][[1]]
  maxRPS <- fit[3][[1]]
  current_time <- format(Sys.time(), "%Y-%m-%d %X")
  if(any(grepl('avg', colnames(details_df)))){
    if ("z" %in% colnames(details_df)){
      feature <- "A+B+C"
    }
    if (!"z" %in% colnames(details_df)){
      feature <- "A+C"
    }
  }
  if (!(any(grepl('avg', colnames(details_df)))) & ("z" %in% colnames(details_df))) {feature <- "A+B"}
  if (!(any(grepl('avg', colnames(details_df)))) & !("z" %in% colnames(details_df))) {feature <- "A"}  
  
  df_summary <- read.csv("summary.csv")
  
  df_new <- data.frame(ModelType = model_type,
                           Feature = feature,
                           Ordered = ordered,
                           Weeks = week_number,
                           Seasons = season_number,
                           TestSize = test_size,
                           OurRPS = ourRPS,
                           MinRPS = minRPS,
                           MaxRPS = maxRPS,
                           timestamp = current_time)
  
  df_summary <- rbind(df_summary, df_new)
  write.csv(df_summary, file = "summary.csv", row.names = FALSE, quote = FALSE)
  
}


#train and test are necessary, requires defined lastrps (maybe use only last?)
#returns output_prob, testRPS
random_forest <- function(train, test, wide_test, fit_model = NULL,
                          control_method = "repeatedcv", control_number = 10, repeat_number = 10, 
                          is_ordered = FALSE){
  
  if (!is.null(fit_model)){
    output_prob <- predict(fit_model, test, "prob")
    colnames(output_prob) <- c("odd1", "oddX", "odd2")
    output_prob$winner <- wide_test$winner
    output_prob$matchId <- wide_test$matchId
    setcolorder(output_prob, c("matchId", "odd1", "oddX", "odd2", "winner"))
    output_prob <- as.data.table(output_prob)[, RPS := calculate_rps(odd1, oddX, odd2, winner), by = 1:nrow(output_prob)]
  }
  
  if (is.null(fit_model)){
    control <- trainControl(method = control_method, number = control_number, repeats = repeat_number, search = "grid")
    #metric can be Accuracy, ROC, RMSE, logLoss
    set.seed(1234)
    mtry <- sqrt(ncol(train))
    tunegrid <- expand.grid(.mtry=mtry)
    
    rf_default <- train(factor(convert(winner))~., 
                        data=train, 
                        method="rf", 
                        metric="Accuracy", 
                        tuneGrid=tunegrid, 
                        trControl=control, 
                        importance = T)

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
    minRPS <- round(min(testRPS$var),7)
    maxRPS <- round(max(testRPS$var),7)
    
    ourRPS <- mean(output_prob$RPS)
    x <- data.frame("***IE 492***", ourRPS)
    names(x) <- names(testRPS)
    testRPS <- rbind(testRPS, x)
    testRPS <- testRPS[order(testRPS$var),]
    print(testRPS)
    return(list(ourRPS, minRPS, maxRPS))
  }
}


#trace prints out that sentence
#max includes column for maximum oddtype
train_glmnet <- function(train, test, wide_test,
                         alpha=1,nlambda=50, tune_lambda=T,nofReplications=2,
                         nFolds=10,trace=F, max = F){
  set.seed(1234)
  train$winner <- convert(train$winner)
  train_class <- as.numeric(train$winner)
  train <- train[,-c("winner")]
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
  output_prob <- data.table(predicted_probabilities[,,])
  
  colnames(output_prob) <- c("odd1", "oddX", "odd2")
  output_prob$winner <- wide_test$winner
  output_prob$matchId <- wide_test$matchId
  setcolorder(output_prob, c("matchId", "odd1", "oddX", "odd2", "winner"))
  output_prob <- as.data.table(output_prob)[, RPS := calculate_rps(odd1, oddX, odd2, winner), by = 1:nrow(output_prob)]
  print(output_prob)
  
  testRPS <- lastrps[matchId %in% wide_test$matchId][, .(var = mean(Shin_RPS, na.rm = TRUE)), by = c("bookmaker")]
  testRPS <- testRPS[order(testRPS$var),]
  minRPS <- round(min(testRPS$var),7)
  maxRPS <- round(max(testRPS$var),7)
  
  ourRPS <- mean(output_prob$RPS)
  x <- data.frame("***IE 492***", ourRPS)
  names(x) <- names(testRPS)
  testRPS <- rbind(testRPS, x)
  testRPS <- testRPS[order(testRPS$var),]
  print(testRPS)
  return(list(ourRPS, minRPS, maxRPS))
}


gradient_boosting <- function(train, test, wide_test, fit_model){
  
  fitControl <- trainControl(method = "cv", number = 10)
  tune_Grid <-  expand.grid(interaction.depth = c(1,3,5),
                            n.trees = (1:5)*200,
                            shrinkage = c(0.1),
                            n.minobsinnode = c(5,10))
  nrow(tune_Grid)
  plot(tune_Grid)
  plot(gbmFit, plotType = "level")
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
  ourRPS <- mean(output_prob$RPS)
  
  df_summary <- read.csv("tuning.csv")
  
  df_new <- data.frame(cbind(fit$bestTune, fitControl$method, ourRPS))
  
  df_summary <- rbind(df_summary, df_new)
  write.csv(df_summary, file = "tuning.csv", row.names = FALSE, quote = FALSE)
  
  
  testRPS <- lastrps[matchId %in% wide_test$matchId][, .(var = mean(Shin_RPS, na.rm = TRUE)), by = c("bookmaker")]
  testRPS <- testRPS[order(testRPS$var),]
  minRPS <- round(min(testRPS$var),7)
  maxRPS <- round(max(testRPS$var),7)
  
  ourRPS <- mean(output_prob$RPS)
  x <- data.frame("***IE 492***", ourRPS)
  names(x) <- names(testRPS)
  testRPS <- rbind(testRPS, x)
  testRPS <- testRPS[order(testRPS$var),]
  print(testRPS)
  return(list(ourRPS, minRPS, maxRPS))
}


decision_tree <- function(train, test, wide_test, fit_model = NULL){
  set.seed(1234)
  
  # if (!is.null(fit_model)){
  #   output_prob <- predict(fit_model, test, "prob")
  #   colnames(output_prob) <- c("odd1", "oddX", "odd2")
  #   output_prob$matchId <- wide_test$matchId
  #   setcolorder(output_prob, c("matchId", "odd1", "oddX", "odd2"))
  #   output_prob <- comparison(output_prob, rank = T)
  #   ourRPS <- mean(output_prob$RPS)
  #   return(list(fit, ourRPS))
  # }
  
  # if (is.null(fit_model)){
  if (ordered){
    fit <-  train(y = train$winner, 
                  x = train[,-c("winner")], 
                  method = "rpart", 
                  tuneGrid = expand.grid(.cp = c((1:15)*0.005)),
                  trControl = trainControl(method = "repeatedcv", 
                                           number = 10, 
                                           repeats = 10,
                                           classProbs = T,
                                           summaryFunction = rpsCaret))
  }  
  if (!ordered){
    fit <-  train(y = train$winner, 
                  x = train[,-c("winner")], 
                  method = "rpart", 
                  tuneGrid = expand.grid(.cp = c((1:15)*0.005)),
                  trControl = trainControl(method = "repeatedcv", number = 10, repeats = 10))
  }
   
    output_prob <- data.frame(predict(fit,test,type= "prob"))
    colnames(output_prob) <- c("odd1", "oddX", "odd2")
    output_prob$matchId <- wide_test$matchId
    setcolorder(output_prob, c("matchId", "odd1", "oddX", "odd2"))
    output_prob <- comparison(output_prob, T)
    ourRPS <- mean(output_prob$RPS)
    
    
    return(list(fit, ourRPS))
  # }
}


fit <-  train(y = (train$winner), 
              x = train[,-c("winner")], 
              method = "rpart", 
              tuneGrid = expand.grid(.cp = c((1:15)*0.01)),
              trControl = trainControl(method = "repeatedcv", 
                                       number = 10, 
                                       repeats = 3,
                                       classProbs = T,
                                       summaryFunction = rpsCaret))



tc <- trainControl(method="cv",classProb=TRUE,summaryFunction=roc)

if (d == "odd1") {d=1}
if (d == "oddX") {d=2}
if (d == "odd2") {d=3}
pred = t(matrix(c(a, b, c)))
output <- rps(obs = c(d), pred = pred)
output$rps

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
calculate_rps(0.276, 0.713, 0.001, 3)
0.009513953 + 0.237572678+ 0.478036409+ 0.219385103+0.223962831+0.273281869+ 0.310554713+ 0.430385600+ 0.332925821+ 0.239500386
