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

# unique(matches[season == '2018-2019']$week)
# matches_df = next_matches[date == '2018-12-16']
# matches_df = matches[season == '2018-2019'][week == 48]
# details_df = shin
models <- function(matches_df, details_df,
                   model_type = c("random_forest", "glmnet","gradient_boosting","decision_tree"),
                   is_ordered = FALSE) {
  
  test_match_ids <- matches_df$matchId
  test_data <- details_df[matchId %in% test_match_ids]
  if (all(is.na(test_data$winner))){
    wide_test <- subsetBookies(bookiesToKeep, last[matchId %in% test_match_ids])
    wide_test <- reshape(wide_test, idvar = c("matchId", "bookmaker"), timevar = c("oddtype"), direction = "wide")
    wide_test <- reshape(wide_test, idvar = c("matchId"), timevar = c("bookmaker"), direction = "wide")
    wide_test$winner <- NA
  }
  if (!all(is.na(test_data$winner))){
    wide_test <- widening_withwinner(test_data, bookiesToKeep)
    wide_test <- wide_test[complete.cases(wide_test)]
  }
  min_date <- min(matches_df[matchId %in% test_match_ids]$date)
  if(nrow(wide_test) > 25){prev_date <- 1000}
  if(nrow(wide_test) <= 25){prev_date <- 365}

  lower_date <- as.Date(min_date) - prev_date
  train_match_ids <- matches[date < min_date][date > lower_date]$matchId
  train_data <- details_df[matchId %in% train_match_ids]
  wide_train <- widening_withwinner(train_data, bookiesToKeep)
  wide_train <- wide_train[complete.cases(wide_train)]

  train <- wide_train[,-c("matchId", "date", "week", "season")]
  test <- wide_test[,-c("matchId", "winner", "date", "week", "season")]

  if (is_ordered){train$winner <- ordered(train$winner, levels = c("odd1", "oddX", "odd2"))}
  print(paste(model_type, ", is_ordered", is_ordered))
  
  
  if (model_type == "random_forest") {
    if (is_ordered){
      fit <- ord_rf(train, test, wide_test)    
    }
    if(!is_ordered){
      fit <- random_forest(train, test, wide_test)
    }
  }
  if (model_type == "vglm") {
    if (is_ordered){
      fit <- vglmCumulative(train, test, wide_test)    
    }
    if(!is_ordered){
      print("Vglm works with ordinal variables")
    }
  }
  if (model_type == "glmnet") {
    if (is_ordered) {
      print("Ordered GLMNET is not implemented yet.")
    }
    if (!is_ordered){
      fit <- train_glmnet(train, test, wide_test)
    }
  }
  if (model_type == "gradient_boosting") {
    fit <- gradient_boosting(train, test, wide_test)
  }
  if (model_type == "decision_tree") {
    fit <- decision_tree(train, test, wide_test)
  }
  ourRPS <- mean(fit[[2]]$RPS)
  preds <- fit[[2]]
  fit <- fit[[1]]
  print(preds[oddX > 0.3])
  
  week_number <- unique(matches_df$week)
  if (length(week_number) > 1) {week_number <- paste(length(week_number), "weeks")}
  season_number <- unique(matches_df$season)
  test_size <- nrow(matches_df)
  current_time <- format(Sys.time(), "%Y-%m-%d %X")
  if(any(grepl('avg', colnames(details_df)))){
    if ("z" %in% colnames(details_df)){feature <- "A+B+C"}
    if (!"z" %in% colnames(details_df)){feature <- "A+C"}
  }
  if (!(any(grepl('avg', colnames(details_df)))) & ("z" %in% colnames(details_df))) {feature <- "A+B"}
  if (!(any(grepl('avg', colnames(details_df)))) & !("z" %in% colnames(details_df))) {feature <- "A"}  
  str = ""
  for (i in 1:ncol(fit$bestTune)) {
    str <- paste(str, names(fit$bestTune[i]), fit$bestTune[i][1,])
  }
  
  df_summary <- read.csv("summary2.csv")
  
  df_new <- data.frame(ModelType = model_type,
                           Feature = feature,
                           Ordered = is_ordered,
                           Weeks = week_number,
                           Seasons = season_number,
                           TestSize = test_size,
                           TrainStart = toString(lower_date),
                           TestStart = toString(min_date),
                           OurRPS = ourRPS,
                           BestTune = str,
                           timestamp = current_time)
  
  df_summary <- rbind(df_summary, df_new)
  write.csv(df_summary, file = "summary2.csv", row.names = FALSE, quote = FALSE)
  return(list(fit, ourRPS, preds))
}


#train and test are necessary, requires defined lastrps (maybe use only last?)
#returns output_prob, testRPS
random_forest <- function(train, test, wide_test){
  set.seed(1234)
  control <- trainControl(method = "repeatedcv", 
                            number = 10, 
                            repeats = 3, 
                            search = "grid",
                            classProbs = TRUE,
                            summaryFunction = rpsCaret)
  tunegrid <- expand.grid(.mtry= 4+(0:8)*0.5)
    
  fit <- train(winner~., 
                data=train, 
                method="rf", 
                tuneGrid=tunegrid, 
                trControl=control,
                importance = T)

  output_prob <- predict(fit, test, "prob")
  colnames(output_prob) <- c("odd1", "oddX", "odd2")
  output_prob$matchId <- wide_test$matchId
  setcolorder(output_prob, c("matchId", "odd1", "oddX", "odd2"))
  output_prob <- comparison(output_prob, trace = T)
  return(list(fit, output_prob))
}

ord_rf <- function(train, test, wide_test){
  set.seed(1234)
  # There are several hyperparameters, which do, however,
  # not have to be optimized by the user in general, because the default
  # values used for these hyperparameters were seen to be in a reasonable 
  # range and the results seem to be quite robust with respect to the 
  # choices of the hyperparameter values.
  fit <- ordfor(depvar="winner", 
                      data=train, nsets=1000, ntreeperdiv=400,
                      ntreefinal=5000, perffunction = "equal")
  preds <- predict(fit, newdata=test, "prob")
  output_prob <- data.table(preds$classfreqtree)
  output_prob$matchId <- wide_test$matchId
  setcolorder(output_prob, c("matchId", "odd1", "oddX", "odd2"))
  output_prob <- comparison(output_prob, trace = T)
  return(list(fit, output_prob))
}

train_glmnet <- function(train, test, wide_test,
                         alpha=1,nlambda=50, tune_lambda=T,nofReplications=2,
                         nFolds=10,trace=F, max = F){
  set.seed(1234)
  train$winner <- convert(train$winner)
  train_class <- as.numeric(train$winner)
  train <- train[,-c("winner")]
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
  
  fit <- glmnet(as.matrix(train),as.factor(train_class),family="multinomial", alpha = alpha,lambda=cvResultsSummary$lambda.min)
  predicted_probabilities <- predict(fit, as.matrix(test), type = "response")
  output_prob <- data.table(predicted_probabilities[,,])
  colnames(output_prob) <- c("odd1", "oddX", "odd2")
  output_prob$matchId <- wide_test$matchId
  setcolorder(output_prob, c("matchId", "odd1", "oddX", "odd2"))
  output_prob <- comparison(output_prob, trace = T)
  fit$bestTune <- data.frame(lambda = fit$lambda)
  return(list(fit, output_prob))
}

train_glmnetcr <- function(train, test, wide_test,
                         alpha=1,nlambda=50,nofReplications=2,
                         nFolds=10){
  set.seed(1234)
  train_class <- train$winner
  train <- train[,-c("winner")]

  # to set lambda parameter, cross-validation will be performed and lambda is selected based on RPS performance
  cvindices <- generateCVRuns(train_class,nofReplications,nFolds,stratified=TRUE)
  
  # first get lambda sequence for all data
  glmnet_alldata <- glmnetcr(as.matrix(train), as.factor(train_class), alpha = alpha, nlambda=nlambda)
  lambda_sequence <- glmnet_alldata$lambda
  
  cvresult=vector('list',nofReplications*nFolds)
  iter=1
  for(i in 1:nofReplications) {
    thisReplication=cvindices[[i]]
    for(j in 1:nFolds){
      testindices <- order(thisReplication[[j]])
      
      cvtrain <- train[-testindices]  
      cvtrainclass <- train_class[-testindices]   
      cvtest <- train[testindices]
      cvtestclass <- train_class[testindices] 
      
      inner_cv_glmnet_fit <- glmnetcr(data.matrix(cvtrain),as.factor(cvtrainclass), alpha = alpha,lambda=lambda_sequence)
      valid_pred <- predict(inner_cv_glmnet_fit, data.matrix(cvtest), s = lambda_sequence, type = "prob")
      
      #check order of predictions
      #order_of_class <- attr(valid_pred,'dimnames')[[2]]
      #new_order <- c(which(order_of_class=='1'),which(order_of_class=='2'),which(order_of_class=='3'))
      foldresult <- rbindlist(lapply(c(1:length(lambda_sequence)),function(x) { 
        data.table(repl=i,fold=j,lambda=lambda_sequence[x],valid_pred$probs[,,x],result=cvtestclass)
        }))
      cvresult[[iter]]=foldresult
      iter=iter+1
    }
  }
  
  cvresult <- rbindlist(cvresult)
  #cvresult$result <- convert(cvresult$result)
  #names(cvresult) <- c("repl", "fold", "lambda", "odd1", "oddX", "odd2", "result")
  
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

  
  fit <- glmnetcr(as.matrix(train),as.factor(train_class), alpha = alpha,lambda=cvResultsSummary$lambda.min)
  predicted_probabilities <- predict(fit, data.matrix(test), type = "response")
  output_prob <- data.table(predicted_probabilities[,,])
  colnames(output_prob) <- c("odd1", "oddX", "odd2")
  output_prob$matchId <- wide_test$matchId
  setcolorder(output_prob, c("matchId", "odd1", "oddX", "odd2"))
  output_prob <- comparison(output_prob, trace = T)
  fit$bestTune <- data.frame(lambda = fit$lambda)
  return(list(fit, output_prob))
}



gradient_boosting <- function(train, test, wide_test){
  fitControl <- trainControl(method = "repeatedcv", 
                             number = 10, 
                             repeats = 3,
                             classProbs = T,
                             summaryFunction = rpsCaret)
  tune_Grid <-  expand.grid(interaction.depth = c(1,3,5),
                            n.trees = (1:10)*50,
                            shrinkage = c(0.01, 0.05, 0.1),
                            n.minobsinnode = c(5,10))
  #plot(tune_Grid) #Error in plot.new() : figure margins too large 
  #plot(gbmFit, plotType = "level")
  set.seed(1234)
  fit <- train(winner ~ ., 
               data = train,
               method = "gbm",
               trControl = fitControl,
               verbose = FALSE,
               tuneGrid = tune_Grid)

  output_prob <- predict(fit, test, "prob")
  #colnames(output_prob) <- c("odd1", "oddX", "odd2")
  output_prob$matchId <- wide_test$matchId
  setcolorder(output_prob, c("matchId", "odd1", "oddX", "odd2"))
  output_prob <- comparison(output_prob, trace = T)
  return(list(fit, output_prob))
}


decision_tree <- function(train, test, wide_test){
  set.seed(1234)
  fit <-  train(y = train$winner, 
                x = train[,-c("winner")], 
                method = "rpart", 
                tuneGrid = expand.grid(.cp = c((1:7)*0.03)),
                trControl = trainControl(method = "repeatedcv", 
                                         number = 10, 
                                         repeats = 10,
                                         classProbs = T,
                                         summaryFunction = rpsCaret))
   
  output_prob <- predict(fit, test, "prob")
  #colnames(output_prob) <- c("odd1", "oddX", "odd2")
  output_prob$matchId <- wide_test$matchId
  setcolorder(output_prob, c("matchId", "odd1", "oddX", "odd2"))
  output_prob <- comparison(output_prob, trace = T)
  return(list(fit, output_prob))
}

vglmCumulative <- function(train, test, wide_test){
  set.seed(1234)
  control <- trainControl(method = "cv", 
                          number = 10, 
                          search = "random",
                          classProbs = TRUE,
                          summaryFunction = rpsCaret)
  tunegrid <- expand.grid(parallel = TRUE, link = c("logit", "probit"))

  
  fit <- train(winner~., 
               data=train, 
               method="vglmCumulative", 
               trControl=control,
               preProc = c("center", "scale"),
               importance = T)
  
  output_prob <- predict(fit, test, "prob")
  colnames(output_prob) <- c("odd1", "oddX", "odd2")
  output_prob$matchId <- wide_test$matchId
  setcolorder(output_prob, c("matchId", "odd1", "oddX", "odd2"))
  output_prob <- comparison(output_prob, trace = T)
  return(list(fit, output_prob))
}

best_model <- function(train, test, wide_test){
  fitControl <- trainControl(method = "repeatedcv", 
                             number = 10, 
                             repeats = 3,
                             classProbs = T,
                             summaryFunction = rpsCaret)
  tune_Grid <-  expand.grid(interaction.depth = 1,
                            n.trees = c(200),
                            shrinkage = 0.01,
                            n.minobsinnode = 5)
  #plot(tune_Grid) #Error in plot.new() : figure margins too large 
  #plot(gbmFit, plotType = "level")
  set.seed(1234)
  fit <- train(winner ~ ., 
               data = train,
               method = "gbm",
               trControl = fitControl,
               verbose = FALSE,
               tuneGrid = tune_Grid)
  
  output_prob <- predict(fit, test, "prob")
  #colnames(output_prob) <- c("odd1", "oddX", "odd2")
  output_prob$matchId <- wide_test$matchId
  setcolorder(output_prob, c("matchId", "odd1", "oddX", "odd2"))
  output_prob <- comparison(output_prob, trace = T)
  return(list(fit, output_prob))
}


odd_strategy <- function(){
  best_bookmaker <- "Pinnacle"
  portfolio_df <- last[bookmaker == best_bookmaker][matchId %in% play$matchId][,-4]
  odds <- details[bookmaker == "Pinnacle"][matchId %in% play$matchId]
  key(odds) <- c("matchId", "oddtype")
  last_odds <- odds[unique(odds[,key(odds), with = FALSE]), mult = 'last']
  last_oddsX <- last_odds[oddtype == "oddX"]
  final <- merge(play, last_oddsX, by = "matchId")
  final$initial <- 10
  final$onhand <- 0
  final$cumulative <- 0
  final$net <- 0
  
  if (final$winner[1] == "oddX"){
    final$onhand[1] <- final$odd[1] * 10

  }
  for (i in (2:nrow(final))){
    if (final$winner[i] == "oddX"){
      final$onhand[i] <- final$odd[i] * 10
    }
    final$cumulative[i] <- final$onhand[i] + final$cumulative[i-1]
  }
  for (i in (1:nrow(final))){
    final$net[i] <- final$cumulative[i] - i * 10
  }
  final
}

