# Continuation Ratio Model for Ordinal Data - vglmContRatio
# Cumulative Probability Model for Ordinal Data - vglmCumulative
# Penalized Ordinal Regression - ordinalNet
# glmnetcr - ordered glmnet


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
# matches_df = matches[week == 43][season == '2018-2019']
# details_df = shin
models <- function(matches_df, details_df,
                   model_type = c("randomforest", "glmnet","gradient_boosting","decision_tree"),
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
  prev_date <- 365
  lower_date <- as.Date(min_date) - prev_date
  train_match_ids <- matches[date < min_date][date > lower_date]$matchId
  train_data <- details_df[matchId %in% train_match_ids]
  wide_train <- widening_withwinner(train_data, bookiesToKeep)
  wide_train <- wide_train[complete.cases(wide_train)]

  train <- wide_train[,-c("matchId", "date", "week", "season")]
  test <- wide_test[,-c("matchId", "winner", "date", "week", "season")]

  if (is_ordered){train$winner <- ordered(train$winner, levels = c("odd1", "oddX", "odd2"))}
  
  if (model_type == "random_forest") {
    if (ordered){
      ### ordinal random forest needs to be implemented
    }
    if(!ordered){
      fit <- random_forest(train, test, wide_test)
    }
  }
  if (model_type == "glmnet") {
    if (ordered) {
      ### ordinal glmnet, glmnetcr
    }
    if (!ordered){
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
                           TrainStart = lower_date,
                           TestStart = min_date,
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
  tunegrid <- expand.grid(.mtry= 1 + (0:14) * 0.5)
    
  fit <- train(winner~., 
                data=train, 
                method="rf", 
                tuneGrid=tunegrid, 
                trControl=control,
                ntree = 2000,
                importance = T)

  output_prob <- predict(fit, test, "prob")
  colnames(output_prob) <- c("odd1", "oddX", "odd2")
  output_prob$matchId <- wide_test$matchId
  setcolorder(output_prob, c("matchId", "odd1", "oddX", "odd2"))
  output_prob <- comparison(output_prob, rank = FALSE)
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
  
  final_glmnet_fit <- glmnet(as.matrix(train),as.factor(train_class),family="multinomial", alpha = alpha,lambda=cvResultsSummary$lambda.min)
  predicted_probabilities <- predict(final_glmnet_fit, as.matrix(test), type = "response")
  output_prob <- data.table(predicted_probabilities[,,])
  colnames(output_prob) <- c("odd1", "oddX", "odd2")
  output_prob$matchId <- wide_test$matchId
  setcolorder(output_prob, c("matchId", "odd1", "oddX", "odd2"))
  output_prob <- comparison(output_prob, rank = FALSE)
  return(list(fit, output_prob))
}


gradient_boosting <- function(train, test, wide_test){
  fitControl <- trainControl(method = "repeatedcv", 
                             number = 10, 
                             repeats = 3,
                             classProbs = T,
                             summaryFunction = rpsCaret)
  tune_Grid <-  expand.grid(interaction.depth = c(3,4,5),
                            n.trees = (1:5)*200,
                            shrinkage = c(0.1),
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
  colnames(output_prob) <- c("odd1", "oddX", "odd2")
  output_prob$matchId <- wide_test$matchId
  setcolorder(output_prob, c("matchId", "odd1", "oddX", "odd2"))
  output_prob <- comparison(output_prob, rank = FALSE)
  return(list(fit, output_prob))
}


decision_tree <- function(train, test, wide_test){
  set.seed(1234)
  fit <-  train(y = train$winner, 
                x = train[,-c("winner")], 
                method = "rpart", 
                tuneGrid = expand.grid(.cp = c((1:14)*0.005)),
                trControl = trainControl(method = "repeatedcv", 
                                         number = 10, 
                                         repeats = 10,
                                         classProbs = T,
                                         summaryFunction = rpsCaret))
   
  output_prob <- predict(fit, test, "prob")
  #colnames(output_prob) <- c("odd1", "oddX", "odd2")
  output_prob$matchId <- wide_test$matchId
  setcolorder(output_prob, c("matchId", "odd1", "oddX", "odd2"))
  output_prob <- comparison(output_prob, trace = FALSE)
  return(list(fit, output_prob))
}





# bet_on <- function(){
#   best_bookmaker <- testRPS$bookmaker[1]
#   portfolio_df <- last[bookmaker == best_bookmaker][matchId %in% predictions[["predictions"]]$matchId][,-4]
#   bookmaker_pred <- widening(portfolio_df, best_bookmaker)[,c(1,5,2,4,3)]
#   our_pred <- predictions[["predictions"]]
#   colnames(bookmaker_pred) <- colnames(our_pred)
#   differences <- merge(our_pred, bookmaker_pred, on = c("matchId"))
#   differences$odd1_diff <- differences[, odd1.x-odd1.y, by = 1:nrow(differences)]$V1
#   differences$oddX_diff <- differences[, oddX.x-oddX.y, by = 1:nrow(differences)]$V1
#   differences$odd2_diff <- differences[, odd2.x-odd2.y, by = 1:nrow(differences)]$V1
#   we_bet_on <- convert(max.col(differences[,c(10:12)], 'first'))
#   actual_outcome <- differences$winner.x
#   results <- as.data.table(cbind(differences$matchId, we_bet_on, actual_outcome, matrix(1, 10, 3)))
#   colnames(results) <- c("matchId", "we_bet_on", "actual_outcome", "odd", "bet_amount", "on_hand")
#   
#   for (i in 1:nrow(results)){
#     results$odd[i] <- last(details[(matchId == results$matchId[i]) & (bookmaker == "12BET") & (oddtype == results$we_bet_on[i])])$odd
#     if(results$we_bet_on[i] == results$actual_outcome[i]){
#       results$on_hand[i] <- as.integer(results$bet_amount[i]) * as.double(results$odd[i])
#     }
#     else{
#       results$on_hand[i] <- 0
#     }
#   }
#   print(results)
# }
# 
