 ### clears the environment
rm(list = ls())

if (grepl("mert", toString(getwd()))){
  setwd("/Users/mertsarikaya/bitirme/")
}
if (grepl("Hp", toString(getwd()))) {
  setwd("C:/Users/Hp/Desktop/Bitirme/bitirme")
}

library(readr)
library(graphics)
library(data.table)
library(verification)
library(glmnet)
library(TunePareto)
library(anytime) 
library(plotly)

##### FUNCTIONS TO BE USED

### implementation of ranked probability score
# functions in this file:
# 1 - calculate_rps(home, draw, away, actual) 
# 2 - calculate_rps2(over, under, actual)
source("rps.R")

### converting odd1, oddX, odd2 to 1,2,3 and viceversa
# 1 - convert(arr)
source("converter.R")

### read and prepare dataframes (not ready)
# 1 - details (matchId, bookmaker, oddtype, odd)
# 2 - matches (matchId, score, home, away, date, over_under, winner, season)
# 3 - first (matchId, bookmaker, oddtype, odd)
# 4 - last (matchId, bookmaker, oddtype, odd)
# 5 - next_matches (matchId, score, home, away, date)
source("get_dataframes.R")

### converting odds to basic and shin probabilities
# changes first and last dataframes
source("convert_odds.R")

### changing odds
#
source("changing_odds.R")

### reshaping first and last dataframes to feature extraction
# 1 - wide_first (matchId, shin*basic*bookmaker*oddtype, winner)
# 2 - wide_last (matchId, shin*basic*bookmaker*oddtype, winner)
source("reshape.R")

#wide_first <- widening(first, c("888sport", "SBOBET", "bwin", "Pinnacle", "Betclic"))
wide_last <- widening(last, bookiesToKeep)

### calculate RPS for all matches using Basic and Shin probs
# changes in first and last dataframes
source("calculate_rps.R")

### calculate average RPS for all bookmakers using Basic and Shin probs
source("bookmaker_comparison.R")



### Deleting noncomplete season 2018-2019
# WHY??
wide_last <- wide_last[season != "2018-2019"]


### Creating training and test data
testStart=as.Date('2017-07-15')
trainStart=as.Date('2010-08-13')
train_features <- wide_last[date>=trainStart & date<testStart] 
test_features <- wide_last[date>=testStart] 
n <- ncol(train_features)
not_included_feature_indices = c(1,n-3,n-2,n-1,n)
TrainSet <- nrow(train_features)
TestSet <- nrow(test_features)

# or seasonal
train_features <- wide_last[season != "2018-2019"]
test_features <- wide_last[season == "2018-2019"]
n <- ncol(train_features)
not_included_feature_indices = c(1,n-3,n-2,n-1,n)

# or between dates
start = '2018-11-28'
end = '2018-12-01'
next_match_ids <- next_matches[date >= start][date <= end]$matchId
test_data <- last[matchId %in% next_match_ids]
wide_test <- widening_test(test_data, bookiesToKeep)
test_features <- wide_test
train_features <- wide_last
n <- ncol(train_features)
not_included_feature_indices = c(1,n-3,n-2,n-1,n)

# or weekly
test_match_ids <- matches[week == 48][season == '2018-2019']$matchId
test_data <- last[matchId %in% test_match_ids]
wide_test <- widening(test_data, bookiesToKeep)
test_features <- wide_test
train_features <- wide_last[date < '2018-12-03']
n <- ncol(train_features)
not_included_feature_indices = c(1,n-3,n-2,n-1,n)
TrainSet <- nrow(train_features)
TestSet <- nrow(test_features)
testStart=as.Date('2018-12-03')
trainStart=as.Date('2010-08-13')


### construction of model
# functions in this file:
# 1 - train_glmnet
source("train_models.R")

### Run glmnet on train data with tuning lambda parameter based on RPS and return predictions based on lambda with minimum RPS
predictions <- train_glmnet(train_features, test_features,not_included_feature_indices, 
                            alpha=1,nlambda=50, tune_lambda=TRUE,
                            nofReplications=2,nFolds=10,trace=T,max=FALSE)

testRPS <- lastrps[matchId %in% predictions[["predictions"]]$matchId][, .(var = mean(Shin_RPS, na.rm = TRUE)), by = c("bookmaker")]
testRPS <- testRPS[order(testRPS$var),]

### report of model
# functions in this file:
# 1 - model_report
source("model_report.R")


### NOTE: Change the comment below about the input type
myRPS <- model_report(modeltype = "GLMNET", n_of_inputs = n, Comment = "Basic + Shin", TrainSet, TestSet, trainStart, testStart, predictions, testRPS)

myRPS

