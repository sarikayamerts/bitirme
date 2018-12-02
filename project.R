 ### clears the environment
rm(list = ls())

### mert's macbook github directory
setwd("/Users/mertsarikaya/bitirme/")
### mert's windows github directory
# setwd("")
### emre's github directory
#setwd("C:/Users/Hp/Desktop/Bitirme/bitirme")

library(readr)
library(graphics)
library(data.table)
library(verification)
library(glmnet)
library(TunePareto)
library(anytime) 
library(plotly)

testStart=as.Date('2017-07-15')
trainStart=as.Date('2010-08-13')

### implementation of shin probability calculation 
# functions in this file:
# 1 - shin_prob_calculator(list)
source("shin.R")

### implementation of converting match results from string to {over, under, 1, X, 2} types of outcome
# functions in this file:
# 1 - winner(score) 
# 2 - over_under(score)
# 3 - inverse(odd)
source("match_scores.R")

### implementation of ranked probability score
# functions in this file:
# 1 - calculate_rps(home, draw, away, actual) 
# 2 - calculate_rps2(over, under, actual)
source("rps.R")

### converting dates to seasons
# functions in this file:
# 1 - season_calc(date) 
source("season_calculator.R")

### read and prepare dataframes (not ready)
# 1 - details (matchId, bookmaker, oddtype, odd)
# 2 - matches (matchId, score, home, away, date, over_under, winner, season)
# 3 - first (matchId, bookmaker, oddtype, odd)
# 4 - last (matchId, bookmaker, oddtype, odd)
# 5 - next_matches (matchId, score, home, away, date)
source("get_dataframes.R")

### handle missing odds
# 
source("missingvalues.R")


### converting odds to basic and shin probabilities
# changes first and last dataframes
source("convert_odds.R")

### reshaping first and last dataframes to feature extraction
# 1 - wide_first (matchId, shin*basic*bookmaker*oddtype, winner)
# 2 - wide_last (matchId, shin*basic*bookmaker*oddtype, winner)
source("reshape.R")

### calculate RPS for all matches using Basic and Shin probs
# changes in first and last dataframes
source("calculate_rps.R")

### calculate average RPS for all bookmakers using Basic and Shin probs
source("bookmaker_comparison.R")

### Creating training and test data
train_features <- wide_last[date>=trainStart & date<testStart] 
test_features <- wide_last[date>=testStart] 
not_included_feature_indices = c(1,12,13,14)

### construction of model (not ready)
# functions in this file:
# 1 - train_glmnet
source("train_models.R")

### Run glmnet on train data with tuning lambda parameter based on RPS and return predictions based on lambda with minimum RPS
predictions=train_glmnet(train_features, test_features,not_included_feature_indices, alpha=1,nlambda=50, tune_lambda=TRUE,nofReplications=2,nFolds=10,trace=T)
levels(df$col)[levels(df$col) == "No contact"] <- "0"

predict = predictions[["predictions"]]
predict = predict[, RPS := calculate_rps(odd1,oddX,odd2,winner), by = 1:nrow(predict)]
averageRPS = mean(predict$RPS)

### importing data and manipulating it to calculate normalized (basic and shin) probabilities for each match & bookmaker
# datatables in this script:
# 1 - details (matchid, bookmaker, bettype, oddtype, odd)
# 2 - matches (matchid, score, over_under, winner)
# 3 - first (matchid, bookmaker, norm_prob.odd1-oddX-odd2, shin_prob.odd1-oddX-odd2, winner)
# 4 - last (matchid, bookmaker, norm_prob.odd1-oddX-odd2, shin_prob.odd1-oddX-odd2, winner)
# source("data_manipulation.R")

df <- first[bookmaker == "1xBet" | bookmaker == "Betfair" | bookmaker == "ComeOn" | bookmaker == "888Sport" | bookmaker == "Pinnacle" | bookmaker == "Betsafe"]
df <- df[complete.cases(df), ]
multinomial_model(df)

model_RPS <- predictions[, calculate_rps(odd1, oddX, odd2, winner), by = 1:nrow(predictions)]
predictions$RPS <- model_RPS$V1
rm(model_RPS)
avg <- mean(predictions$RPS)






