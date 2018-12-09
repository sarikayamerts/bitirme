### NEEDS TO BE DONE
# changes in odd
# friedman nemenyi tests
# last 5 matches

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
library(stats)
library(PMCMR)

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
wide_last <- widening(last[,-4], bookiesToKeep)

### calculate RPS for all matches using Basic and Shin probs
# changes in first and last dataframes
source("calculate_rps.R")

### calculate average RPS for all bookmakers using Basic and Shin probs
source("bookmaker_comparison.R")

### statistical tests
#   You need to send dataframes as c(matchId, bookmakers, RPS)
#
source("statistical_tests.R")

basic_vs_shin <- basic_vs_shin(lastrps)
bookie_friedman <- bookmaker_comp_friedman(lastrps, bookiesToKeep)
nemenyi_test_outputs <- bookmaker_comp_plot(lastrps,bookiesToKeep)

source("train_models.R")

models(matches[date > '2017-07-15'], "randomforest")
models(matches[season == '2018-2019'], "multinomial")
models(matches[date >= '2018-11-28'][date <= '2018-12-01'])
models(matches[week == 48][season == '2018-2019'], "randomforest")

## for all weeks in a season
for (i in noquote(unique(matches[season == "2018-2019"]$week))){
  paste("Season 2018-2019, week ", i)
  models(matches[week == i][season == '2018-2019'], "randomforest")
}


### report of model
# functions in this file:
# 1 - model_report
source("model_report.R")


### NOTE: Change the comment below about the input type
myRPS <- model_report("GLMNET", n, "Basic + Shin 48 Week", TrainSet, TestSet, trainStart, testStart, predictions, testRPS)

myRPS