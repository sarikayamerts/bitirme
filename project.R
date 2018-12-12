### NEEDS TO BE DONE
# DONE changes in odd
# DONE friedman nemenyi tests
# last 5 matches
# hybrid random forest
# ordinal outcomes (in caret package 7.0.30)
# DONE shift edip meanleri hesapla, sürpriz var mı gözle bak
# scmamp
# DONE last ve first için datayı bir daha order et, hatalı olabilir kendi sırası
# DONE odd'lardaki değişimleri saat farkına böldüm (saat sıralamasını unutma)
## böyle olunca son bir saat içinde olanlar gözüktü hep 2 gün önce çok büyül bir değişiklik olduysa oran çok küçük kaldı
# oddX'i çıkar, 1 ve 2 kalsın sadece

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

################################################
# implementation of ranked probability score
# functions in this file:
# 1 - calculate_rps(home, draw, away, actual) 
# 2 - calculate_rps2(over, under, actual)
################################################
source("rps.R")


################################################
# converting odd1, oddX, odd2 to 1,2,3 and viceversa
# 1 - convert(arr)
################################################
source("converter.R")


################################################
# read and prepare dataframes 
# 1 - details (matchId, bookmaker, oddtype, odd)
# 2 - matches (matchId, score, home, away, date, over_under, winner, season)
# 3 - first (matchId, bookmaker, oddtype, odd)
# 4 - last (matchId, bookmaker, oddtype, odd)
# 5 - next_matches (matchId, score, home, away, date)
# 6 - details_change
# functions
# 1 - inverse
# 2 - over_under
# 3 - season_calc
# 4 - set_directory
# 5 - winner
################################################
source("get_dataframes.R")
View(details)
View(head(matches))
View(head(details_change))


### converting odds to basic and shin probabilities, gives insiders
source("convert_odds.R")

### changing odds
#source("changing_odds.R")


################  NOT FOR MODELS ############## 
### calculate RPS for all matches using Basic and Shin probs
# lastrps <<<< required for models
source("calculate_rps.R")
### calculate average RPS for all bookmakers using Basic and Shin probs
source("bookmaker_comparison.R")
### statistical tests
source("statistical_tests.R")
basic_vs_shin <- basic_vs_shin(lastrps)
bookie_friedman <- bookmaker_comp_friedman(lastrps, bookiesToKeep)
nemenyi_test_outputs <- bookmaker_comp_plot(lastrps,bookiesToKeep)
###############################################




##################################
# Features:
# 1 - Shin Probabilities  df = last[matchId,bookmaker,oddtype,shin_prob])
# 2 - Changing in Odds    df = changes[....])
# 3 - Insider Traders     df = insider[matchId,bookmaker,z] 
##################################

### reshaping features to create train_features
source("reshape.R")


### Creating training and test data together

#shin_wide <- widening(last, bookiesToKeep)
#change_wide <- widening(change, bookiesToKeep) 
#insider_wide <- widening_others(insider, bookiesToKeep)

#bence böyle widelayalım
#we will do widening inside of our model prepration
#last:           matchId, bookmaker, oddtype, shin_prob
#lastrps:        matchId, bookmaker, shin_prob(1X2), winner, season, week, shinrps
#insider:        matchId, bookmaker, z
#details_change: matchId, bookmaker, oddtype, diff, winner, avg
#shin_insider:   matchId, bookmaker, shin_prob(1X2), winner, z
#shin_changes:   matchId, bookmaker, winner, shin_prob(1X2), avg(1X2)
#shin_changes_insider: matchId, bookmaker, winner, shin_prob(1X2), avg(1X2), z
shin <- lastrps[,-c("Shin_RPS")]
shin_insider <- merge(lastrps[,c(1:6)], insider, by = c("matchId", "bookmaker"))
shin_changes <- merge(last, details_change[,c("matchId", "bookmaker", "oddtype", "avg", "winner")],by = c("matchId", "bookmaker", "oddtype"))
shin_changes <- reshape(shin_changes, idvar = c("matchId", "bookmaker", "winner"), timevar = c("oddtype"), direction = "wide")
shin_changes_insider <- merge(shin_changes, insider, by = c("matchId", "bookmaker"))

#features <- merge(shin_wide, matches[, .(matchId, winner, date, week, season)], by = "matchId")
#features <- merge(features, change_wide, by = "matchId")
#features <- merge(features, insider_wide, by = "matchId")


source("variable_importance.R")

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



# A = shin_prob 
A <- models(matches_df =  matches[season == '2017-2018'], 
            details_df =  shin, 
            model_type =  "random_forest")
A_ord <- models(matches_df =  matches[season == '2017-2018'], 
            details_df =  lastrps[,-c("Shin_RPS")], 
            model_type =  "random_forest",
            ordered = TRUE)
# A + B = shin_prob + insider
AB <- models(matches_df = matches[week == 48][season == '2018-2019'], 
             details_df = shin_insider, 
             model_type = "random_forest")
AB_ord <- models(matches_df = matches[season == '2017-2018'], 
             details_df = shin_insider, 
             model_type = "random_forest",
             ordered = TRUE)
# A + C = shin_prob + average change rate
AC <- models(matches_df = matches[week == 48][season == '2018-2019'], 
             details_df = shin_changes, 
             model_type = "random_forest")
AC_ord <- models(matches_df = matches[week == 48][season == '2018-2019'], 
             details_df = shin_changes, 
             model_type = "random_forest",
             ordered = TRUE)
# A + B + C = shin_prob + insider + average change rate
ABC <- models(matches_df = matches[week == 48][season == '2018-2019'], 
             details_df = shin_changes_insider, 
             model_type = "random_forest")
ABC_ord <- models(matches_df = matches[week == 48][season == '2018-2019'], 
              details_df = shin_changes_insider, 
              model_type = "random_forest",
              ordered = TRUE)


for (i in c("random_forest", "decision_tree", "gradient_boosting")){
  AB <- models(matches_df = matches[week == 48][season == '2018-2019'], 
               details_df = shin, 
               model_type = i)
}


### report of model
# functions in this file:
# 1 - model_report
source("model_report.R")


### NOTE: Change the comment below about the input type
myRPS <- model_report("GLMNET", n, "Basic + Shin 48 Week", TrainSet, TestSet, trainStart, testStart, predictions, testRPS)

myRPS