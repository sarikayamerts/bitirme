### clears the environment
rm(list = ls())

### mert's macbook github directory
#setwd("/Users/mertsarikaya/bitirme/")
### mert's windows github directory
# setwd("")
### emre's github directory
 setwd("C:/Users/Hp/Desktop/Bitirme/bitirme")

library(readr)
library(graphics)
library(data.table)
library(verification)
library(nnet)

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
source("rps.R")

# 1 - predictions
# 2 - confusion matrix
source("model_implementation.R")

### importing data and manipulating it to calculate normalized (basic and shin) probabilities for each match & bookmaker
# datatables in this script:
# 1 - details (matchid, bookmaker, bettype, oddtype, odd)
# 2 - matches (matchid, score, over_under, winner)
# 3 - first (matchid, bookmaker, norm_prob.odd1-oddX-odd2, shin_prob.odd1-oddX-odd2, winner)
# 4 - last (matchid, bookmaker, norm_prob.odd1-oddX-odd2, shin_prob.odd1-oddX-odd2, winner)
source("data_manipulation.R")

### Calculating average RPS's for each bookmakers (smaller values are better)
average <- first[, .(var = mean(Basic_RPS, na.rm = TRUE)), by = bookmaker]
average <- merge(average, first[, .(var = mean(Shin_RPS, na.rm = TRUE)), by = bookmaker], by = "bookmaker")
average <- merge(average, last[, .(var = mean(Basic_RPS, na.rm = TRUE)), by = bookmaker], by = "bookmaker")
average <- merge(average, last[, .(var = mean(Shin_RPS, na.rm = TRUE)), by = bookmaker], by = "bookmaker")
colnames(average) <- c("bookmaker","First_Basic", "First_Shin", "Last_Basic", "Last_Shin")


# 
df <- first[bookmaker == "1xBet" | bookmaker == "Betfair" | bookmaker == "ComeOn" | bookmaker == "888Sport" | bookmaker == "Pinnacle" | bookmaker == "Betsafe"]
df <- df[complete.cases(df), ]
multinomial_model(df)

model_RPS <- predictions[, calculate_rps(odd1, oddX, odd2, winner), by = 1:nrow(predictions)]
predictions$RPS <- model_RPS$V1
rm(model_RPS)
avg <- mean(predictions$RPS)


#CLUSTER
clusters <- hclust(dist(test[, 3:5]))
plot(clusters)
clusterCut <- cutree(clusters, 10)
table(clusterCut, test$winner_category)
table(clusterCut, test$pred)

