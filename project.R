rm(list = ls())
options(warn=-1)
library(readr)
library(graphics)
library(data.table)
library(verification)

setwd("Downloads/Bitirme")

england_premier_league_details <- read_rds("england_premier_league_details.rds")
england_premier_league_raw <- read_rds("england_premier_league_raw.rds")

details <- data.table(england_premier_league_details)[, c("matchid", "bookmaker", "bettype", "oddtype", "odd"), with = FALSE]
matches <- data.table(england_premier_league_raw)[, c("matchid", "score"), with = FALSE]
rm(england_premier_league_details, england_premier_league_raw)

key(details) <- c("matchid", "bookmaker", "oddtype")
first <- details[unique(details[,key(details), with = FALSE]), mult = 'first']
last <- details[unique(details[,key(details), with = FALSE]), mult = 'last']
# to see specific rows
# details[matchid == "004f4ING" & bookmaker == "10Bet"] 

# unique(matches$score)
# matches[matchid == "0Ct34Nck"]
matches <- matches[score != "POSTP." & score != ""]

#To check if there is any 10-0 score or etc.
matches <- matches[, len := nchar(score)]


winner <- function(a){
  home <- as.numeric(substring(a, 1, 1))
  away <- as.numeric(substring(a, 3, 3))
  if(home > away) {output = "odd1"}
  if(home == away) {output = "oddX"}
  if(home < away) {output = "odd2"}
  output
}

over_under <- function(a){
  home <- as.numeric(substring(a, 1, 1))
  away <- as.numeric(substring(a, 3, 3))
  if(home + away >= 3) {output = "oddover"}
  if(home + away < 3) {output = "oddunder"}
  output
}

inverse <- function(a){1/as.numeric(a)}

matches$over_under <- matches[, over_under(score), by = 1:nrow(matches)]$V1
matches$winner <- matches[, winner(score), by = 1:nrow(matches)]$V1
first <- first[,probs := inverse(odd)]
last <- last[,probs := inverse(odd)]

#first_shin <- first
#last_shin <- last

### BASIC NORMALIZATION MODEL

first <- first[, norm_prob := probs/sum(probs), by=list(matchid,bookmaker,bettype)]
last <- last[, norm_prob := probs/sum(probs), by=list(matchid,bookmaker,bettype)]



### SHIN MODEL

fixed_point_iter <- function(l){
  a = l[1]
  b = l[2]
  c = l[3]
  
  if(is.na(a) || is.na(b) || is.na(c)){
    as.double(NA)
  }
  else{
    beta = a + b + c
    z_current = 0
    z_new = sqrt(z_current^2+4*(1-z_current)*a*a/beta)+sqrt(z_current^2+4*(1-z_current)*b*b/beta)+sqrt(z_current^2+4*(1-z_current)*c*c/beta)-2
    i = 1
    while (abs(z_new - z_current) > 0.001 && i < 21 ){
      z_current = z_new
      z_new = sqrt(z_current^2+4*(1-z_current)*a*a/beta)+sqrt(z_current^2+4*(1-z_current)*b*b/beta)+sqrt(z_current^2+4*(1-z_current)*c*c/beta)-2
      i = i+1
    }
    round((z_current+z_new)/2,3)
  }
}

shin_prob_calculator <- function(list){
  
  z <- fixed_point_iter(list)
  beta = list[1] + list[2] + list[3]
  (sqrt(z^2+4*(1-z)*list*list/beta)-z)/(2-2*z)
}

#first <- first[bettype == "1x2", z := fixed_point_iter(probs), by=list(matchid,bookmaker)]
#first <- first[bettype == "1x2", beta := sum(probs) , by=list(matchid,bookmaker)]
first <- first[bettype == "1x2", shin_prob := shin_prob_calculator(probs) , by=list(matchid,bookmaker)]

#last <- last[bettype == "1x2", z := fixed_point_iter(probs), by=list(matchid,bookmaker)]
#last <- last[bettype == "1x2", beta := sum(probs) , by=list(matchid,bookmaker)]
last <- last[bettype == "1x2", shin_prob := shin_prob_calculator(probs) , by=list(matchid,bookmaker)]

#Alttaki iki satýra gerek kalmadý
#first1x2 <- first[bettype == "1x2",c("matchid","bookmaker","oddtype","norm_prob","shin_prob")]
#last1x2 <- last[bettype == "1x2",c("matchid","bookmaker","oddtype","norm_prob","shin_prob")]

first1x2 <- first
last1x2 <- last

# rps = 1/(r-1) * sum{i = 1, over r} (sum{over i} p_j -sum{over i} e_j)^2
# where r is number of outcomes (3), p_j forecasted prob, e_j actual prob
# if 0.3, 0.5, 0.2 are forecasted prob and 1,0,0 actual prob
# then rps = 1/2 * (0.49+0.04) = 0.265
# rps(obs = c(1), pred = matrix(c(0.3, 0.5, 0.2), nrow = 1))

firstdata <- reshape(first1x2, idvar = c("matchid", "bookmaker"), timevar = "oddtype", direction = "wide")
firstdata <- merge(firstdata, matches[, .(matchid, over_under, winner)], by = "matchid")
setcolorder(firstdata, c("matchid","bookmaker","norm_prob.odd1","norm_prob.oddX", "norm_prob.odd2","shin_prob.odd1","shin_prob.oddX", "shin_prob.odd2", "winner"))

lastdata <- reshape(last1x2, idvar = c("matchid", "bookmaker"), timevar = "oddtype", direction = "wide")
lastdata <- merge(lastdata, matches[, .(matchid, over_under, winner)], by = "matchid")
setcolorder(lastdata, c("matchid","bookmaker","norm_prob.odd1","norm_prob.oddX", "norm_prob.odd2","shin_prob.odd1","shin_prob.oddX", "shin_prob.odd2", "winner"))

                         

# converting odd columns to row in order to use in rps function
# firstdata <- first[,.(norm_prob), by = .(matchid, bookmaker, oddtype)]
# firstdata <- reshape(firstdata, idvar = c("matchid", "bookmaker"), timevar = "oddtype", direction = "wide")
# firstdata <- merge(firstdata, matches[, .(matchid, over_under, winner)], by = "matchid")
# setcolorder(firstdata, c("matchid","bookmaker","norm_prob.odd1","norm_prob.oddX", "norm_prob.odd2","norm_prob.oddover",
#                          "norm_prob.oddunder", "over_under", "winner"))
# lastdata <- last[,.(norm_prob), by = .(matchid, bookmaker, oddtype)]
# lastdata <- reshape(lastdata, idvar = c("matchid", "bookmaker"), timevar = "oddtype", direction = "wide")
# lastdata <- merge(lastdata, matches[, .(matchid, over_under, winner)], by = "matchid")
# setcolorder(lastdata, c("matchid","bookmaker","norm_prob.odd1","norm_prob.oddX", "norm_prob.odd2","norm_prob.oddover",
#                         "norm_prob.oddunder", "over_under", "winner"))

calculate_rps <- function(a,b,c,d){
  if(is.na(a) || is.na(b) || is.na(c)){
    as.double(NA)
  }
  else{
    if (d == "odd1") {d=1}
    if (d == "oddX") {d=2}
    if (d == "odd2") {d=3}
    pred = t(matrix(c(a, b, c)))
    output <- rps(obs = c(d), pred = pred)
    output$rps
  }
}

#for over under
# calculate_rps2 <- function(a,b,c){
#   if(is.na(a) || is.na(b)){
#     as.double(NA)
#   }
#   else{
#     if (c == "oddover") {c=1}
#     if (c == "oddunder") {c=2}
#     pred = t(matrix(c(a, b)))
#     output <- rps(obs = c(c), pred = pred)
#     output$rps
#   }
# }

Basic_RPS <- firstdata[, calculate_rps(norm_prob.odd1, norm_prob.oddX, norm_prob.odd2, winner), by = 1:nrow(firstdata)]
firstdata$Basic_RPS <- Basic_RPS$V1
Shin_RPS <- firstdata[, calculate_rps(shin_prob.odd1, shin_prob.oddX, shin_prob.odd2, winner), by = 1:nrow(firstdata)]
firstdata$Shin_RPS <- Shin_RPS$V1
# OverUnderRPS <- firstdata[, calculate_rps2(norm_prob.oddover, norm_prob.oddunder, over_under), by = 1:nrow(firstdata)]
# firstdata$RPS_overunder <- OverUnderRPS$V1

Basic_RPS <- lastdata[, calculate_rps(norm_prob.odd1, norm_prob.oddX, norm_prob.odd2, winner), by = 1:nrow(lastdata)]
lastdata$Basic_RPS <- Basic_RPS$V1
Shin_RPS <- lastdata[, calculate_rps(shin_prob.odd1, shin_prob.oddX, shin_prob.odd2, winner), by = 1:nrow(lastdata)]
lastdata$Shin_RPS <- Shin_RPS$V1
# OverUnderRPS <- lastdata[, calculate_rps2(norm_prob.oddover, norm_prob.oddunder, over_under), by = 1:nrow(lastdata)]
# lastdata$RPS_overunder <- OverUnderRPS$V1

rm(Basic_RPS, Shin_RPS)

average <- firstdata[, .(var = mean(Basic_RPS, na.rm = TRUE)), by = bookmaker]
average <- merge(average, firstdata[, .(var = mean(Shin_RPS, na.rm = TRUE)), by = bookmaker], by = "bookmaker")
average <- merge(average, lastdata[, .(var = mean(Basic_RPS, na.rm = TRUE)), by = bookmaker], by = "bookmaker")
average <- merge(average, lastdata[, .(var = mean(Shin_RPS, na.rm = TRUE)), by = bookmaker], by = "bookmaker")
colnames(average) <- c("bookmaker","First_Basic", "First_Shin", "Last_Basic", "Last_Shin")


# Subsetting wrt best bookmaker's odds

df <- firstdata[bookmaker == "1xBet"]
df <- df[complete.cases(df), ]
df$winner_category <- sapply(as.character(df$winner), switch, "odd1" = 1, "oddX" = 2, "odd2" = 3, USE.NAMES = F)
df <- subset(df, select = c('shin_prob.odd1', 'shin_prob.oddX', 'shin_prob.odd2', 'winner_category'))
smp_size <- floor(0.75 * nrow(df))
set.seed(123)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)
train <- df[train_ind, ]
test <- df[-train_ind, ]

mult <- multinom(winner_category ~ ., data =train)
summary(mult)
pred<-predict(mult,test, "probs")
pred



