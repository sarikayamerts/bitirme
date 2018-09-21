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

OverUnder <- matches[, over_under(score), by = 1:nrow(matches)]
matches$over_under <- OverUnder$V1
HomeDrawAway <- matches[, winner(score), by = 1:nrow(matches)]
matches$winner <- HomeDrawAway$V1
Probabilities <- first[, inverse(odd), by = 1:nrow(first)]
first$probs <- Probabilities$V1
Probabilities2 <- last[, inverse(odd), by = 1:nrow(last)]
last$probs <- Probabilities2$V1
rm(HomeDrawAway, OverUnder, Probabilities, Probabilities2)

first_shin <- first
last_shin <- last
first <- first[, norm_prob := probs/sum(probs), by=list(matchid,bookmaker,bettype)]
last <- last[, norm_prob := probs/sum(probs), by=list(matchid,bookmaker,bettype)]

# rps = 1/(r-1) * sum{i = 1, over r} (sum{over i} p_j -sum{over i} e_j)^2
# where r is number of outcomes (3), p_j forecasted prob, e_j actual prob
# if 0.3, 0.5, 0.2 are forecasted prob and 1,0,0 actual prob
# then rps = 1/2 * (0.49+0.04) = 0.265
# rps(obs = c(1), pred = matrix(c(0.3, 0.5, 0.2), nrow = 1))

# converting odd columns to row in order to use in rps function
firstdata <- first[,.(norm_prob), by = .(matchid, bookmaker, oddtype)]
firstdata <- reshape(firstdata, idvar = c("matchid", "bookmaker"), timevar = "oddtype", direction = "wide")
firstdata <- merge(firstdata, matches[, .(matchid, over_under, winner)], by = "matchid")
setcolorder(firstdata, c("matchid","bookmaker","norm_prob.odd1","norm_prob.oddX", "norm_prob.odd2","norm_prob.oddover",
                         "norm_prob.oddunder", "over_under", "winner"))
lastdata <- last[,.(norm_prob), by = .(matchid, bookmaker, oddtype)]
lastdata <- reshape(lastdata, idvar = c("matchid", "bookmaker"), timevar = "oddtype", direction = "wide")
lastdata <- merge(lastdata, matches[, .(matchid, over_under, winner)], by = "matchid")
setcolorder(lastdata, c("matchid","bookmaker","norm_prob.odd1","norm_prob.oddX", "norm_prob.odd2","norm_prob.oddover",
                        "norm_prob.oddunder", "over_under", "winner"))

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

calculate_rps2 <- function(a,b,c){
  if(is.na(a) || is.na(b)){
    as.double(NA)
  }
  else{
    if (c == "oddover") {c=1}
    if (c == "oddunder") {c=2}
    pred = t(matrix(c(a, b)))
    output <- rps(obs = c(c), pred = pred)
    output$rps
  }
}

HomeAwayRPS <- firstdata[, calculate_rps(norm_prob.odd1, norm_prob.oddX, norm_prob.odd2, winner), by = 1:nrow(firstdata)]
firstdata$RPS_homeaway <- HomeAwayRPS$V1
OverUnderRPS <- firstdata[, calculate_rps2(norm_prob.oddover, norm_prob.oddunder, over_under), by = 1:nrow(firstdata)]
firstdata$RPS_overunder <- OverUnderRPS$V1

HomeAwayRPS <- lastdata[, calculate_rps(norm_prob.odd1, norm_prob.oddX, norm_prob.odd2, winner), by = 1:nrow(lastdata)]
lastdata$RPS_homeaway <- HomeAwayRPS$V1
OverUnderRPS <- lastdata[, calculate_rps2(norm_prob.oddover, norm_prob.oddunder, over_under), by = 1:nrow(lastdata)]
lastdata$RPS_overunder <- OverUnderRPS$V1

rm(HomeAwayRPS, OverUnderRPS)

average <- firstdata[, .(var = mean(RPS_homeaway, na.rm = TRUE)), by = bookmaker]
average <- merge(average, firstdata[, .(var = mean(RPS_overunder, na.rm = TRUE)), by = bookmaker], by = "bookmaker")
average <- merge(average, lastdata[, .(var = mean(RPS_homeaway, na.rm = TRUE)), by = bookmaker], by = "bookmaker")
average <- merge(average, lastdata[, .(var = mean(RPS_overunder, na.rm = TRUE)), by = bookmaker], by = "bookmaker")
colnames(average) <- c("bookmaker","First_HomeAway", "First_OverUnder", "Last_HomeAway", "Last_OverUnder")


### SHIN MODEL


fixed_point_iter <- function(a,b,c){
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

firstdata <- first_shin[,.(probs), by = .(matchid, bookmaker, oddtype)]
firstdata <- reshape(firstdata, idvar = c("matchid", "bookmaker"), timevar = "oddtype", direction = "wide")
firstdata <- merge(firstdata, matches[, .(matchid, over_under, winner)], by = "matchid")
# setcolorder(firstdata, c("matchid","bookmaker","norm_prob.odd1","norm_prob.oddX", "norm_prob.odd2","norm_prob.oddover",
#                         "norm_prob.oddunder", "over_under", "winner"))

firstdata <- firstdata[, z := fixed_point_iter(probs.odd1,probs.odd2,probs.oddX), by=list(matchid,bookmaker)]
firstdata <- firstdata[, beta := probs.odd1 + probs.odd2 + probs.oddX , by=list(matchid,bookmaker)]

shin_prob_calculator <- function(a, b, z){
  (sqrt(z^2+4*(1-z)*a*a/b)-z)/(2-2*z)
}

firstdata <- firstdata[, shin.odd1 := shin_prob_calculator(probs.odd1, beta, z) , by=list(matchid,bookmaker)]
firstdata <- firstdata[, shin.oddX := shin_prob_calculator(probs.oddX, beta, z) , by=list(matchid,bookmaker)]
firstdata <- firstdata[, shin.odd2 := shin_prob_calculator(probs.odd2, beta, z) , by=list(matchid,bookmaker)]

