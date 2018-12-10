### sets directory easily
# 1 - set_directory(name)
# name can be "code", "data"
source("set_directory.R")

### implementation of converting match results from string to {over, under, 1, X, 2} types of outcome
# functions in this file:
# 1 - winner(score) 
# 2 - over_under(score)
# 3 - inverse(odd)
source("match_scores.R")

### converting dates to seasons
# functions in this file:
# 1 - season_calc(date) 
source("season_calculator.R")

set_directory("data")

#read raw data
matches <- read_rds("df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds")
details <- read_rds("df9b1196-e3cf-4cc7-9159-f236fe738215_odd_details.rds")

#prepare matches
matches <- data.table(matches)[, c("matchId", "score", "home", "away", "date"), with = FALSE]
matches <- unique(matches)
matches[,date:=anydate(date)]

next_matches <- matches[is.na(score)]
matches <- matches[!is.na(score)]

matches$over_under <- matches[, over_under(score), by = 1:nrow(matches)]$V1
matches$winner <- matches[, winner(score), by = 1:nrow(matches)]$V1
matches$week <- matches[, strftime(date-1, format = "%V"), by = 1:nrow(matches)]$V1
matches$season <- matches[, season_calc(date), by = 1:nrow(matches)]$V1

next_matches$winner <- next_matches[, winner(score), by = 1:nrow(next_matches)]$V1
next_matches$week <- next_matches[, strftime(date-1, format = "%V"), by = 1:nrow(next_matches)]$V1
next_matches$season <- next_matches[, season_calc(date), by = 1:nrow(next_matches)]$V1

#prepare details
details <- data.table(details)[, c("matchId", "bookmaker", "betType", "oddtype", "odd", "totalhandicap"), with = FALSE]
details <- details[bookmaker != 'Betfair Exchange']

details_otherbets <- details[betType != "1x2"]
details <- details[betType == '1x2']
details[, c("totalhandicap" , "betType") := NULL]

#prepare first & last
key(details) <- c("matchId", "bookmaker", "oddtype")
#first <- details[unique(details[,key(details), with = FALSE]), mult = 'first']
last <- details[unique(details[,key(details), with = FALSE]), mult = 'last']

set_directory("code")
