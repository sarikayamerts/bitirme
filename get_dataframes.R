### mert's macbook database directory
#setwd("/Users/mertsarikaya/Downloads/Bitirme/")
### mert's windows database directory
#setwd("")
### emre's database directory
setwd("C:/Users/Hp/Desktop/Bitirme")

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

#prepare details
details <- data.table(details)[, c("matchId", "bookmaker", "betType", "oddtype", "odd", "totalhandicap"), with = FALSE]
details <- details[bookmaker != 'Betfair Exchange']

details_otherbets <- details[betType != "1x2"]
details <- details[betType == '1x2']
details[, c("totalhandicap" , "betType") := NULL]

#prepare first & last
key(details) <- c("matchId", "bookmaker", "oddtype")
first <- details[unique(details[,key(details), with = FALSE]), mult = 'first']
last <- details[unique(details[,key(details), with = FALSE]), mult = 'last']

### mert's macbook github directory
#setwd("/Users/mertsarikaya/bitirme/")
### mert's windows github directory
# setwd("")
### emre's github directory
setwd("C:/Users/Hp/Desktop/Bitirme/bitirme")
