### mert's macbook database directory
setwd("/Users/mertsarikaya/Downloads/Bitirme/")
### mert's windows database directory
#setwd("")
### emre's database directory
#setwd("C:/Users/Hp/Desktop/Bitirme")

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

matches$over_under <- matches[, over_under(score), by = 1:nrow(matches)]$V1
matches$winner <- matches[, winner(score), by = 1:nrow(matches)]$V1

first <- first[bettype == "1x2"]
last <- last[bettype == "1x2"]


#calculating implied probabilities
first <- first[bettype == "1x2",probs := inverse(odd)]
last <- last[bettype == "1x2",probs := inverse(odd)]
first <- first[probs != "NA"]
last <- last[probs != "NA"]

#calculating booksum to detect abnormalies
first <- first[, booksum := sum(probs), by=list(matchid,bookmaker)]
first <- first[booksum <= 1.5 & booksum >= 1.0]

last <- last[, booksum := sum(probs), by=list(matchid,bookmaker)]
last <- last[booksum <= 1.5 & booksum >= 1.0]

first[, c("bettype", "odd", "booksum") := NULL]
last[, c("bettype", "odd", "booksum") := NULL]

#basic normalization
first <- first[, norm_prob := probs/sum(probs), by=list(matchid,bookmaker)]
last <- last[, norm_prob := probs/sum(probs), by=list(matchid,bookmaker)]

#shin normalization
first <- first[, shin_prob := round(shin_prob_calculator(probs), digits = 7) , by=list(matchid,bookmaker)]
last <- last[, shin_prob := round(shin_prob_calculator(probs), digits = 7) , by=list(matchid,bookmaker)]

#widening to apply rps calculation
first[, c("probs") := NULL]
first <- reshape(first, idvar = c("matchid", "bookmaker"), timevar = "oddtype", direction = "wide")
first <- merge(first, matches[, .(matchid, winner)], by = "matchid")
setcolorder(first, c("matchid","bookmaker","norm_prob.odd1","norm_prob.oddX", "norm_prob.odd2","shin_prob.odd1","shin_prob.oddX", "shin_prob.odd2", "winner"))

last[, c("probs") := NULL]
last <- reshape(last, idvar = c("matchid", "bookmaker"), timevar = "oddtype", direction = "wide")
last <- merge(last, matches[, .(matchid, winner)], by = "matchid")
setcolorder(last, c("matchid","bookmaker","norm_prob.odd1","norm_prob.oddX", "norm_prob.odd2","shin_prob.odd1","shin_prob.oddX", "shin_prob.odd2", "winner"))

#rps calculation
Basic_RPS <- first[, calculate_rps(norm_prob.odd1, norm_prob.oddX, norm_prob.odd2, winner), by = 1:nrow(first)]
first$Basic_RPS <- Basic_RPS$V1
Shin_RPS <- first[, calculate_rps(shin_prob.odd1, shin_prob.oddX, shin_prob.odd2, winner), by = 1:nrow(first)]
first$Shin_RPS <- Shin_RPS$V1

Basic_RPS <- last[, calculate_rps(norm_prob.odd1, norm_prob.oddX, norm_prob.odd2, winner), by = 1:nrow(last)]
last$Basic_RPS <- Basic_RPS$V1
Shin_RPS <- last[, calculate_rps(shin_prob.odd1, shin_prob.oddX, shin_prob.odd2, winner), by = 1:nrow(last)]
last$Shin_RPS <- Shin_RPS$V1

rm(Basic_RPS, Shin_RPS)
