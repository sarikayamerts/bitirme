### mert's macbook database directory
setwd("/Users/mertsarikaya/Downloads/Bitirme/")
### mert's windows database directory
#setwd("")
### emre's database directory
#setwd("C:/Users/Hp/Desktop/Bitirme")

matches <- read_rds("df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds")
details <- read_rds("df9b1196-e3cf-4cc7-9159-f236fe738215_odd_details.rds")

matches <- data.table(matches)[, c("matchId", "score", "home", "away", "date"), with = FALSE]
matches <- unique(matches)
matches[,date:=anydate(date)]

next_matches <- matches[is.na(score)]
matches <- matches[!is.na(score)]

details <- data.table(details)[, c("matchId", "bookmaker", "betType", "oddtype", "odd", "totalhandicap"), with = FALSE]
details <- details[betType == '1x2']
details <- details[bookmaker != 'Betfair Exchange']

key(details) <- c("matchId", "bookmaker", "oddtype")
first <- details[unique(details[,key(details), with = FALSE]), mult = 'first']
last <- details[unique(details[,key(details), with = FALSE]), mult = 'last']
# to see specific rows
# details[matchId == "004f4ING" & bookmaker == "10Bet"] 


# unique(matches$score)
# matches[matchId == "0Ct34Nck"]

matches$over_under <- matches[, over_under(score), by = 1:nrow(matches)]$V1
matches$winner <- matches[, winner(score), by = 1:nrow(matches)]$V1
matches$season <- matches[, season_calc(date), by = 1:nrow(matches)]$V1

first[, totalhandicap := NULL]
last[, totalhandicap := NULL]


#calculating implied probabilities
first <- first[,probs := inverse(odd)]
last <- last[,probs := inverse(odd)]


#calculating booksum to detect abnormalies
#i am removing these for now (betexchange i kötü görmek istiyorum)
#first <- first[,booksum := sum(probs),by=list(matchId,bookmaker)]
#first <- first[booksum <= 1.15]

#last <- last[, booksum := sum(probs), by=list(matchId,bookmaker)]
#last <- last[booksum <= 1.15]

first[, c("betType", "odd") := NULL]
last[, c("betType", "odd") := NULL]

#basic normalization
first <- first[, norm_prob := probs/sum(probs), by=list(matchId,bookmaker)]
last <- last[, norm_prob := probs/sum(probs), by=list(matchId,bookmaker)]

#shin normalization
first <- first[, shin_prob := round(shin_prob_calculator(probs), digits = 7) , by=list(matchId,bookmaker)]
last <- last[, shin_prob := round(shin_prob_calculator(probs), digits = 7) , by=list(matchId,bookmaker)]


#prob_comparison <- reshape(first[,c(1,2,3,6)], idvar = c("matchId","bookmaker"), timevar = c("oddtype"), direction = "wide")
#prob_comparison <- rbind(prob_comparison, reshape(last[,c(1,2,3,6)], idvar = c("matchId","bookmaker"), timevar = c("oddtype"), direction = "wide"))

changes <- merge(first[,c(1,2,3,6)], last[,c(1,2,3,6)], c('matchId', 'bookmaker', 'oddtype'))
changes$change <- (changes$shin_prob.y - changes$shin_prob.x)/changes$shin_prob.x
changes <- changes[order(changes$change, decreasing = TRUE),]
changes_matches <- changes[, .(avg_change = mean(change)), by = c("matchId", "oddtype")]
changes_matches <- changes_matches[order(changes_matches$avg_change, decreasing = TRUE),]
changes_matches <- reshape(changes_matches, idvar = c("matchId"), timevar = c("oddtype"), direction = "wide")
changes_matches <- merge(changes_matches, matches[,c(1,2)], 'matchId')
changes_matches <- merge(changes_matches, changes[, .(avg_change_total = mean(change)), by = c("matchId")], 'matchId')
setcolorder(changes_matches, c("matchId", "avg_change.odd1", "avg_change.oddX", "avg_change.odd2", "score", "avg_change_total"))

#insider traders calculating
#first <- first[, z := z_calculator(probs) , by=list(matchId,bookmaker)]
#last <- last[, z := z_calculator(probs) , by=list(matchId,bookmaker)]

#Last Shin'e göre en iyileri: Betfair, ComeOn, 888sport, 188BET, 12bet, bet365, SBOBET, 1xBet, bet-at-home, Pinnacle
#First Shin'e göre en iyileri: 1xBet, Betfair, ComeOn, 888sport, Betsafe, Betsson, Pinnacle

#widening to apply rps calculation
#first[, c("probs") := NULL]
first[, c("probs", "norm_prob") := NULL]
first <- first[bookmaker %in% c('Betfair', 'ComeOn', '888sport', '1xBet', '188BET')]
wide_first <- reshape(first, idvar = c("matchId", "bookmaker"), timevar = c("oddtype"), direction = "wide")
wide_first <- reshape(wide_first, idvar = c("matchId"), timevar = c("bookmaker"), direction = "wide")
wide_first <- merge(wide_first, matches[, .(matchId, winner)], by = "matchId")
first <- reshape(first, idvar = c("matchId","bookmaker"), timevar = c("oddtype"), direction = "wide")
first <- merge(first, matches[, .(matchId, winner, season)], by = "matchId")
#setcolorder(first, c("matchId","bookmaker","z","norm_prob.odd1","norm_prob.oddX", "norm_prob.odd2","shin_prob.odd1","shin_prob.oddX", "shin_prob.odd2", "winner"))
booksum_df <- first[,c(2,3,13)]


last[, c("probs") := NULL]
#last[, c("probs", "norm_prob") := NULL]
wide_last <- reshape(last, idvar = c("matchId", "bookmaker"), timevar = c("oddtype"), direction = "wide")
wide_last <- reshape(wide_last, idvar = c("matchId"), timevar = c("bookmaker"), direction = "wide")
wide_last <- merge(wide_last, matches[, .(matchId, winner)], by = "matchId")
#last <- reshape(last, idvar = c("matchId", "bookmaker"), timevar = "oddtype", direction = "wide")
last <- reshape(last, idvar = c("matchId", "bookmaker"), timevar = "oddtype", direction = "wide")
last <- merge(last, matches[, .(matchId, winner, season)], by = "matchId")
#setcolorder(last, c("matchId","bookmaker","z","norm_prob.odd1","norm_prob.oddX", "norm_prob.odd2","shin_prob.odd1","shin_prob.oddX", "shin_prob.odd2", "winner"))

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
