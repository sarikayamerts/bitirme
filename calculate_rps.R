#widening to apply rps calculation
#first <- reshape(first, idvar = c("matchId","bookmaker"), timevar = c("oddtype"), direction = "wide")
#first <- merge(first, matches[, .(matchId, winner, season, week)], by = "matchId")
#setcolorder(first, c("matchId","bookmaker","z","norm_prob.odd1","norm_prob.oddX", "norm_prob.odd2","shin_prob.odd1","shin_prob.oddX", "shin_prob.odd2", "winner"))

lastrps <- copy(last)
lastrps <- reshape(lastrps, idvar = c("matchId", "bookmaker"), timevar = "oddtype", direction = "wide")
lastrps <- merge(lastrps, matches[, .(matchId, winner)], by = "matchId")
#lastrps <- merge(lastrps, matches[, .(matchId, winner, season, week)], by = "matchId")
#setcolorder(last, c("matchId","bookmaker","z","norm_prob.odd1","norm_prob.oddX", "norm_prob.odd2","shin_prob.odd1","shin_prob.oddX", "shin_prob.odd2", "winner"))

#rps calculation
#first <- first[, Basic_RPS := calculate_rps(norm_prob.odd1, norm_prob.oddX, norm_prob.odd2, winner), by = 1:nrow(first)]
#first <- first[, Shin_RPS := calculate_rps(shin_prob.odd1, shin_prob.oddX, shin_prob.odd2, winner), by = 1:nrow(first)]

#lastrps <- lastrps[, Basic_RPS := calculate_rps(norm_prob.odd1, norm_prob.oddX, norm_prob.odd2, winner), by = 1:nrow(lastrps)]
lastrps <- lastrps[, Shin_RPS := calculate_rps(shin_prob.odd1, shin_prob.oddX, shin_prob.odd2, winner), by = 1:nrow(lastrps)]
