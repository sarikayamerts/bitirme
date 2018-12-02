#widening to apply rps calculation
first <- reshape(first, idvar = c("matchId","bookmaker"), timevar = c("oddtype"), direction = "wide")
first <- merge(first, matches[, .(matchId, winner, season, week)], by = "matchId")
#setcolorder(first, c("matchId","bookmaker","z","norm_prob.odd1","norm_prob.oddX", "norm_prob.odd2","shin_prob.odd1","shin_prob.oddX", "shin_prob.odd2", "winner"))

last <- reshape(last, idvar = c("matchId", "bookmaker"), timevar = "oddtype", direction = "wide")
last <- merge(last, matches[, .(matchId, winner, season, week)], by = "matchId")
#setcolorder(last, c("matchId","bookmaker","z","norm_prob.odd1","norm_prob.oddX", "norm_prob.odd2","shin_prob.odd1","shin_prob.oddX", "shin_prob.odd2", "winner"))

#rps calculation
first <- first[, Basic_RPS := calculate_rps(norm_prob.odd1, norm_prob.oddX, norm_prob.odd2, winner), by = 1:nrow(first)]
first <- first[, Shin_RPS := calculate_rps(shin_prob.odd1, shin_prob.oddX, shin_prob.odd2, winner), by = 1:nrow(first)]

last <- last[, Basic_RPS := calculate_rps(norm_prob.odd1, norm_prob.oddX, norm_prob.odd2, winner), by = 1:nrow(last)]
last <- last[, Shin_RPS := calculate_rps(shin_prob.odd1, shin_prob.oddX, shin_prob.odd2, winner), by = 1:nrow(last)]
