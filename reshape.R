#widening first and last for feature extraction
wide_first <- reshape(first[,-4], idvar = c("matchId", "bookmaker"), timevar = c("oddtype"), direction = "wide")
wide_first <- reshape(wide_first, idvar = c("matchId"), timevar = c("bookmaker"), direction = "wide")
wide_first <- merge(wide_first, matches[, .(matchId, winner, date, week, season)], by = "matchId")

wide_last <- reshape(last[,-4], idvar = c("matchId", "bookmaker"), timevar = c("oddtype"), direction = "wide")
wide_last <- reshape(wide_last, idvar = c("matchId"), timevar = c("bookmaker"), direction = "wide")
wide_last <- merge(wide_last, matches[, .(matchId, winner, date, week, season)], by = "matchId")
