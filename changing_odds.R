changes <- merge(first[,c(1,2,3,5)], last[,c(1,2,3,5)], c('matchId', 'bookmaker', 'oddtype'))
changes$change <- (changes$shin_prob.y - changes$shin_prob.x)/changes$shin_prob.x
#changes <- changes[order(changes$change, decreasing = TRUE),]
changes[, c("shin_prob.x") := NULL]
#changes[oddtype != "oddX"]
colnames(changes) <- c("matchId", "bookmaker", "oddtype", "shin_prob", "change")
#changes_matches <- changes[, .(avg_change = mean(change)), by = c("matchId", "oddtype")]
#changes_matches <- changes_matches[order(changes_matches$avg_change, decreasing = TRUE),]
#changes <- reshape(changes, idvar = c("matchId","bookmaker"), timevar = c("oddtype"), direction = "wide")
#changes_matches <- merge(changes_matches, matches[,c(1,2)], 'matchId')
#changes_matches <- merge(changes_matches, changes[, .(avg_change_total = mean(change)), by = c("matchId")], 'matchId')
#setcolorder(changes_matches, c("matchId", "avg_change.odd1", "avg_change.oddX", "avg_change.odd2", "score", "avg_change_total"))
