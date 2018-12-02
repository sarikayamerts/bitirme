#calculating implied probabilities
first <- first[,probs := inverse(odd)]
last <- last[,probs := inverse(odd)]

first[, odd := NULL]
last[, odd := NULL]

#basic normalization
first <- first[, norm_prob := probs/sum(probs), by=list(matchId,bookmaker)]
last <- last[, norm_prob := probs/sum(probs), by=list(matchId,bookmaker)]

#shin normalization
first <- first[, shin_prob := round(shin_prob_calculator(probs), digits = 7) , by=list(matchId,bookmaker)]
last <- last[, shin_prob := round(shin_prob_calculator(probs), digits = 7) , by=list(matchId,bookmaker)]

#deleting implied probs
first[, c("probs") := NULL]
last[, c("probs") := NULL]
