### implementation of converting match results from string to {over, under, 1, X, 2} types of outcome
# functions in this file:
# 1 - winner(score) 
# 2 - over_under(score)
# 3 - inverse(odd)
source("match_scores.R")

### implementation of shin probability calculation 
# functions in this file:
# 1 - shin_prob_calculator(list)
# 2 - z_calculator(list)
source("shin.R")

#calculating implied probabilities
#first <- first[,probs := inverse(odd)]
last <- last[,probs := inverse(odd)]

#first[, odd := NULL]
last[, odd := NULL]

#basic normalization
#first <- first[, norm_prob := probs/sum(probs), by=list(matchId,bookmaker)]
#last <- last[, norm_prob := probs/sum(probs), by=list(matchId,bookmaker)]

#shin normalization
#first <- first[, shin_prob := round(shin_prob_calculator(probs), digits = 7) , by=list(matchId,bookmaker)]
last <- last[, shin_prob := round(shin_prob_calculator(probs), digits = 7) , by=list(matchId,bookmaker)]

#insider trading
insider <- last[,list(z=z_calculator(probs)),list(matchId,bookmaker)]

#deleting implied probs
#first[, c("probs") := NULL]
last[, c("probs") := NULL]
