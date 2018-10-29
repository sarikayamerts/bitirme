for (i in 1:length(unique(matches$season))){
  league_season <- matches[season == unique(matches$season)[i]]
  league_season$home_point <- 0
  league_season$away_point <- 0
  
  score_table <- data.table("team" = unique(league_season$home), "point" = 0)
  point_history <- function(home,away,winner){
    if (winner == "odd1"){
      score_table[team == home]$point <<- score_table[team == home]$point + 3 
    }
    if (winner == "oddX"){
      score_table[team == home]$point <<- score_table[team == home]$point + 1
      score_table[team == away]$point <<- score_table[team == away]$point + 1
    }
    if (winner == "odd2"){
      score_table[team == away]$point <<- score_table[team == away]$point + 3 
    }
    score_table <<- score_table[order(-point)]
  }
  for (j in 1:nrow(league_season)){
    k = which(matches$matchId == league_season$matchId[j])
    matches$home_point[k] <- score_table[team == league_season$home[j]]$point
    matches$away_point[k] <- score_table[team == league_season$away[j]]$point
    point_history(league_season$home[j], league_season$away[j], league_season$winner[j])
  }
}
# 
# b <- a[home == 'tottenham' | away == 'tottenham']
# last_matches <- function(home, away, winner, day){
#   c <- tail(b[date < day],5)
#   c<- c[seq(dim(c)[1],1),]
#   text = ''
#   for (i in 1:nrow(c)){
#     if(c$winner[i] == "odd1" & c$home[i] == 'tottenham'){text = paste(text, 'W', sep = '')}
#     else if(c$winner[i] == "odd1"){text = paste(text, 'L', sep = '')}
#     if(c$winner[i] == "odd2" & c$away[i] == 'tottenham'){text = paste(text, 'W', sep = '')}
#     else if(c$winner[i] == "odd2"){text = paste(text, 'L', sep = '')}
#     if (c$winner[i] == "oddX"){text = paste(text, 'D', sep = '')}
#   }
# }