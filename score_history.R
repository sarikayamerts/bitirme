a <- matches[season == '2010-2011']
a$home_point <- 0
a$away_point <- 0

x <- data.table("team" = unique(a$home), "point" = 0)
point_history <- function(home,away,winner){
  if (winner == "odd1"){
    x[team == home]$point <<- x[team == home]$point + 3 
  }
  if (winner == "oddX"){
    x[team == home]$point <<- x[team == home]$point + 1
    x[team == away]$point <<- x[team == away]$point + 1
  }
  if (winner == "odd2"){
    x[team == away]$point <<- x[team == away]$point + 3 
  }
}
for (i in 1:nrow(a)){
  a$home_point[i] <- x[team == a$home[i]]$point
  a$away_point[i] <- x[team == a$away[i]]$point
  point_history(a$home[i], a$away[i], matches$winner[i])
}

b <- a[home == 'tottenham' | away == 'tottenham']
last_matches <- function(home, away, winner, day){
  c <- tail(b[date < day],5)
  c<- c[seq(dim(c)[1],1),]
  text = ''
  for (i in 1:nrow(c)){
    if(c$winner[i] == "odd1" & c$home[i] == 'tottenham'){text = paste(text, 'W', sep = '')}
    else if(c$winner[i] == "odd1"){text = paste(text, 'L', sep = '')}
    if(c$winner[i] == "odd2" & c$away[i] == 'tottenham'){text = paste(text, 'W', sep = '')}
    else if(c$winner[i] == "odd2"){text = paste(text, 'L', sep = '')}
    if (c$winner[i] == "oddX"){text = paste(text, 'D', sep = '')}
  }
}