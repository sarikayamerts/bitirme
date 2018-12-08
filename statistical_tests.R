### Friedman post-hoc Nemenyi Tests
# functions in this file:
# 1- basic_vs_shin
# 2- 

basic_vs_shin <- function(df){
  test_df <- df[,c(1,2,10,11,12,13)]
  
  
  basic_vs_shin <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("bookmaker", "p_value","is_signif"))
  limit <- 0.001
  
  
  for(i in unique(df$bookmaker)){
    which_book <- i
    test_data <- test_df[bookmaker == which_book,c(1,5,6)]
    
    test_data_long <- melt(test_data, id.vars = c("matchId"))
    
    obj <- friedman.test(test_data_long$value, test_data_long$variable, test_data_long$matchId)
    basic_vs_shin[nrow(basic_vs_shin) + 1,] <- list(noquote(which_book), obj$p.value, (obj$p.value<=limit) )
    
  }
  
  basic_vs_shin
}

bookmaker_comp_friedman <- function(df,bookies){
  #which_season <- season
  #test_df <- df[,c(1,2,10,13)]
  test_df <- df[,c(1,2,13)]
  #test_df <- test_df[season == which_season]
  test_df <- test_df[bookmaker %in% bookies]
  test_df <- test_df[, rank:= rank(Shin_RPS,ties.method = "first"), by = matchId][,c(1,2,4)]
  test_df <- reshape(test_df, idvar = c("matchId"), timevar = c("bookmaker"), direction = "wide")
  test_df <- test_df[complete.cases(test_df)]
  test_data_long <- melt(test_df, id.vars = c("matchId"))

  limit <- 0.001
  obj <- friedman.test(test_data_long$value, test_data_long$variable, test_data_long$matchId)
  
  
  
  obj$p.value
}

bookmaker_comp_nemenyi <- function(df,bookies){
  #which_season <- season
  #test_df <- df[,c(1,2,10,13)]
  test_df <- df[,c(1,2,13)]
  #test_df <- test_df[season == which_season]
  test_df <- test_df[bookmaker %in% bookies]
  test_df <- test_df[, rank:= rank(Shin_RPS,ties.method = "first"), by = matchId][,c(1,2,4)]
  test_df <- reshape(test_df, idvar = c("matchId"), timevar = c("bookmaker"), direction = "wide")
  test_df <- test_df[complete.cases(test_df)]
  test_df <- test_df[, c("matchId") := NULL]
  myMatrix <- data.matrix(test_df)
 
  
  obj <- posthoc.friedman.nemenyi.test(myMatrix, p.adjust.method = "none")

  p_values <- data.table(obj$p.value)
}

bookmaker_comp_plot <- function(df,bookies){
  test_df <- df[,c(1,2,13)]
  #test_df <- test_df[season == which_season]
  test_df <- test_df[bookmaker %in% bookies]
  test_df <- reshape(test_df, idvar = c("matchId"), timevar = c("bookmaker"), direction = "wide")
  test_df <- test_df[complete.cases(test_df)]
  
  n <- nrow(test_df)
  matchesToKeep <- unique(test_df$matchId)
  
  
  test_df <- df[,c(1,2,13)]
  #test_df <- test_df[season == which_season]
  test_df <- test_df[bookmaker %in% bookies]
  test_df <- test_df[matchId %in% matchesToKeep]
  
  test_df <- test_df[, rank:= rank(Shin_RPS,ties.method = "first"), by = matchId][,c(1,2,4)]
  
  ave_df <- test_df[, ave_rank:= mean(rank), by = bookmaker][1:length(bookies),c(2,4)]
  ave_df <- ave_df[order(ave_rank)]

  
  k <- length(bookies)
  
  #r.stat <- qtukey(0.01,k,k-1) * sqrt((k*(k+1))/(12*n))
  
  rstat <- 3.853 * sqrt((k*(k-1))/(6*n))
  
  return(list(ave_rank = ave_df, CD = rstat ))
}