comparison <- function(output_df, rank = FALSE){
  matchId_compared <- output_df$matchId
  output_df <- merge(output_df, matches[matchId %in% matchId_compared][,c("matchId", "winner")], by = "matchId")
  output_df <- as.data.table(output_df)[, RPS := calculate_rps(odd1, oddX, odd2, winner), by = 1:nrow(output_df)]
  ourRPS <- mean(output_df$RPS)
  
  if (rank){
    testRPS <- lastrps[matchId %in% matchId_compared][, .(var = mean(Shin_RPS, na.rm = TRUE)), by = c("bookmaker")]
    testRPS <- testRPS[order(testRPS$var),]
    minRPS <- round(min(testRPS$var),7)
    maxRPS <- round(max(testRPS$var),7)
    x <- data.frame("***IE 492***", ourRPS)
    names(x) <- names(testRPS)
    testRPS <- rbind(testRPS, x)
    testRPS <- testRPS[order(testRPS$var),]
    print(testRPS)
  }
  return(output_df)
}
