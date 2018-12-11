### handle missing odds and subsetting data
source("missingvalues.R")

#widening dataframes for features releated to odds
widening <- function(df, arr){
  odds <- copy(df)
  odds <- subsetBookies(arr, odds)

  df_wide <- reshape(odds, idvar = c("matchId", "bookmaker"), timevar = c("oddtype"), direction = "wide")
  #df_wide[, c("shin_prob.oddX") := NULL] 
  df_wide <- reshape(df_wide, idvar = c("matchId"), timevar = c("bookmaker"), direction = "wide")
  df_wide
  
}

widening_diff <- 

#widening dataframes for features not releated to odds
widening_others <- function(df, arr){
  odds <- copy(df)
  odds <- subsetBookies(arr, odds)
  
  df_wide <- reshape(odds, idvar = c("matchId"), timevar = c("bookmaker"), direction = "wide")
  df_wide
  
}

widening_test <- function(df, arr){
  odds <- copy(df)
  odds <- subsetBookies(arr, odds)
  
  df_wide <- reshape(odds, idvar = c("matchId", "bookmaker"), timevar = c("oddtype"), direction = "wide")
  df_wide <- reshape(df_wide, idvar = c("matchId"), timevar = c("bookmaker"), direction = "wide")
  df_wide <- merge(df_wide, next_matches[, .(matchId, winner, date, week, season)], by = "matchId")
  df_wide
}

#features selection
#df_wide[] #(0.1839656)
#df_wide[, c("shin_prob.oddX","shin_prob.odd1","shin_prob.odd2") := NULL] #(0.1839904)
#df_wide[, c("norm_prob.oddX","norm_prob.odd1","norm_prob.odd2") := NULL] #(0.1842094)
#df_wide[, c("shin_prob.odd1","norm_prob.odd1") := NULL] #(0.1839544)
#df_wide[, c("shin_prob.oddX","norm_prob.oddX") := NULL] #(0.1825615)(bunlar changing odds eklenmeden)


