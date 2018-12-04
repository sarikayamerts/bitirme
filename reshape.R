#widening first and last for feature extraction
widening <- function(df, arr){
  df_copy <- copy(df)
  df_copy <- subsetBookies(arr, df_copy)
  
  df_wide <- reshape(df_copy, idvar = c("matchId", "bookmaker"), timevar = c("oddtype"), direction = "wide")
  
  #features selection
  #df_wide[] #(0.1839656)
  #df_wide[, c("shin_prob.oddX","shin_prob.odd1","shin_prob.odd2") := NULL] #(0.1839904)
  #df_wide[, c("norm_prob.oddX","norm_prob.odd1","norm_prob.odd2") := NULL] #(0.1842094)
  #df_wide[, c("shin_prob.odd1","norm_prob.odd1") := NULL] #(0.1839544)
  df_wide[, c("shin_prob.oddX","norm_prob.oddX") := NULL] #(0.1825615)
  

  df_wide <- reshape(df_wide, idvar = c("matchId"), timevar = c("bookmaker"), direction = "wide")
  df_wide <- merge(df_wide, matches[, .(matchId, winner, date, week, season)], by = "matchId")
  df_wide
}