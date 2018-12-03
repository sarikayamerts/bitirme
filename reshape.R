#widening first and last for feature extraction
widening <- function(df, arr){
  df_copy <- copy(df)
  df_copy <- subsetBookies(arr, df_copy)
  df_wide <- reshape(df_copy[,-"norm_prob"], idvar = c("matchId", "bookmaker"), timevar = c("oddtype"), direction = "wide")
  df_wide <- reshape(df_wide, idvar = c("matchId"), timevar = c("bookmaker"), direction = "wide")
  df_wide <- merge(df_wide, matches[, .(matchId, winner, date, week, season)], by = "matchId")
  df_wide
}