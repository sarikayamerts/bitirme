multinomial_model <- function(data){
  df$winner_category <- sapply(as.character(df$winner), switch, "odd1" = 1, "oddX" = 2, "odd2" = 3, USE.NAMES = F)
  df <- subset(df, select = c('matchId', 'bookmaker', 'shin_prob.odd1', 'shin_prob.oddX', 'shin_prob.odd2', 'winner_category'))
  smp_size <- floor(0.70 * nrow(df))
  set.seed(123)
  train_ind <- sample(seq_len(nrow(df)), size = smp_size)
  train <- df[train_ind, ]
  test <- df[-train_ind, ]
  
  mult <- multinom(winner_category ~ shin_prob.odd1 + shin_prob.oddX + shin_prob.odd2 , data = train, model = TRUE)
  summary(mult)
  pred<-predict(mult,test)
  conf_matrix <<- table(test$winner_category, pred)

  pred_prob<-as.data.table(predict(mult,test, "prob"))
  pred_prob$winner <- sapply(as.character(test$winner_category), switch, "1" = "odd1", "2" = "oddX", "3" = "odd2", USE.NAMES = F)
  pred_prob$matchId <- test$matchId
  names(pred_prob) <- c("odd1", "oddX", "odd2", "winner", "matchId")
  predictions <<- pred_prob
  
  
}