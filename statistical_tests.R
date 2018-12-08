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

