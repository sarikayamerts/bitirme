plot_ly(data = first, 
        y = ~Shin_RPS, 
        color = ~bookmaker, 
        type = "box") %>%
  layout(title = 'BoxPlot of Shin RPS (First Data)')

plot_ly(data = first, x = ~bookmaker, y = ~Shin_RPS, color = ~season, type = "box") %>%
  layout(boxmode = "group", 
         title = 'BoxPlot of Shin RPS (First Data)')

plot_ly(data = first, x = ~season, y = ~Shin_RPS, color = ~bookmaker, type = "box") %>%
  layout(boxmode = "group")

plot_ly(data = last, y = ~bookmaker, x = ~Shin_RPS, color = ~season, type = "box") %>%
  layout(boxmode = "group", 
         xaxis = list(range = c(0, 0.28)),
         title = 'RPS Values of Bookmakes, by Season')
  
plot_ly(x = average$bookmaker,y = average$First_Basic, name = "First_Basic",  type = "bar") %>%
  add_trace(y = average$First_Shin, name = 'First Shin') %>%
  add_trace(y = average$Last_Basic, name = 'Last Basic') %>%
  add_trace(y = average$Last_Shin, name = 'Last Shin') %>%
  layout(yaxis = list(title = 'RPS', range = c(0.188, 0.195)), barmode = 'group')

var(first[bookmaker == '1xBet']$Shin_RPS)
mean(first[bookmaker == '1xBet']$Shin_RPS)
mean(first[bookmaker == '1xBet']$Shin_RPS) + var(first[bookmaker == '1xBet']$Shin_RPS)
quantile(first[bookmaker == '1xBet']$Shin_RPS)


average_withmedian <- last[, .(var = mean(Basic_RPS, na.rm = TRUE)), by = bookmaker]
average_withmedian <- merge(average_withmedian, last[, .(var = median(Basic_RPS, na.rm = TRUE)), by = bookmaker], by = "bookmaker")
average_withmedian <- merge(average_withmedian, last[, .(var = mean(Shin_RPS, na.rm = TRUE)), by = bookmaker], by = "bookmaker")
average_withmedian <- merge(average_withmedian, last[, .(var = median(Shin_RPS, na.rm = TRUE)), by = bookmaker], by = "bookmaker")
colnames(average_withmedian) <- c("bookmaker","basicmean", "basicmedian", "shinmean", "shinmedian")
average_withmedian$meanchange <- round(100*((average_withmedian$basicmean - average_withmedian$shinmean) / average_withmedian$basicmean),3)
average_withmedian$medianchange <- round(100*((average_withmedian$basicmedian - average_withmedian$shinmedian) / average_withmedian$basicmedian),3)
colnames(average_withmedian) <- c("bookmaker","Mean of Basic Norm.", "Median of Basic Norm.", "Mean of Shin Norm.", "Median of Shin Norm.", "Change in Mean", "Change in Median")

shin_basic <- average[,c(1,4,5)]
shin_basic$percentage <- ((shin_basic$Last_Basic - shin_basic$Last_Shin) / shin_basic$Last_Basic)
colnames(shin_basic) <- c('bookmaker', 'Basic Normalization', 'Shin Normalization')



plot_ly(x = changes$change, type = 'histogram')
plot_ly(x = changes$shin_prob.x, y = changes$shin_prob.y)  %>%
  layout(title = 'Changes In Implied Probabilities',
         yaxis = list(title = 'Last Probabilities'),
         xaxis = list(title = 'First Probabilities'))



