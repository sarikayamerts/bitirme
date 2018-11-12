plot_ly(data = first[season == '2016-2017'], 
        y = ~Shin_RPS, 
        color = ~bookmaker, 
        type = "box") %>%
  layout(title = "Boxplot of Bookmakers' RPS in 2016-2017 Season")

plot_ly(data = first, x = ~bookmaker, y = ~Shin_RPS, color = ~season, type = "box") %>%
  layout(boxmode = "group", 
         title = 'BoxPlot of Shin RPS (First Data)')

plot_ly(data = first, x = ~season, y = ~Shin_RPS, color = ~bookmaker, type = "box") %>%
  layout(boxmode = "group")

plot_ly(data = last, y = ~bookmaker, x = ~Shin_RPS, color = ~season, type = "box") %>%
  layout(boxmode = "group", 
         xaxis = list(range = c(0, 0.28)),
         title = 'RPS Values of Bookmakes, by Season')
  
plot_ly(x = average$bookmaker,y = average$First_Basic, name = "Basic",  type = "bar") %>%
  #add_trace(y = average$First_Shin, name = 'First Shin') %>%
  #add_trace(y = average$Last_Basic, name = 'Last Basic') %>%
  add_trace(y = average$First_Shin, name = 'Shin') %>%
  layout(yaxis = list(title = 'RPS', range = c(0.186, 0.196)), barmode = 'group')

plot_ly(x = average[bookmaker == '888sport']$season,y = average[bookmaker == '888sport']$First_Basic, name = "Basic",  type = "bar")

plot_ly(x = average$season,y = average$First_Shin, color = average$bookmaker, type = "bar") %>%
  layout(yaxis = list(title = 'RPS', range = c(0.1, 0.23)), barmode = 'group')

plot_ly(x = booksum_df$season, y = booksum_df$booksum.odd1, color = booksum_df$bookmaker, type = "bar") %>%
  layout(yaxis = list(title = 'Booksum', range = c(1, 1.2)), barmode = 'group')



var(first[bookmaker == '1xBet']$Shin_RPS)
mean(first[bookmaker == '1xBet']$Shin_RPS)
mean(first[bookmaker == '1xBet']$Shin_RPS) + var(first[bookmaker == '1xBet']$Shin_RPS)
quantile(first[bookmaker == '1xBet']$Shin_RPS)


average_withmedian <- last[, .(var = mean(Basic_RPS, na.rm = TRUE)), by = bookmaker]
average_withmedian <- merge(average_withmedian, last[, .(var = mean(Shin_RPS, na.rm = TRUE)), by = bookmaker], by = "bookmaker")
average_withmedian <- merge(average_withmedian, last[, .(var = median(Basic_RPS, na.rm = TRUE)), by = bookmaker], by = "bookmaker")
average_withmedian <- merge(average_withmedian, last[, .(var = median(Shin_RPS, na.rm = TRUE)), by = bookmaker], by = "bookmaker")
colnames(average_withmedian) <- c("bookmaker","basicmean", "shinmean", "basicmedian", "shinmedian")
average_withmedian$meanchange <- round(100*((average_withmedian$basicmean - average_withmedian$shinmean) / average_withmedian$basicmean),3)
average_withmedian$medianchange <- round(100*((average_withmedian$basicmedian - average_withmedian$shinmedian) / average_withmedian$basicmedian),3)
colnames(average_withmedian) <- c("bookmaker","Mean of Basic Norm.", "Mean of Shin Norm.", "Median of Basic Norm.", "Median of Shin Norm.", "Change in Mean", "Change in Median")
average_withmedian <- average_withmedian[-c(22,25)]

shin_basic <- average[,c(1,4,5)]
shin_basic$percentage <- ((shin_basic$Last_Basic - shin_basic$Last_Shin) / shin_basic$Last_Basic)
colnames(shin_basic) <- c('bookmaker', 'Basic Normalization', 'Shin Normalization', 'percentage')



plot_ly(x = changes$change, type = 'histogram')
plot_ly(x = changes$shin_prob.x, y = changes$shin_prob.y)  %>%
  layout(title = 'Changes In Implied Probabilities',
         yaxis = list(title = 'Last Probabilities'),
         xaxis = list(title = 'First Probabilities'))


library(RColorBrewer)
rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
r <- rf(32)
hist2d(changes[,c(4,5)], FUN=function(x) log(length(x)), nbins = 100, col = r)

h <- hexbin(changes[,c(4,5)])
plot(h)
plot(h, colramp=rf)
hexbinplot(shin_prob.y~shin_prob.x, data=changes, colramp=rf, trans=log, inv=exp)



p <- plot_ly(x = changes$shin_prob.x, y = changes$shin_prob.y)
subplot(
  p %>% add_markers(alpha = 0.2),
  p %>% add_histogram2d()
)


df_x = data.frame(x=0:1, y = 1:0)
plot_ly() %>%
  add_trace(data = prob_comparison, x = ~shin_prob.odd1, y = ~shin_prob.odd2) %>%
  add_trace(x = df_x$x, y = df_x$y, mode = "line", line  = list(color = 'rgb(205,12,24)', dash = 'dot')) %>%
  layout(yaxis = list(title = 'Away Probabilities', range = c(0, 1))) %>%
  layout(xaxis = list(title = 'Home Probabilities', range = c(0, 1)))


plot_ly() %>%
  add_trace(data = prob_comparison, x = ~shin_prob.odd1-shin_prob.odd2, y = ~shin_prob.oddX) %>%
  layout(yaxis = list(title = 'P(Draw)', range = c(0, 0.5))) %>%
  layout(xaxis = list(title = 'P(Home) - P(Away) ', range = c(-1, 1)))

#binning
df <- data.frame(x1= prob_comparison$shin_prob.odd1-prob_comparison$shin_prob.odd2, x2 = prob_comparison$shin_prob.oddX)
vec <- df$x1
nObs <- 300
datLabels <- ceiling(seq_along(vec)/nObs)[rank(vec, ties.method = "first")] 
bins <- split(vec, datLabels)



