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



todo

- scatterplot (x = home probabilities, y = away probabilities) DONE
- scatterplot (x = p(home) - p(away), y = p(draw)) DONE
- separate home goals and away goals, find the mu of poisson, visually inspect whether they fit or not (betamatics page 5) (mu is average goal number for each home and away team)


betamatics

In the left plot, the probability of draw P(draw) for a coordinate (P(home), P(away)) can be seen as the shortest distance between the diagonal line where P(home)+P(away) = 0. This can be derived from the sum of the three possible outcomes

Kelly Criterion: There is a well-known formula in the betting community known as the Kelly Criterion, which is an equation for computing the optimal bet to place in order to maximize the expected outcome, given odds and outcome probabilities as input.
(Portfolio sizing işine girdiğimizde bakmamız gereken yer bu, three outcome kelly)

In order to make a fit we need a model. In short, it should take the odds for home win, draw and away win, and output the three probabilities for each outcome. While we could simply feed the odds directly into the MNR, this would not be able to make good predictions since we do not expect the odds to be linearly dependent on the probability of the outcomes. We therefore should preprocess the odds in some way to get a model that is valid through dimensional analysis

To convert the odds to predictors we take the following steps:
  1. odds: The odds are taken directly from the source, see Section 3.5.
2. odds probability: The odds are converted to probabilities, by the formula assuming
that the bookmakers odds are based on a fair game. (See Section 2.1.)
3. predicting probabilities: In this step we choose which odds probabilities to use for the prediction. In addition to the three outcome probabilities, we found that adding P(favorite win) as a predicting variable improves the results significantly.
4. logit predictors: In the final step before the MNR we transform the probabilities by the logit function, defined as logit(p) = log(p/(1 − p)). (See motivation below.)


In a (binomial) logistic regression, we are essentially doing a linear fit of a predictor to the logit of the outcome probabilities. Since the predictors are also probabilities, it is reasonable to apply the logit function to them before we use them, giving us the following model in the binary case.
logit(p∗) = β0 + β1logit(p)
This model has the benefit that the probabilities are always in the range (0,1), and it thus weights the variable in a more sensible way than just using the probabilities. For example, input probabilities of 0 or 1 will map to ±∞ (in the linear model space), and the model will never be able to output unreasonable values such as −0.1 or 1.1. In the multinomial case the softmax function is used instead of the logistic function, and the equations are changed accordingly
The motivation for using P(favorite win) as predicting variable comes from that we, by inspection, think that the peak in the draw probability curve (Figure 6 (a)) is lower than it should be and that it not necessarily must be smooth for even matches. The favorite win probability, defined as
P(favorite win) = max(P(home win), P(away win))
introduces the non-linearity seen in Figure 6 (b), and allows the MNR fit to model a
non-smooth peak.