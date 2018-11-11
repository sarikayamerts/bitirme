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
