plot_ly(data = first, y = ~Shin_RPS, color = ~bookmaker, type = "box")

plot_ly(data = first, x = ~bookmaker, y = ~Shin_RPS, color = ~season, type = "box") %>%
  layout(boxmode = "group")

plot_ly(data = first, x = ~season, y = ~Shin_RPS, color = ~bookmaker, type = "box") %>%
  layout(boxmode = "group")

plot_ly(data = last, y = ~bookmaker, x = ~Shin_RPS, color = ~season, type = "box") %>%
  layout(boxmode = "group", xaxis = list(categoryorder = "category descending", domain = c(0, 1), range = c(0, 0.27)))
  
plot_ly(x = average$bookmaker,y = average$First_Basic, name = "First_Basic",  type = "bar") %>%
  add_trace(y = average$First_Shin, name = 'First Shin') %>%
  add_trace(y = average$Last_Basic, name = 'Last Basic') %>%
  add_trace(y = average$Last_Shin, name = 'Last Shin') %>%
  layout(yaxis = list(title = 'RPS', range = c(0.18, 0.20)), barmode = 'group')
  