a <- data.table(read_csv("summary2.csv"))
a <- tail(a, 78)
ourresult1 <- a[, .(var = mean(OurRPS, na.rm = TRUE)), by = c("Feature", "Weeks")]

b <- lastrps[matchId %in% matches[season == '2018-2019']$matchId]
b <- merge(b, matches[,c("matchId", "week")])
bookieResult <- b[, .(var = mean(Shin_RPS, na.rm = TRUE)), by = c("bookmaker", "week")]
