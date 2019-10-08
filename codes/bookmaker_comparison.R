### Calculating average RPS's for each bookmakers (smaller values are better)
#average <- first[, .(var = mean(Basic_RPS, na.rm = TRUE)), by = bookmaker]
#average <- merge(average, first[, .(var = mean(Shin_RPS, na.rm = TRUE)), by = bookmaker], by = "bookmaker")
#average <- merge(average, last[, .(var = mean(Basic_RPS, na.rm = TRUE)), by = bookmaker], by = "bookmaker")
#average <- merge(average, last[, .(var = mean(Shin_RPS, na.rm = TRUE)), by = bookmaker], by = "bookmaker")
#colnames(average) <- c("bookmaker","First_Basic", "First_Shin", "Last_Basic", "Last_Shin")
# eskiden böyleydi season ekledik şimdi
average <- lastrps[, .(var = mean(Shin_RPS, na.rm = TRUE)), by = c("bookmaker")]
average_season <- lastrps[, .(var = mean(Shin_RPS, na.rm = TRUE)), by = c("bookmaker","season")]
average_week <- lastrps[, .(var = mean(Shin_RPS, na.rm = TRUE)), by = c("bookmaker", "week","season")]
#average <- merge(average, first[, .(var = mean(Shin_RPS, na.rm = TRUE)), by = c("bookmaker")], by = c("bookmaker"))
#average <- merge(average, lastrps[, .(var = mean(Basic_RPS, na.rm = TRUE)), by = c("bookmaker")], by = c("bookmaker"))
#average <- merge(average, lastrps[, .(var = mean(Shin_RPS, na.rm = TRUE)), by = c("bookmaker")], by = c("bookmaker"))
#colnames(average) <- c("bookmaker", "First_Basic", "First_Shin", "Last_Basic", "Last_Shin")
