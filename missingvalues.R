#Handle missing odds
#bookieMissingness = first[Match_Date >= trainStart,list(.N,percMiss=sum(is.na(odd))/.N),by=list(bookmaker,betType)]
#cat("Number of bookmakers with proportion of missings below",pMissThreshold,"since",as.character(trainStart),":",length(bookiesToKeep),"\n")

#nonmissingBookmakers_sinceTestStart = unique(details_melt[Match_Date >= testStart, list(.N,NA_SUM=sum(is.na(odd))),by=list(bookmaker,betType)][NA_SUM==0]$bookmaker)
#bookiesToKeep = intersect(bookiesToKeep,nonmissingBookmakers_sinceTestStart)
#cat("Number of bookmakers with no missings since testStart", as.character(testStart), ":", length(bookiesToKeep), "\n")

#details = dcast(feature_odd_details,matchId~oddtype+bookmaker,value.var = c("Odd_Open","Odd_Close"))
#columnsToKeep = grep(paste(bookiesToKeep,collapse="|"),names(details),value=T)
#details = details[,c('matchId',columnsToKeep),with=F]

ordering <- lastrps[matchId %in% unique(matches[season == '2017-2018']$matchId)][, .(var = mean(Shin_RPS, na.rm = TRUE)), by = c("bookmaker")]
ordering <- ordering[order(var)]
rm(ordering)

#bookiesToKeep = c("10Bet", "12BET", "188BET", "BetVictor", "Betclic", "Betsafe", "Betsson", "Betway", "Pinnacle", "SBOBET", "Unibet", "William Hill", "bet365", "bet-at-home", "bwin")
bookiesToKeep = c("1xBet", "youwin", "12BET", "bet365", "SBOBET", "10Bet", "Pinnacle", "BetVictor", "Betclic", "Betsafe", "Betsson")
subsetBookies <- function(arr, df){
  df <- df[bookmaker %in% arr]
  df
}

#subsetBookies(bookiesToKeep)
