rm(list = ls())
options(warn=-1)
library(readr)
library(graphics)
library(data.table)
library(verification)
library(nnet)

#new matches file
matches <- read_rds("df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds")
details <- read_rds("df9b1196-e3cf-4cc7-9159-f236fe738215_odd_details.rds")

details <- data.table(details)[, c("matchId", "bookmaker", "betType", "oddtype", "odd"), with = FALSE]
matches <- data.table(matches)[, c("matchId", "score"), with = FALSE]
