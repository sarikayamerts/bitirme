rm(list = ls())
options(warn=-1)
library(readr)
library(graphics)
library(data.table)
library(verification)
library(nnet)

### mert's macbook database directory
setwd("/Users/mertsarikaya/Downloads/Bitirme/")
### mert's windows database directory
#setwd("")
### emre's database directory
#setwd("")

#new matches file
matches <- read_rds("df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds")
details <- read_rds("df9b1196-e3cf-4cc7-9159-f236fe738215_odd_details.rds")

details <- data.table(details)[, c("matchId", "bookmaker", "betType", "oddtype", "odd"), with = FALSE]
matches <- data.table(matches)[, c("matchId", "score"), with = FALSE]

df <- rbind(read_rds("england_premier_league_details.rds"), 
            read_rds("england_championship_details.rds"),
            read_rds("italy_serie_a_details.rds"),
            read_rds("spain_laliga_details.rds"),
            read_rds("turkey_super_lig_details.rds"))

df2 <- rbind(read_rds("england_premier_league_raw.rds"), 
             read_rds("england_championship_raw.rds"),
             read_rds("italy_serie-a_raw.rds"),
             read_rds("spain_laliga_raw.rds"),
             read_rds("turkey_super_lig_raw.rds"))
