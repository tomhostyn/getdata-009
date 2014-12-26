library(dplyr)
library(lubridate)


q1 <- function () {
  ss06hid <- read.csv("getdata-data-ss06hid.csv")
  strsplit (names(ss06hid), "wgtp")[123]
}

q2 <- function () {
  GDP <- read.csv("getdata-data-GDP.csv", skip=4, nrows=190, stringsAsFactors=FALSE)
  mean(as.numeric (sapply(GDP$X.4, function (s) {gsub (",", "", s)})))
}

q3 <- function () {
  GDP <- read.csv("getdata-data-GDP.csv", skip=4, nrows=190)
  countryNames <-GDP$X.3
  countryNames[grep("^United",countryNames)]
}

q4 <- function () {
  GDP <- read.csv("getdata-data-GDP.csv", skip=4, nrows=190)
  EDSTATS <- read.csv("getdata-data-EDSTATS_Country.csv")
  join <- left_join(GDP, EDSTATS, by = c("X" = "CountryCode"))
  length(join$Special.Notes[grep ("Fiscal year end: June", join$Special.Notes)])  
}

q5 <- function () {
  amzn = getSymbols("AMZN",auto.assign=FALSE)
  sampleTimes = index(amzn) 
  sum(year(sampleTimes) == 2012)
}