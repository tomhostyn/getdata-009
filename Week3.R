library(dplyr)

q1 <- function () {
  ss06hid <- read.csv("getdata-data-ss06hid.csv")
  
  which (ss06hid$ACR ==3 & ss06hid$AGS >= 3)
}

library (jpeg)

q2 <- function () {
  jpeg <- readJPEG ("getdata-jeff.jpg", native=TRUE) 
  quantile(jpeg, c(0.30, 0.80))
}

q3 <- function () {
  GDP <- read.csv("getdata-data-GDP.csv", skip=4, nrows=190)
  EDSTATS <- read.csv("getdata-data-EDSTATS_Country.csv")
  
  GDP <- GDP [, c(1,2,4)]
  EDSTATS <- EDSTATS[,c(1,2)]
  
  join <- left_join(GDP, EDSTATS, by = c("X" = "CountryCode"))
 
  print(dim(join)[1])
  join[order(-join$X.1)[13],]
}

q4 <- function () {
  GDP <- read.csv("getdata-data-GDP.csv", skip=4, nrows=190)
  EDSTATS <- read.csv("getdata-data-EDSTATS_Country.csv")
 
  join <- left_join(GDP, EDSTATS, by = c("X" = "CountryCode"))
  mean(join [join$Income.Group=="High income: OECD", 2], na.rm=TRUE)
  warning ("wrong")
}


q5 <- function () {
  GDP <- read.csv("getdata-data-GDP.csv", skip=4, nrows=190)
  EDSTATS <- read.csv("getdata-data-EDSTATS_Country.csv")
  join <- left_join(GDP, EDSTATS, by = c("X" = "CountryCode"))
  quantile (join$X.1)
  sum(join$X.1 <= 38 & join$Income.Group=="Lower middle income")
}