
if (is.na(Sys.getenv("FOOBAR_CONSUMER_SECRET", unset = NA))) {
  warning("FOOBAR_CONSUMER_SECRET undefined, get it from github & define on console")
  # https://github.com/settings/applications#personal-access-tokens
  #  Sys.setenv(FOOBAR_CONSUMER_SECRET= "<key>")
}

library(httr)

q1 <- function () {
  
  # 1. Find OAuth settings for github:
  #    http://developer.github.com/v3/oauth/
  oauth_endpoints("github")
  
  # 2. Register an application at https://github.com/settings/applications;
  #    Use any URL you would like for the homepage URL (http://github.com is fine)
  #    and http://localhost:1410 as the callback url
  #
  #    Insert your client ID and secret below - if secret is omitted, it will
  #    look it up in the GITHUB_CONSUMER_SECRET environmental variable.
  myapp <- oauth_app("foobar", "01c1d12c9c76451f7937")
  
  # 3. Get OAuth credentials
  github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)
  
  # 4. Use API
  gtoken <- config(token = github_token)
  req <- GET("https://api.github.com/users/jtleek/repos", gtoken)
  stop_for_status(req)
  content(req)
  
  sapply (content(req), function (x){print (x$name) ; print (x$created_at)})
#   [1] "datasharing"
#   [1] "2013-11-07T13:25:07Z"
  
}

library(sqldf)
q2 <- function (){
  acs <- read.csv("getdata-data-ss06pid.csv")
  sqldf("select pwgtp1 from acs where AGEP < 50")
}


q3 <- function (){
  length (unique(acs$AGEP))
  length(t(sqldf("select distinct AGEP from acs")))
}


q4 <- function () {
  con = url("http://biostat.jhsph.edu/~jleek/contact.html")
  htmlCode = readLines(con)
  close(con)
  htmlCode
  nchar(htmlCode[10])
  nchar(htmlCode[20])
  nchar(htmlCode[30])
  nchar(htmlCode[100])
}


q5 <- function () {
  x <- read.fwf(
    file=url("https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for "),
    skip=4,
    widths=c(12, 7,4, 9,4, 9,4, 9,4))
  
  head(x)
  sum (x$V4)
}