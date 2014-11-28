

delayedAssign('ss06hid', loadss06hid())

loadss06hid <- function () {
  read.csv("getdata-data-ss06hid.csv")
}              
              
q1<- function () {
#  ss06hid$ADJUST
  sum (ss06hid$VAL ==24, na.rm =TRUE) 
}

q2<- function () {
#  print ("Each tidy data table contains information about only one type of observation.")
  print ("Tidy data has one variable per column.")
}

library(xlsx)

q3<-function () {
  dat <- read.xlsx("getdata-data-DATA.gov_NGAP.xlsx", sheetIndex=1, rowIndex=18:23, colIndex=7:15)
  sum(dat$Zip*dat$Ext,na.rm=T) 
}

library(XML)
q4<-function () {
  doc <- xmlTreeParse("getdata-data-restaurants.xml",useInternal=TRUE)
  rootNode <- xmlRoot(doc)
  sum(xpathSApply(rootNode,"//zipcode",xmlValue)==21231)  
}

library(data.table)
q5<- function () {
  #download.file(url="https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv", "dest.csv" ) 
  DT <- fread("dest.csv")
#NOT  tapply(DT$pwgtp15,DT$SEX,mean)

  system.time(mean(DT$pwgtp15,by=DT$SEX))
system.time(mean(DT[DT$SEX==1,]$pwgtp15))+ system.time(mean(DT[DT$SEX==2,]$pwgtp15))
system.time(DT[,mean(pwgtp15),by=SEX])
system.time(tapply(DT$pwgtp15,DT$SEX,mean))
#system.time(rowMeans(DT)[DT$SEX==1])+system.time( rowMeans(DT)[DT$SEX==2])
system.time(sapply(split(DT$pwgtp15,DT$SEX),mean))
}