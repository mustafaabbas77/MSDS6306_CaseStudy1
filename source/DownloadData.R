
#download and read the data

GDP <- read.csv('https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv ' ,skip = 3 ,nrows=236,na.strings=c("","NA"))
econ <- read.csv('https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv',na.strings=c("","NA"))