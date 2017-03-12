#Cleaning the data, deleting unnecessary rows and columns
names(GDP) <- c('CountryCode','Rank','x','Country','USD') 

#Subsetting columns of our interest 
Clean_GDP_NA <- subset(GDP,select = c('CountryCode','Rank','Country','USD'))

#Creating a function to check for NA is rows
row.has.na <- apply(Clean_GDP_NA, 1, function(x){any(is.na(x))})

#Removing the rows that are NULL and getitng a clean set of data 
Clean_GDP <- Clean_GDP_NA[!row.has.na,]

#Count the number of missing values
NA_Count <- sum(is.na(Clean_GDP_NA$CountryCode)) 
NA_Count

#making Rank and GDP numeric
Clean_GDP$Rank<-as.numeric(as.character((Clean_GDP$Rank)))
Clean_GDP$USD<-as.numeric(gsub(",","",Clean_GDP$USD))


