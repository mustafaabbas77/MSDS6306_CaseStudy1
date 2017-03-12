#Merging the two datasets
Comb_data<-merge(Clean_GDP,econ,by="CountryCode")
#Making Rank and USD numeric
Comb_data$Rank<-as.numeric(as.character(Comb_data$Rank))
Comb_data$USD<-as.numeric(gsub(",","",Comb_data$USD))

#Q1
summary(Clean_GDP)
summary(Comb_data)
#189 of 190 IDs match
