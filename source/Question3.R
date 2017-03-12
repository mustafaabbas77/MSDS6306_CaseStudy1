#Q3
Hi_OECD<-subset(Comb_data,Income.Group=="High income: OECD",select=c(Rank,CountryCode,USD,Income.Group))
mean(Hi_OECD$Rank)

Hi_nonOECD<-subset(Comb_data,Income.Group=="High income: nonOECD",select=c(Rank,CountryCode,USD,Income.Group))
mean(Hi_nonOECD$Rank)