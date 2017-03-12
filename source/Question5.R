#Q5
Comb_data$quantile<-with(Comb_data,cut(Rank,breaks=quantile(Rank, probs=seq(0,1,by=0.2),na.rm=TRUE),include.lowest=TRUE))
IncvsGDP<-Comb_data[(Comb_data$Income.Group=="Lower middle income") & (Comb_data$quantile=="[1,38.6]"),c("Rank","CountryCode","Income.Group","quantile")]
str(IncvsGDP)