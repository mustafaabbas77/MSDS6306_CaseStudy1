#Q4
Comb_data$CountryCode<-factor(Comb_data$CountryCode,levels=Comb_data[order(Comb_data$USD,decreasing=FALSE),"CountryCode"])
qplot(CountryCode,USD,data=Comb_data,fill=Income.Group,geom="col")+scale_y_log10(labels=c("$0","$10","$1K","$100K","$10M"))+labs(x="Country",y="GPD in USD (log10 scale)")
