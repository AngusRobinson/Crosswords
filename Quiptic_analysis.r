library(tidyr)
library(dplyr)
library(zoo)
library(ggplot2)

quiptics <- read.csv('~/Documents/Activity/Quiptic_tracker.csv')
View(quiptics)

#Convert times to minutes
quiptics$Mins_taken <- as.difftime(quiptics$T.5M,format="%H:%M:%S",units="mins")

#Add a rolling average column
quiptics$rollingavge <- rollmean(quiptics$Mins_taken, k=10,fill=NA, align='right')

#See average by setter
bysetter <- quiptics%>%
  group_by(Setter)%>%
  summarise_at(vars(Mistakes,Mins_taken), list(name=mean))

#Add a rolling average by setter column
quiptics$rollbysetter <- NA
for (x in bysetter$Setter) {
  quiptics[quiptics$Setter==x,]$rollbysetter <- rollmean(quiptics[quiptics$Setter==x,]$Mins_taken,k=5,fill=NA,align='right')}

#Plot over time
ggplot(data=quiptics)+
  geom_path(mapping=aes(x=Quiptic_number,y=rollbysetter,color=Setter))+
  geom_path(mapping=aes(x=Quiptic_number,y=rollingavge),color='blue')