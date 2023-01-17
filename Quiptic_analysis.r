library(tidyr)
library(dplyr)
library(zoo)

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