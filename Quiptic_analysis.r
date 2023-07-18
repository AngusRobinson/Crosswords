library(tidyr)
library(dplyr)
library(zoo)
library(ggplot2)
library(ggthemes)

quiptics <- read.csv('~/Documents/Crosswords/Quiptic_tracker.csv')
View(quiptics)

#Add penalty time column
quiptics$Penalty_time <- quiptics$Mistakes * 5


#Convert times to minutes
quiptics$Solve_time <- as.difftime(quiptics$Time_taken,format="%H:%M:%S",units="mins")
quiptics$Total_time <- as.difftime(quiptics$T.5M,format="%H:%M:%S",units="mins")

#Add a rolling average column
quiptics$rollingavge <- rollmean(quiptics$Total_time, k=10,fill=NA, align='right')

#See average by setter
bysetter <- quiptics%>%
  group_by(Setter)%>%
  summarise_at(vars(Mistakes,Solve_time,Penalty_time,Total_time), list(name=mean))

#Add a rolling average by setter column
quiptics$rollbysetter <- NA
for (x in bysetter$Setter) {
  quiptics[quiptics$Setter==x,]$rollbysetter <- rollmean(quiptics[quiptics$Setter==x,]$Total_time,k=5,fill=NA,align='right')}

#Plot over time
ggplot(data=quiptics)+
  geom_point(mapping=aes(x=Quiptic_number,y=Total_time),colour='lightblue')+
  geom_path(mapping=aes(x=Quiptic_number,y=rollingavge),colour='darkblue')+
  theme_solarized() +
  # Add a title and axis labels
  ggtitle("Quiptic Solve Time") +
  xlab("Quiptic Number") +
  ylab("Time Taken (minutes) + 5 mins per mistake")+
  annotate("text", x = min(quiptics$Quiptic_number), y = max(na.omit(quiptics$rollingavge))+5, label = "Rolling Average (last ten puzzles)", hjust = 0,colour='darkgrey')

#Plot by setter over time
ggplot(data=quiptics)+
  geom_path(mapping=aes(x=Quiptic_number,y=rollbysetter,color=Setter))+
  theme_solarized() +
  # Add a title and axis labels
  ggtitle("Quiptic Solve Time by Setter") +
  labs(subtitle="Rolling average (last 5 puzzles by that setter)")+
  xlab("Quiptic Number") +
  ylab("Time Taken (mins) + 5 per mistake")
  #annotate("text", x = min(quiptics$Quiptic_number), y = max(na.omit(quiptics$rollingavge))+5, label = "Rolling Average (last five puzzles)", hjust = 0)

#Plot setter difficulty
ggplot(data=bysetter)+
  geom_col(mapping=aes(x=Setter, y=Total_time_name, fill="Penalty Time"))+
  geom_col(mapping=aes(x=Setter, y=Solve_time_name, fill="Solve Time"), position = position_dodge())+
  theme_solarized() +
  # Add a title and axis labels
  ggtitle("Quiptic Solve Time by Setter") +
  xlab("Setter") +
  ylab("Total time (minutes)") +
  labs(subtitle="Average time solving + 5 min penalty per mistake/reveal")+
  scale_fill_manual(name="",
                    values=c( "Solve Time" = "lightblue","Penalty Time" = "pink"))


