
library(readstata13)
library(ggplot2)
library(fastmatch)
library(xtable)
library(Rfast)
library(matrixStats)
library(sqldf)
library(reshape2)
library(plyr)
library(tidyverse)
library(igraph)

# read agent data: exiters, treatment and control agents
setwd("/Users/terrywu/Dropbox/MLS Network/Exiter Analysis 2")
load("agent.Rdata")

# read probability of sales data
setwd("/Users/terrywu/Dropbox/MLS Network/Exiter Analysis 2")
trans<-read.csv("Prob_Sale.csv",stringsAsFactors = F)

setwd("/Users/terrywu/Dropbox/MLS Network/Exiter Analysis 2")
trans1<-read.csv("Prob_Sale_Noexiters.csv",stringsAsFactors = F)


# we create a treatment dataset
# such that it contains: agent, event time, year of transaction
treatment_trans<-data.frame(NA, NA, NA,NA)
colnames(treatment_trans)<-c("agent","year","num_trans","event_time")
# restrict to -3 to 3

# loop through all treatment year
for(i in 1:length(connect_agent))
{
  # extract the trans for these agents
  temp<-trans1[trans1$agent_id %in% connect_agent[[i]],]
  # add event time
  temp$event_time<-temp$year-(2000+i)
  # restrict to -3 to 3
  temp<-temp[temp$event_time %in% c(-3:3),]
  # change column names
  colnames(temp)<-c("agent","year","num_trans","event_time")
  # merge
  treatment_trans<-rbind(treatment_trans, temp)
  # print the process
  print(2000+i)
}
treatment_trans<-treatment_trans[-1,]
# add group name
treatment_trans$group<-"connected"

# we create a control dataset
# such that it contains: agent, event time, year of transaction
control_trans<-data.frame(NA, NA, NA,NA)
colnames(control_trans)<-c("agent","year","num_trans","event_time")

# loop through all control year
for(i in 1:length(notconnect_agent))
{
  # extract the trans for these agents
  temp<-trans[trans$Agent_ID %in% notconnect_agent[[i]],]
  # add event time
  temp$event_time<-temp$Year-(2000+i)
  # restrict to -3 to 3
  temp<-temp[temp$event_time %in% c(-3:3),]
  # change column names
  colnames(temp)<-c("agent","year","num_trans","event_time")
  # merge
  control_trans<-rbind(control_trans, temp)
  # print the process
  print(2000+i)
}
control_trans<-control_trans[-1,]
# add group name
control_trans$group<-"disconnected"

# a data frame to plot
df<-rbind(treatment_trans, control_trans)
# average by event time and group identity
df<-aggregate(df$num_trans~df$group+df$event_time, FUN=mean)
colnames(df)<-c("group","event_time","num")

# plot
ggplot(df, aes(x=event_time,y=num,group=group,color=group))+
  geom_line(size=2)+
  theme_bw()+
  ylab("Mean Transaction Counts by Event Time")+
  xlab("Event Time")+
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25))+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_discrete(limits=c(-3:3))+
  expand_limits(y = c(0, 1))+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=25))+
  theme(legend.position="bottom",
        legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))+
  geom_vline(xintercept = 0,size=2)

# the difference
d1<-df[df$group=="Control",]
d2<-df[df$group=="Treatment",]
# order by event time
d1<-d1[order(d1$event_time),]
d2<-d2[order(d2$event_time),]
# the difference
d1$num<-d2$num-d1$num
# plot
ggplot(d1, aes(x=event_time,y=num))+
  geom_line(size=2)+
  theme_bw()+
  ylab("Mean Transaction Difference by Event Time")+
  xlab("Event Time")+
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25))+
  theme(axis.text.x = element_text(angle = 90))+
  expand_limits(x = c(-3, 3))+
  scale_x_discrete(limits=c(-3:3))+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=25))+
  theme(legend.position="bottom",
        legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))+
  geom_vline(xintercept = 0,size=2)

