
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
library(plm)


## First Version: Oct 20, 2019
## This Version: Oct 20, 2019

# 1. Plot the regressions
# read data frames
setwd("/Users/terrywu/Dropbox/MLS Network/Exiter Analysis 2")
df1<-read.csv("df1.csv",stringsAsFactors = F)
df2<-read.csv("df2.csv",stringsAsFactors = F)
df3<-read.csv("df3.csv",stringsAsFactors = F)

# restrict to -3 to 3
df1<-df1[df1$event_time_year %in% -3:3,]
df2<-df2[df2$event_time_year %in% -3:3,]
df3<-df3[df3$event_time_year %in% -3:3,]

# add group dummy
df1$groupdummy<-0
df1$groupdummy[df1$group=="connected"]<-1
df2$groupdummy<-0
df2$groupdummy[df2$group=="connected"]<-1
df3$groupdummy<-0
df3$groupdummy[df3$group=="connected"]<-1

# fixed effect of year regression
model.a <- lm(num_trans ~ event_time_year+groupdummy+groupdummy:event_time_year +factor(trans_year), data = df1)
model.b <- lm(prob ~ event_time_year+groupdummy+groupdummy:event_time_year +factor(prob_year), data = df2)
model.c <- lm(prob ~ event_time_year+groupdummy+groupdummy:event_time_year +factor(prob_year), data = df3)

# add in fitted values
df1$fitted<-model.a$fitted.values
df2$fitted<-model.b$fitted.values
df3$fitted<-model.c$fitted.values

# average
A<-aggregate(df1$fitted~df1$group+df1$event_time_year, FUN=mean)
colnames(A)<-c("group","event_time","num")
B<-aggregate(df2$fitted~df2$group+df2$event_time_year, FUN=mean)
colnames(B)<-c("group","event_time","num")
C<-aggregate(df3$fitted~df3$group+df3$event_time_year, FUN=mean)
colnames(C)<-c("group","event_time","num")

# plot
ggplot(A, aes(x=event_time,y=num,group=group,color=group))+
  geom_line(size=2)+
  theme_bw()+
  ylab("Mean Transaction by Event Time")+
  xlab("Event Time")+
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25))+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_discrete(limits=c(-3:3))+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=25))+
  theme(legend.position="bottom",
        legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))+
  geom_vline(xintercept = 0,size=2)

ggplot(B, aes(x=event_time,y=num,group=group,color=group))+
  geom_line(size=2)+
  theme_bw()+
  ylab("Mean Prob of Sales by Event Time")+
  xlab("Event Time")+
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25))+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_discrete(limits=c(-3:3))+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=25))+
  theme(legend.position="bottom",
        legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))+
  geom_vline(xintercept = 0,size=2)

ggplot(C, aes(x=event_time,y=num,group=group,color=group))+
  geom_line(size=2)+
  theme_bw()+
  ylab("Mean Prob of Sales by Event Time")+
  xlab("Event Time")+
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25))+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_discrete(limits=c(-3:3))+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=25))+
  theme(legend.position="bottom",
        legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))+
  geom_vline(xintercept = 0,size=2)


# add difference graphs
d1<-A[A$group=="connected",]
d2<-A[A$group=="disconnected",]
# order by event time
d1<-d1[order(d1$event_time),]
d2<-d2[order(d2$event_time),]
# the difference
d1$num<-d1$num-d2$num
# plot
ggplot(d1, aes(x=event_time,y=num))+
  geom_line(size=2)+
  theme_bw()+
  ylab("Mean Transaction Difference by Event Time")+
  xlab("Event Time")+
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25))+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_discrete(limits=c(-3:3))+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=25))+
  theme(legend.position="bottom",
        legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))+
  geom_vline(xintercept = 0,size=2)


d1<-B[B$group=="connected",]
d2<-B[B$group=="disconnected",]
# order by event time
d1<-d1[order(d1$event_time),]
d2<-d2[order(d2$event_time),]
# the difference
d1$num<-d1$num-d2$num
# plot
ggplot(d1, aes(x=event_time,y=num))+
  geom_line(size=2)+
  theme_bw()+
  ylab("Mean Prob of Sales Difference by Event Time")+
  xlab("Event Time")+
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25))+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_discrete(limits=c(-3:3))+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=25))+
  theme(legend.position="bottom",
        legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))+
  geom_vline(xintercept = 0,size=2)

d1<-C[C$group=="connected",]
d2<-C[C$group=="disconnected",]
# order by event time
d1<-d1[order(d1$event_time),]
d2<-d2[order(d2$event_time),]
# the difference
d1$num<-d1$num-d2$num
# plot
ggplot(d1, aes(x=event_time,y=num))+
  geom_line(size=2)+
  theme_bw()+
  ylab("Mean Prob of Sales Difference by Event Time")+
  xlab("Event Time")+
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25))+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_discrete(limits=c(-3:3))+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=25))+
  theme(legend.position="bottom",
        legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))+
  geom_vline(xintercept = 0,size=2)


## 2. in the following, we remove all relavant outcomes w.r.t. to exiters

# remove the environment
rm(list=ls())

# read transaction data
setwd("/Users/terrywu/Dropbox/MLS Network/Superstar Task")
trans<-read.csv("Transaction_by_Agent_Year.csv",stringsAsFactors = F)

# exclude year 2013 and after
trans<-trans[trans$Year<=2012,]

# augment it to create a balanced dataset
df_b<-data.frame(Year = rep(unique(trans$Year), length(unique(trans$Agent_ID))),
                 Agent_ID = rep(unique(trans$Agent_ID), each = length(unique(trans$Year))))
trans<-left_join(df_b, trans)
# NA to 0
trans[is.na(trans$Num_Transactions),3]<-0

# calculate active dataset based on trans
active<-acast(trans, Agent_ID~Year, value.var="Num_Transactions",fun.aggregate=sum)
active[is.na(active)]<-0

# year span
year<-unique(trans$Year)
year<-year[order(year)]

# using 2-year def to create a list of exiting agents
exit<-list()
for(i in 1:(length(year)-2))
{
  temp<-active[active[,i+1]==0,]
  temp<-temp[temp[,i+2]==0,]
  temp<-temp[temp[,i]!=0,]
  exit[[i]]<-rownames(temp)
  print(i)
}

# calcualte the treatment agents
# suppose i exits in year t+1, and calculate the agents who connects her in year t
# and those who did not in year t

# calcualte the treatment agents
# suppose i exits in year t+1, and calculate the agents who connects her in year t
# and those who did not in year t

# read the original dataset
setwd("/Users/terrywu/Dropbox/MLS Network")
load("Michigan_Full.RData")

# removed all "999999"
X<-X[X$lagent_id!="999999",]
X<-X[X$bagent_id!="999999",]

# restricting to all pre-2013
X<-X[X$close_year<=2012,]
X<-X[X$list_year<=2012,]

# for each year, we first compute the agents who connect to exiters
# s.t. the treatment groups are who connected to exiters (t) in time t-1
connect_agent<-list()
# loop through
for(i in 1:(length(exit)-1))
{
  # for exiters acting as listers
  temp1<-X[X$close_year==year[i],] 
  temp1<-temp1[temp1$lagent_id %in% exit[[i+1]],]
  # take the bagent out
  b<-temp1$bagent_id
  
  # for exiters acting as buyers
  temp1<-X[X$list_year==year[i],] 
  temp1<-temp1[temp1$bagent_id %in% exit[[i+1]],]
  # take the lagent out
  b<-c(b,temp1$lagent_id)
  
  # include only unique agent
  b<-b[b!=""]
  b<-unique(b)
  
  # also we excude the exiters themselves
  b<-b[!(b %in% exit[[i+1]])]
  connect_agent[[i]]<-b
  
  # print the process
  print(i)
}

# for each year, calculate the agents who do not connect to exiters
notconnect_agent<-list()
# loop through year
for(i in 1:(length(exit)-1))
{
  # extracting all agents
  A<-active[active[,i]>=1,]
  A<-row.names(A)
  A<-unique(A)
  # calculating the agents who do not connect
  B<-setdiff(A, connect_agent[[i]])
  
  # exclude the exiters
  B<-B[!(B %in% exit[[i+1]])]
  
  # save
  notconnect_agent[[i]]<-B
  # print the process
  print(i)
}

# calculate the transaction of not-connected group by year
notconnected_trans<-data.frame(NA, NA, NA, NA)
colnames(notconnected_trans)<-c("agent_id","event_year","trans_year","trans")

# loop through year
for(i in 1:length(notconnect_agent))
{
# the agent
agent_id<-notconnect_agent[[i]]
# transactions counts
active_temp<-active[rownames(active) %in% agent_id,]
# add agent name
active_temp<-as.data.frame(active_temp)
active_temp$agent_id<-agent_id
# wide to long
a<-reshape(active_temp, direction = "long", varying = list(names(active_temp)[1:12]), v.names = "Value", 
        idvar = c("agent_id"), timevar = "Year", times = 2001:2012)
colnames(a)<-c("agent_id","trans_year","trans")
# add event year
a$event_year<-2001+i
# merge
notconnected_trans<-rbind(notconnected_trans,a)
# print process
print(i)
}
notconnected_trans<-notconnected_trans[-1,]
rownames(notconnected_trans)<-1:length(notconnected_trans$agent_id)

# then do a similar dataset for connected group

# set the working directory and read data
setwd("/Users/terrywu/Dropbox/MLS Network")
load("Michigan_Full.RData")

# remove "999999"
X<-X[X$bagent_id!="999999",]
X<-X[X$lagent_id!="999999",]

# remove post-2013
X<-X[X$list_year<=2012,]
X<-X[X$close_year<=2012,]

# we first calculate the transactions per agent-year

# year span
year<-c(X$list_year, X$close_year)
year<-unique(year)
year<-year[!is.na(year)]
year<-year[order(year)]

# create a data frame such that the first column is agent id
# the second column is year
# the third column is num of transactions
connected_trans<-data.frame(NA, NA, NA,NA)
colnames(connected_trans)<-colnames(notconnected_trans)

# add a number for calculating
X$Num<-1

# year span
year<-2002:2010

# loop through all connected agents
for(i in 1:length(year))
{
# as a listing agent
temp1<-X[X$lagent_id %in% connect_agent[[i]],]
# exluding exiters in that year
temp1<-temp1[!(temp1$bagent_id %in% exit[[i+1]]),]
# aggregate by year
A<-aggregate(temp1$Num~temp1$list_year+temp1$lagent_id, FUN=sum)
# change name
colnames(A)<-c("trans_year","agent_id","trans")
# record
B<-A

# as a buying agent
temp1<-X[X$bagent_id %in% connect_agent[[i]],]
# exluding exiters in that year
temp1<-temp1[!(temp1$lagent_id %in% exit[[i+1]]),]
# aggregate by year
A<-aggregate(temp1$Num~temp1$list_year+temp1$bagent_id, FUN=sum)
# change name
colnames(A)<-c("trans_year","agent_id","trans")

# merge
B<-rbind(A, B)

# aggregate
B<-aggregate(B$trans~B$trans_year+B$agent_id, FUN=sum)
colnames(B)<-c("trans_year","agent_id","trans")

# add event year
B$event_year<-year[i]

# add to main dataset
connected_trans<-rbind(connected_trans,B)

# print process
print(year[i])
}
connected_trans<-connected_trans[-1,]

# event time
connected_trans$event_time_year<-connected_trans$trans_year-connected_trans$event_year
notconnected_trans$event_time_year<-notconnected_trans$trans_year-notconnected_trans$event_year

# restrict to -3 to 3
connected_trans<-connected_trans[connected_trans$event_time_year %in% -3:3,]
notconnected_trans<-notconnected_trans[notconnected_trans$event_time_year %in% -3:3,]

# merge these two datasets
connected_trans$group<-"connected"
notconnected_trans$group<-"disconnected"
mydf<-rbind(notconnected_trans,connected_trans)

# average 
my11<-aggregate(mydf$trans~mydf$event_time_year+mydf$group, FUN=mean)
colnames(my11)<-c("event_time","group","num")

# plot
ggplot(my11, aes(x=event_time,y=num,group=group,color=group))+
  geom_line(size=2)+
  theme_bw()+
  ylab("Mean Transaction Counts by Event Time")+
  xlab("Event Time")+
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25))+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_discrete(limits=c(-3:3))+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=25))+
  theme(legend.position="bottom",
        legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))+
  geom_vline(xintercept = 0,size=2)

# and a difference graph
d1<-my11[my11$group=="connected",]
d2<-my11[my11$group=="disconnected",]
# order by event time
d1<-d1[order(d1$event_time),]
d2<-d2[order(d2$event_time),]
# the difference
d1$num<-d1$num-d2$num
# plot
ggplot(d1, aes(x=event_time,y=num))+
  geom_line(size=2)+
  theme_bw()+
  ylab("Mean Transaction Difference by Event Time")+
  xlab("Event Time")+
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25))+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_discrete(limits=c(-3:3))+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=25))+
  theme(legend.position="bottom",
        legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))+
  geom_vline(xintercept = 0,size=2)


# then probability of sales
prob_sale<-data.frame(NA, NA, NA)
colnames(prob_sale)<-c("agent_id","year","prob")

prob_sale2<-data.frame(NA, NA, NA)
colnames(prob_sale2)<-c("agent_id","year","prob")

# the first probability set is for all listings
# the second remove listings with exiters
Y<-X
# non-missing list year
Y<-Y[!is.na(Y$list_year),]

# list year span
year<-unique(Y$list_year)
year<-year[order(year)]
year<-year[year<=2010]

# loop through each listing year
for(i in 1:length(year))
{
# listing in that year
temp1<-Y[Y$list_year==year[i],]
# if close price is non-missing, then it was sold
temp1<-data.frame(temp1$lagent_id, temp1$close_price)
# for non-missing price, assign 1 and 0 o.w.
temp1$temp1.close_price[!is.na(temp1$temp1.close_price)]<-1
temp1$temp1.close_price[is.na(temp1$temp1.close_price)]<-0
# the probs
temp1<-aggregate(temp1$temp1.close_price~temp1$temp1.lagent_id, FUN=mean)
colnames(temp1)<-c("agent_id","prob")
temp1$year<-year[i]
# merge
prob_sale<-rbind(prob_sale, temp1)

# then we exlude those listings with exiters

# listing in that year
temp1<-Y[Y$list_year==year[i],]
# excluding exiters
temp1<-temp1[!(temp1$bagent_id %in% exit[[i]]),]
# if close price is non-missing, then it was sold
temp1<-data.frame(temp1$lagent_id, temp1$close_price)
# for non-missing price, assign 1 and 0 o.w.
temp1$temp1.close_price[!is.na(temp1$temp1.close_price)]<-1
temp1$temp1.close_price[is.na(temp1$temp1.close_price)]<-0
# the probs
temp1<-aggregate(temp1$temp1.close_price~temp1$temp1.lagent_id, FUN=mean)
colnames(temp1)<-c("agent_id","prob")
temp1$year<-year[i]
# merge
prob_sale2<-rbind(prob_sale2, temp1)
# print the progress
print(i)
}
prob_sale<-prob_sale[-1,]
prob_sale2<-prob_sale2[-1,]

# we create a connected dataset
# such that it contains: agent, event time, year of transaction
treatment_trans<-data.frame(NA, NA, NA,NA)
colnames(treatment_trans)<-c("agent_id","year","num_trans","event_time")
# restrict to -3 to 3

# loop through all treatment year
for(i in 1:length(connect_agent))
{
# extract the trans for these agents
temp<-prob_sale2[prob_sale2$agent_id %in% connect_agent[[i]],]
# add event time
temp$event_time<-temp$year-(2001+i)
# restrict to -3 to 3
temp<-temp[temp$event_time %in% c(-3:3),]
# change column names
colnames(temp)<-c("agent_id","year","num_trans","event_time")
# merge
treatment_trans<-rbind(treatment_trans, temp)
# print the process
print(2001+i)
}
treatment_trans<-treatment_trans[-1,]
# add group name
treatment_trans$group<-"connected"
# to -3 to 3

# we create a disconnected dataset
# such that it contains: agent, event time, year of transaction
control_trans<-data.frame(NA, NA, NA,NA)
colnames(control_trans)<-c("agent_id","year","num_trans","event_time")
# restrict to -3 to 3

# loop through all year
for(i in 1:length(notconnect_agent))
{
  # extract the trans for these agents
  temp<-prob_sale[prob_sale$agent_id %in% notconnect_agent[[i]],]
  # add event time
  temp$event_time<-temp$year-(2001+i)
  # restrict to -3 to 3
  temp<-temp[temp$event_time %in% c(-3:3),]
  # change column names
  colnames(temp)<-c("agent_id","year","num_trans","event_time")
  # merge
  control_trans<-rbind(control_trans, temp)
  # print the process
  print(2001+i)
}
control_trans<-control_trans[-1,]
# add group name
control_trans$group<-"disconnected"

# merge two datasets
mydf<-rbind(treatment_trans, control_trans)

# mean by group
A<-aggregate(mydf$num_trans~mydf$event_time+mydf$group, FUN=mean)
colnames(A)<-c("event_time","group","prob")
# plot
ggplot(A, aes(x=event_time,y=prob,group=group,color=group))+
  geom_line(size=2)+
  theme_bw()+
  ylab("Mean Prob of Sales by Event Time")+
  xlab("Event Time")+
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25))+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_discrete(limits=c(-3:3))+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=25))+
  theme(legend.position="bottom",
        legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))+
  geom_vline(xintercept = 0,size=2)


# and a difference graph
d1<-A[A$group=="connected",]
d2<-A[A$group=="disconnected",]
# order by event time
d1<-d1[order(d1$event_time),]
d2<-d2[order(d2$event_time),]
# the difference
d1$num<-d1$num-d2$num
# plot
ggplot(d1, aes(x=event_time,y=prob))+
  geom_line(size=2)+
  theme_bw()+
  ylab("Mean Prob of Sales Difference by Event Time")+
  xlab("Event Time")+
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25))+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_discrete(limits=c(-3:3))+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=25))+
  theme(legend.position="bottom",
        legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))+
  geom_vline(xintercept = 0,size=2)


# then probability of sales within 1% price
prob_sale<-data.frame(NA, NA, NA)
colnames(prob_sale)<-c("agent_id","year","prob")

prob_sale2<-data.frame(NA, NA, NA)
colnames(prob_sale2)<-c("agent_id","year","prob")

# the first probability set is for all listings
# the second remove listings with exiters
Y<-X
# non-missing list year
Y<-Y[!is.na(Y$list_year),]

# create a price dummy, s.t. if close price is within 1% of list price
Y$price_dummy<-abs(Y$close_price-Y$list_price)
Y$price_dummy<-Y$price_dummy/Y$list_price
Y$price_dummy<-Y$price_dummy<=0.01
Y$price_dummy[Y$price_dummy]<-1
Y$price_dummy[!Y$price_dummy]<-0

# list year span
year<-unique(Y$list_year)
year<-year[order(year)]
year<-year[year<=2010]

# loop through each listing year
for(i in 1:length(year))
{
  # list in that year
  temp1<-Y[Y$list_year==year[i],]
  # remove missing listing agent
  temp1<-temp1[!is.na(temp1$lagent_id),]
  # remove missing listing price
  temp1<-temp1[!is.na(temp1$list_price),]
  # if close price is non-missing, then it was sold
  temp1<-data.frame(temp1$lagent_id, temp1$price_dummy)
  # for missing price dummy, assign 0
  temp1$temp1.price_dummy[is.na(temp1$temp1.price_dummy)]<-0
  # the probs
  temp1<-aggregate(temp1$temp1.price_dummy~temp1$temp1.lagent_id, FUN=mean)
  colnames(temp1)<-c("agent_id","prob")
  temp1$year<-year[i]
  # merge
  prob_sale<-rbind(prob_sale, temp1)
  
  # then we exlude those listings with exiters
  
  # list in that year
  temp1<-Y[Y$list_year==year[i],]
  # remove missing listing agent
  temp1<-temp1[!is.na(temp1$lagent_id),]
  # remove missing listing price
  temp1<-temp1[!is.na(temp1$list_price),]
  # remove exiters as bagent
  temp1<-temp1[!(temp1$bagent_id %in% exit[[i]]),]
  # if close price is non-missing, then it was sold
  temp1<-data.frame(temp1$lagent_id, temp1$price_dummy)
  # for missing price dummy, assign 0
  temp1$temp1.price_dummy[is.na(temp1$temp1.price_dummy)]<-0
  # the probs
  temp1<-aggregate(temp1$temp1.price_dummy~temp1$temp1.lagent_id, FUN=mean)
  colnames(temp1)<-c("agent_id","prob")
  temp1$year<-year[i]
  # merge
  prob_sale2<-rbind(prob_sale2, temp1)
  # print the progress
  print(i)
}
prob_sale<-prob_sale[-1,]
prob_sale2<-prob_sale2[-1,]

# we create a connected dataset
# such that it contains: agent, event time, year of transaction
treatment_trans<-data.frame(NA, NA, NA,NA)
colnames(treatment_trans)<-c("agent_id","year","num_trans","event_time")
# restrict to -3 to 3

# loop through all treatment year
for(i in 1:length(connect_agent))
{
  # extract the trans for these agents
  temp<-prob_sale2[prob_sale2$agent_id %in% connect_agent[[i]],]
  # add event time
  temp$event_time<-temp$year-(2001+i)
  # restrict to -3 to 3
  temp<-temp[temp$event_time %in% c(-3:3),]
  # change column names
  colnames(temp)<-c("agent_id","year","num_trans","event_time")
  # merge
  treatment_trans<-rbind(treatment_trans, temp)
  # print the process
  print(2001+i)
}
treatment_trans<-treatment_trans[-1,]
# add group name
treatment_trans$group<-"connected"
# to -3 to 3

# we create a disconnected dataset
# such that it contains: agent, event time, year of transaction
control_trans<-data.frame(NA, NA, NA,NA)
colnames(control_trans)<-c("agent_id","year","num_trans","event_time")
# restrict to -3 to 3

# loop through all year
for(i in 1:length(notconnect_agent))
{
  # extract the trans for these agents
  temp<-prob_sale[prob_sale$agent_id %in% notconnect_agent[[i]],]
  # add event time
  temp$event_time<-temp$year-(2001+i)
  # restrict to -3 to 3
  temp<-temp[temp$event_time %in% c(-3:3),]
  # change column names
  colnames(temp)<-c("agent_id","year","num_trans","event_time")
  # merge
  control_trans<-rbind(control_trans, temp)
  # print the process
  print(2001+i)
}
control_trans<-control_trans[-1,]
# add group name
control_trans$group<-"disconnected"

# merge two datasets
mydf<-rbind(treatment_trans, control_trans)

# mean by group
A<-aggregate(mydf$num_trans~mydf$event_time+mydf$group, FUN=mean)
colnames(A)<-c("event_time","group","prob")
# plot
ggplot(A, aes(x=event_time,y=prob,group=group,color=group))+
  geom_line(size=2)+
  theme_bw()+
  ylab("Mean Prob of Sales by Event Time")+
  xlab("Event Time")+
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25))+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_discrete(limits=c(-3:3))+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=25))+
  theme(legend.position="bottom",
        legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))+
  geom_vline(xintercept = 0,size=2)


# and a difference graph
d1<-A[A$group=="connected",]
d2<-A[A$group=="disconnected",]
# order by event time
d1<-d1[order(d1$event_time),]
d2<-d2[order(d2$event_time),]
# the difference
d1$num<-d1$num-d2$num
# plot
ggplot(d1, aes(x=event_time,y=prob))+
  geom_line(size=2)+
  theme_bw()+
  ylab("Mean Prob of Sales Difference by Event Time")+
  xlab("Event Time")+
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25))+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_discrete(limits=c(-3:3))+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=25))+
  theme(legend.position="bottom",
        legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))+
  geom_vline(xintercept = 0,size=2)