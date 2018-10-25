
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

# match connected and diconnected group by zip code

## Oct 8, 2019

####### Create an agent-year-officer_zip data
# clear off
rm(list = ls())
# set the working directory and read dataset
setwd("/Users/terrywu/Dropbox/MLS Network")
load("Michigan_Full.RData")

# removed all "999999"
X<-X[X$lagent_id!="999999",]
X<-X[X$bagent_id!="999999",]

# excluse post-2013
X<-X[X$list_year<=2012,]
X<-X[X$close_year<=2012,]

# year span
year<-unique(c(X$list_year, X$close_year))
year<-year[order(year)]
year<-year[!is.na(year)]

# extracting the year with listing
temp<-data.frame(X$lagent_id, X$lagent_office_zip, X$list_year)
colnames(temp)<-c("agent","zipc","year")
temp<-unique(temp[ , 1:3 ] )
  
# extracting the year with buying
temp1<-data.frame(X$bagent_id, X$bagent_office_zip, X$close_year)
colnames(temp1)<-c("agent","zipc","year")
temp1<-unique(temp1[ , 1:3 ] )

# merge and clean
temp<-rbind(temp, temp1)
temp<-temp[!is.na(temp$agent),]
temp<-temp[!is.na(temp$year),]
temp<-temp[!is.na(temp$zipc),]
temp<-temp[temp$agent!="",]
temp<-temp[order(temp$year),]

# extract the first three digits
temp$zip3<-substr(temp$zipc, 1,3)

# exclude the zip code
temp<-temp[,-2]

# drop duplicates
temp<-unique(temp[ , 1:3 ] )
agent_zip<-temp

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

# for each year, we first compute the agents who connect to exiters
connect_agent<-list()
# loop through
for(i in 1:length(exit))
{
  # for exiters acting as listers
  temp1<-X[X$close_year==year[i],] 
  temp1<-temp1[temp1$lagent_id %in% exit[[i]],]
  # take the bagent out
  b<-temp1$bagent_id
  
  # for exiters acting as buyers
  temp1<-X[X$list_year==year[i],] 
  temp1<-temp1[temp1$bagent_id %in% exit[[i]],]
  # take the lagent out
  b<-c(b,temp1$lagent_id)
  
  # include only unique agent
  b<-b[b!=""]
  b<-unique(b)
  
  # also we excude the exiters themselves
  b<-b[!(b %in% exit[[i]])]
  connect_agent[[i]]<-b
  
  # print the process
  print(i)
}

# for each year, calculate the agents who do not connect to exiters
notconnect_agent<-list()
# loop through year
for(i in 1:length(exit))
{
  # extracting all agents
  A<-active[active[,i]>=1,]
  A<-row.names(A)
  A<-unique(A)
  # calculating the agents who do not connect
  B<-setdiff(A, connect_agent[[i]])
  
  # exclude the exiters
  B<-B[!(B %in% exit[[i]])]
  
  # save
  notconnect_agent[[i]]<-B
  # print the process
  print(i)
}

# restrict the disconnected group to have the same 3-dight zip code
for(i in 1:length(notconnect_agent))
{
# extract the connected_agent zip
z<-agent_zip[agent_zip$year==2000+i,]
z<-z[z$agent %in% connect_agent[[i]],]
z<-unique(z$zip3)

# extract the disconnected_agent zip
zd<-agent_zip[agent_zip$year==2000+i,]
zd<-zd[zd$agent %in% notconnect_agent[[i]],]

# subset
zd<-zd[zd$zip3 %in% z,]

# add in
notconnect_agent[[i]]<-zd$agent

# print the process
print(2000+i)
}

# prob of sales

# probability of sales
# we calculate the probability of sale for each agent for each year
prob_sale<-data.frame(NA, NA, NA)
colnames(prob_sale)<-c("Agent_ID","Year","Prob")

# a loop through to calculate that
for(i in 1:length(year))
{
  # list in that year
  temp1<-X[X$list_year==year[i],]
  # if close price is non-missing, then it was sold
  temp1<-data.frame(temp1$lagent_id, temp1$close_price)
  # for non-missing price, assign 1 and 0 o.w.
  temp1$temp1.close_price[!is.na(temp1$temp1.close_price)]<-1
  temp1$temp1.close_price[is.na(temp1$temp1.close_price)]<-0
  # the probs
  temp1<-aggregate(temp1$temp1.close_price~temp1$temp1.lagent_id, FUN=mean)
  colnames(temp1)<-c("Agent_ID","Prob")
  temp1$Year<-year[i]
  # merge
  prob_sale<-rbind(prob_sale, temp1)
  # print the progress
  print(i)
}

# remove the first obs
prob_sale<-prob_sale[-1,]

# augment it to create a balanced dataset
df_b<-data.frame(Year = rep(unique(prob_sale$Year), length(unique(prob_sale$Agent_ID))),
                 Agent_ID = rep(unique(prob_sale$Agent_ID), each = length(unique(prob_sale$Year))))
prob_sale<-left_join(df_b, prob_sale)
# NA to 0
prob_sale[is.na(prob_sale$Prob),3]<-0


# we create a treatment dataset
# such that it contains: agent, event time, year of transaction
treatment_trans<-data.frame(NA, NA, NA,NA)
colnames(treatment_trans)<-c("agent","year","num_trans","event_time")
# restrict to -3 to 3

# loop through all treatment year
for(i in 1:length(connect_agent))
{
  # extract the trans for these agents
  temp<-trans[trans$Agent_ID %in% connect_agent[[i]],]
  # add event time
  temp$event_time<-temp$Year-(2000+i)
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
treatment_trans$group<-"Treatment"

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
control_trans$group<-"Control"

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
  expand_limits(y = c(0, 40))+
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

# similarly for probability of sales

# we create a treatment dataset
# such that it contains: agent, event time, year of transaction
treatment_trans<-data.frame(NA, NA, NA,NA)
colnames(treatment_trans)<-c("agent","year","num_trans","event_time")
# restrict to -3 to 3

# loop through all treatment year
for(i in 1:length(connect_agent))
{
  # extract the trans for these agents
  temp<-prob_sale[prob_sale$Agent_ID %in% connect_agent[[i]],]
  # add event time
  temp$event_time<-temp$Year-(2000+i)
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
treatment_trans$group<-"Treatment"

# we create a control dataset
# such that it contains: agent, event time, year of transaction
control_trans<-data.frame(NA, NA, NA,NA)
colnames(control_trans)<-c("agent","year","num_trans","event_time")
# restrict to -3 to 3

# loop through all control year
for(i in 1:length(notconnect_agent))
{
  # extract the trans for these agents
  temp<-prob_sale[prob_sale$Agent_ID %in% notconnect_agent[[i]],]
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
control_trans$group<-"Control"


# a data frame to plot
df<-rbind(treatment_trans, control_trans)
# average by event time and group identity
df<-aggregate(df$num_trans~df$group+df$event_time, FUN=mean)
colnames(df)<-c("group","event_time","num")

# plot
ggplot(df, aes(x=event_time,y=num,group=group,color=group))+
  geom_line(size=2)+
  theme_bw()+
  ylab("Mean Prob of Sales by Event Time")+
  xlab("Event Time")+
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25))+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_discrete(limits=c(-3:3))+
  expand_limits(y = c(0.1, 0.55))+
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
  ylab("Mean Prob of Sales Difference by Event Time")+
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


# then we plot conditional on event year

# we create a treatment dataset
# such that it contains: agent, event time, year of transaction
treatment_trans<-data.frame(NA, NA, NA,NA, NA)
colnames(treatment_trans)<-c("agent","year","num_trans","event_time","event_year")
# restrict to -3 to 3

# loop through all treatment year
for(i in 1:length(connect_agent))
{
  # extract the trans for these agents
  temp<-trans[trans$Agent_ID %in% connect_agent[[i]],]
  # add event time
  temp$event_time<-temp$Year-(2000+i)
  # restrict to -3 to 3
  temp<-temp[temp$event_time %in% c(-3:3),]
  # add event year
  temp$event_year<-2000+i
  # change column names
  colnames(temp)<-c("agent","year","num_trans","event_time","event_year")
  # merge
  treatment_trans<-rbind(treatment_trans, temp)
  # print the process
  print(2000+i)
}
treatment_trans<-treatment_trans[-1,]
# add group name
treatment_trans$group<-"Treatment"

# we create a control dataset
# such that it contains: agent, event time, year of transaction
control_trans<-data.frame(NA, NA, NA,NA,NA)
colnames(control_trans)<-c("agent","year","num_trans","event_time","event_year")
# restrict to -3 to 3

# loop through all control year
for(i in 1:length(notconnect_agent))
{
  # extract the trans for these agents
  temp<-trans[trans$Agent_ID %in% notconnect_agent[[i]],]
  # add event time
  temp$event_time<-temp$Year-(2000+i)
  # restrict to -3 to 3
  temp<-temp[temp$event_time %in% c(-3:3),]
  # add event year
  temp$event_year<-2000+i
  # change column names
  colnames(temp)<-c("agent","year","num_trans","event_time","event_year")
  # merge
  control_trans<-rbind(control_trans, temp)
  # print the process
  print(2000+i)
}
control_trans<-control_trans[-1,]
# add group name
control_trans$group<-"Control"

# a data frame to plot
df<-rbind(treatment_trans, control_trans)
# average by event time and event year
df<-aggregate(df$num_trans~df$group+df$event_time+df$event_year, FUN=mean)
colnames(df)<-c("group","event_time","event_year","num")

# plot by each year
for(i in unique(df$event_year))
{
  # subset
  df1<-df[df$event_year==i,]
  # the difference
  d1<-df1[df1$group=="Control",]
  d2<-df1[df1$group=="Treatment",]
  # order by event time
  d1<-d1[order(d1$event_time),]
  d2<-d2[order(d2$event_time),]
  # the difference
  d1$num<-d2$num-d1$num
  # plot
  g<-ggplot(d1, aes(x=event_time,y=num))+
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
  # save
  Name<-paste("trans_diff_year_",i,"_pre2013_match.pdf",sep="")
  ggsave(g, file=Name,width = 25, height = 20, units = "cm")
  # print the process
  print(i)
}

# then we plot conditional on event year
# for prob of sales

# we create a treatment dataset
# such that it contains: agent, event time, year of transaction
treatment_trans<-data.frame(NA, NA, NA,NA, NA)
colnames(treatment_trans)<-c("agent","year","num_trans","event_time","event_year")
# restrict to -3 to 3

# loop through all treatment year
for(i in 1:length(connect_agent))
{
  # extract the trans for these agents
  temp<-prob_sale[prob_sale$Agent_ID %in% connect_agent[[i]],]
  # add event time
  temp$event_time<-temp$Year-(2000+i)
  # restrict to -3 to 3
  temp<-temp[temp$event_time %in% c(-3:3),]
  # add event year
  temp$event_year<-2000+i
  # change column names
  colnames(temp)<-c("agent","year","num_trans","event_time","event_year")
  # merge
  treatment_trans<-rbind(treatment_trans, temp)
  # print the process
  print(2000+i)
}
treatment_trans<-treatment_trans[-1,]
# add group name
treatment_trans$group<-"Treatment"

# we create a control dataset
# such that it contains: agent, event time, year of transaction
control_trans<-data.frame(NA, NA, NA,NA,NA)
colnames(control_trans)<-c("agent","year","num_trans","event_time","event_year")
# restrict to -3 to 3

# loop through all control year
for(i in 1:length(notconnect_agent))
{
  # extract the trans for these agents
  temp<-prob_sale[prob_sale$Agent_ID %in% notconnect_agent[[i]],]
  # add event time
  temp$event_time<-temp$Year-(2000+i)
  # restrict to -3 to 3
  temp<-temp[temp$event_time %in% c(-3:3),]
  # add event year
  temp$event_year<-2000+i
  # change column names
  colnames(temp)<-c("agent","year","num_trans","event_time","event_year")
  # merge
  control_trans<-rbind(control_trans, temp)
  # print the process
  print(2000+i)
}
control_trans<-control_trans[-1,]
# add group name
control_trans$group<-"Control"

# a data frame to plot
df<-rbind(treatment_trans, control_trans)
# average by event time and event year
df<-aggregate(df$num_trans~df$group+df$event_time+df$event_year, FUN=mean)
colnames(df)<-c("group","event_time","event_year","num")

# plot by each year
for(i in unique(df$event_year))
{
  # subset
  df1<-df[df$event_year==i,]
  # the difference
  d1<-df1[df1$group=="Control",]
  d2<-df1[df1$group=="Treatment",]
  # order by event time
  d1<-d1[order(d1$event_time),]
  d2<-d2[order(d2$event_time),]
  # the difference
  d1$num<-d2$num-d1$num
  # plot
  g<-ggplot(d1, aes(x=event_time,y=num))+
    geom_line(size=2)+
    theme_bw()+
    ylab("Mean Prob of Sales Difference by Event Time")+
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
  # save
  Name<-paste("probs_diff_year_",i,"_pre2013_match.pdf",sep="")
  ggsave(g, file=Name,width = 25, height = 20, units = "cm")
  # print the process
  print(i)
}
