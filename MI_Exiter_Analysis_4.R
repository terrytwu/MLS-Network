
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


## First Version: Oct 11, 2019
## This Version: Oct 13, 2019

# the following is for transaction mean and prob of sales analysis
# restricting to pre-2013 subsample

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


# create the probability as sale within 1 percent of list price
prob_sale1<-data.frame(NA, NA, NA)
colnames(prob_sale1)<-c("Agent_ID","Year","Prob")

# create a price dummy, s.t. if close price is within 1% of list price
X$price_dummy<-abs(X$close_price-X$list_price)
X$price_dummy<-X$price_dummy/X$list_price
X$price_dummy<-X$price_dummy<=0.01
X$price_dummy[X$price_dummy]<-1
X$price_dummy[!X$price_dummy]<-0

# a loop through to calculate that
for(i in 1:length(year))
{
  # list in that year
  temp1<-X[X$list_year==year[i],]
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
  colnames(temp1)<-c("Agent_ID","Prob")
  temp1$Year<-year[i]
  # merge
  prob_sale1<-rbind(prob_sale1, temp1)
  # print the progress
  print(i)
}

# remove the first obs
prob_sale1<-prob_sale1[-1,]

# augment it to create a balanced dataset
df_b<-data.frame(Year = rep(unique(prob_sale1$Year), length(unique(prob_sale1$Agent_ID))),
                 Agent_ID = rep(unique(prob_sale1$Agent_ID), each = length(unique(prob_sale1$Year))))
prob_sale1<-left_join(df_b, prob_sale1)


# form a baseline dataset to estimate regression equation
# the first var is agent id, the second var is treatment year, the third var is connected/disconnected dummy
mydf<-data.frame(NA, NA, NA)
colnames(mydf)<-c("agent_id","event_year","group")
# loop through year to add in
for(i in 1:length(connect_agent))
{
# connected group
temp<-data.frame(connect_agent[[i]])
temp$event_year<-2001+i
temp$group<-"connected"
colnames(temp)[1]<-"agent_id"
# merge
mydf<-rbind(mydf, temp)
# disconnected group
temp<-data.frame(notconnect_agent[[i]])
temp$event_year<-2001+i
temp$group<-"disconnected"
colnames(temp)[1]<-"agent_id"
# merge
mydf<-rbind(mydf, temp)
# print the process
print(i)
}
mydf<-mydf[-1,]

# then find the transactions
# a new transaction data to merge
trans2<-trans
colnames(trans2)<-c("trans_year","agent_id","num_trans")
# merge
m<-merge(trans2,mydf,by="agent_id",all=T)
# remove missing event year and group
m<-m[!is.na(m$event_year),]
m<-m[!is.na(m$group),]

# then add a event_time_year
m$event_time_year<-m$trans_year-m$event_year

# write out and estimate it in Stata
#write.csv(m,"/Users/terrywu/Dropbox/MLS Network/Exiter Analysis 2/df1.csv",row.names = F)

# then create a similiar dataset whose outcome is probability of sales

# a new prob data to merge
trans2<-prob_sale
colnames(trans2)<-c("prob_year","agent_id","prob")
# merge
m<-merge(trans2,mydf,by="agent_id",all=T)
# remove missing event year and group
m<-m[!is.na(m$event_year),]
m<-m[!is.na(m$group),]
m<-m[!is.na(m$prob_year),]
m<-m[!is.na(m$prob),]

# then add a event_time_year
m$event_time_year<-m$prob_year-m$event_year

# write out and estimate it in Stata
#write.csv(m,"/Users/terrywu/Dropbox/MLS Network/Exiter Analysis 2/df2.csv",row.names = F)

# a new prob data to merge
trans2<-prob_sale1
colnames(trans2)<-c("prob_year","agent_id","prob")
# merge
m<-merge(trans2,mydf,by="agent_id",all=T)
# remove missing event year and group
m<-m[!is.na(m$event_year),]
m<-m[!is.na(m$group),]
m<-m[!is.na(m$prob_year),]
m<-m[!is.na(m$prob),]

# then add a event_time_year
m$event_time_year<-m$prob_year-m$event_year

# write out and estimate it in Stata
#write.csv(m,"/Users/terrywu/Dropbox/MLS Network/Exiter Analysis 2/df3.csv",row.names = F)



# some plots
# 1. Transactions, read dataset
my1<-read.csv("/Users/terrywu/Dropbox/MLS Network/Exiter Analysis 2/df1.csv")
# keep useful vars
my1<-my1[my1$event_time_year %in% c(-3:3),]
my1<-my1[,c(3,5,6)]
# average 
my11<-aggregate(my1$num_trans~my1$group+my1$event_time_year,FUN=mean)
colnames(my11)<-c("group","event_time","num")

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

# add a difference graph
df1<-my11[my11$group=="connected",]
df2<-my11[my11$group=="disconnected",]
# order by event time
df1<-df1[order(df1$event_time),]
df2<-df2[order(df2$event_time),]
# the difference
df1$num<-df1$num-df2$num
# plot
ggplot(df1, aes(x=event_time,y=num))+
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

# 2. Prob def 1, read dataset
my1<-read.csv("/Users/terrywu/Dropbox/MLS Network/Exiter Analysis 2/df2.csv")
# keep useful vars
my1<-my1[my1$event_time_year %in% c(-3:3),]
my1<-my1[,c(3,5,6)]
# average 
my11<-aggregate(my1$prob~my1$group+my1$event_time_year,FUN=mean)
colnames(my11)<-c("group","event_time","num")

# plot
ggplot(my11, aes(x=event_time,y=num,group=group,color=group))+
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

# add a difference graph
df1<-my11[my11$group=="connected",]
df2<-my11[my11$group=="disconnected",]
# order by event time
df1<-df1[order(df1$event_time),]
df2<-df2[order(df2$event_time),]
# the difference
df1$num<-df1$num-df2$num
# plot
ggplot(df1, aes(x=event_time,y=num))+
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


# 3. Prob def 2, read dataset
my1<-read.csv("/Users/terrywu/Dropbox/MLS Network/Exiter Analysis 2/df3.csv")
# keep useful vars
my1<-my1[my1$event_time_year %in% c(-3:3),]
my1<-my1[,c(3,5,6)]
# average 
my11<-aggregate(my1$prob~my1$group+my1$event_time_year,FUN=mean)
colnames(my11)<-c("group","event_time","num")

# plot
ggplot(my11, aes(x=event_time,y=num,group=group,color=group))+
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

# add a difference graph
df1<-my11[my11$group=="connected",]
df2<-my11[my11$group=="disconnected",]
# order by event time
df1<-df1[order(df1$event_time),]
df2<-df2[order(df2$event_time),]
# the difference
df1$num<-df1$num-df2$num
# plot
ggplot(df1, aes(x=event_time,y=num))+
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
