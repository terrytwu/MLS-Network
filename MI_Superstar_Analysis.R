
library(readstata13)
library(ggplot2)
library(fastmatch)
library(xtable)
library(Rfast)
library(matrixStats)
library(sqldf)
library(reshape2)
library(plyr)
library(igraph)

# The code file for generating superstar analysis for the MI dataset
# First Version: Sep 8th
# This Version: Sep 10th

## 1. Def of Superstar

# set the working directory and read data
setwd("/Users/terrywu/Dropbox/MLS Network")
load("Michigan_Full.RData")

# clean dataset
Y<-X
Y<-Y[Y$bagent_id!="",]

# trascations per agent
trans<-c(Y$lagent_id, Y$bagent_id)
trans<-as.data.frame(table(trans)) 
colnames(trans)<-c("agent","transcation_num")

# summary stats
trans<-trans[order(-trans[,2]),]
# top 15%
cutoff<-round(dim(trans)[1]*0.15)
trans<-trans[1:cutoff,]

# clean dataset
Y<-X
Y<-Y[Y$lagent_id!=Y$bagent_id,]
Y<-Y[Y$bagent_id!="",]

# num of connected agents each year
Y<-data.frame(Y$lagent_id, Y$bagent_id)
Y[,2]<-as.character(Y[,2])
Y[,1]<-as.character(Y[,1])
Y1<-Y
Y2<-Y[,c(2,1)]
colnames(Y1)<-c("agent1","agent2")
colnames(Y2)<-c("agent1","agent2")
Y<-rbind(Y1,Y2)
Y<-unique(Y[,1:2])

# aggregate
Y$num<-1

# degree
deg<-aggregate(Y$num~Y$agent1, FUN=sum)
deg<-deg[order(-deg[,2]),]

# top 15%
cutoff<-round(dim(deg)[1]*0.15)
deg<-deg[1:cutoff,]

# interaction
trans[,1]<-as.character(trans[,1])
deg[,1]<-as.character(deg[,1])
agent<-intersect(trans[,1], deg[,1]) 

# num of agents in the dataset
num_agent<-c(X$lagent_id, X$bagent_id)
num_agent<-num_agent[num_agent!=""]
num_agent<-length(table(num_agent))

# ratio of superstar
length(agent)/num_agent

rm(list = ls())

# Histograms

# set the working directory and read data
setwd("/Users/terrywu/Dropbox/MLS Network")
load("Michigan_Full.RData")

# num of transactions by agent-year

# year span
year<-c(X$list_year, X$close_year)
year<-unique(year)
year<-year[order(year)]
year<-year[!is.na(year)]

# for each year, calculate the number of transactions
trans<-NA

# separate the datasets into two subs
# the first one contains self-transaction in a same year
# the second one does not
X_self<-X[X$lagent_id==X$bagent_id,]
X_self<-X_self[X_self$list_year==X_self$close_year,]
X_not<-X[!rownames(X) %in% rownames(X_self),]

# loop for calculate that
for(i in 1:length(year))
{
# no self transaction
temp1<-X_not[X_not$list_year==year[[i]],]
temp2<-X_not[X_not$close_year==year[[i]],]
temp1<-temp1$lagent_id
temp2<-temp2$bagent_id
agent<-c(temp1, temp2)
# self transaction
temp<-X_self$lagent_id
# transaction count
agent<-c(agent, temp)
agent<-agent[agent!=""]
A<-table(agent)
# add to vector
trans<-c(trans,as.vector(A))
print(i)
}

# plot
trans<-trans[-1]
trans<-log(trans)
mydata1<-data.frame(trans)

ggplot(mydata1, aes(x=trans)) + 
  geom_histogram(binwidth=0.5,color="black", fill="white")+
  theme_bw()+
  ylab("Freq")+
  xlab("Log(transactions)")+
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))+
  theme(text = element_text(size=20))

# percentile
trans<-exp(trans)
quantile(trans, c(.9995,.999,.995,.99,.95,.90,.85,.80)) 

# dollar value of transactions
value<-NA
for(i in 1:length(year))
{
temp<-X[X$close_year==year[i],]  
A<-aggregate(temp$sale_price~temp$bagent_id, FUN=sum)
A<-A[A[,1]!="",]
value<-c(value,A[,2])
print(i)
}

# plot
value<-value[-1]
value<-log(value)
mydata1<-data.frame(value)

ggplot(mydata1, aes(x=value)) + 
  geom_histogram(binwidth=0.5,color="black", fill="white")+
  theme_bw()+
  ylab("Freq")+
  xlab("Log(Value of Transactions)")+
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))+
  theme(text = element_text(size=20))

# percentile
value<-exp(value)
quantile(value, c(.9995,.999,.995,.99,.95,.90,.85,.80))

# then degree distribution

# clean data
X<-X[X$lagent_id!=X$bagent_id,]
X<-X[X$bagent_id!="",]

# a vector
deg<-NA

# loop
for(i in 1:length(year))
{
temp1<-X[X$list_year==year[i],]
temp2<-X[X$close_year==year[i],]
# connection list
temp1<-data.frame(temp1$lagent_id, temp1$bagent_id)
colnames(temp1)<-c("agent1","agent2")
temp2<-data.frame(temp2$lagent_id, temp2$bagent_id)
colnames(temp2)<-c("agent2","agent1")
temp2<-temp2[,c(2,1)]
# degree
temp<-rbind(temp1,temp2)
temp<-unique(temp)
temp$num<-1
degree<-aggregate(temp$num~temp$agent1, FUN=sum)
# add in
deg<-c(deg, degree[,2])
print(i)
}

# plot
deg<-deg[-1]
deg<-log(deg)
mydata1<-data.frame(deg)

ggplot(mydata1, aes(x=deg)) + 
  geom_histogram(binwidth=0.5,color="black", fill="white")+
  theme_bw()+
  ylab("Freq")+
  xlab("Log(Degree)")+
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))+
  theme(text = element_text(size=20))

# percentile
deg<-exp(deg)
quantile(deg, c(.9995,.999,.995,.99,.95,.90,.85,.80))

# Then the same exercise for those exit

# we first calculate the agent list
# such that the list of agents in each year we should focus on
# we calculate the agents who exit and put them
# in the year before them exit

# read active agent dataset
rm(list = ls())
load("Michigan_Full.RData")
setwd("/Users/terrywu/Dropbox/MLS Network/Analysis Sept 1")
A<-read.csv("active_agent_by_year.csv")
# clean active agent dataset
rownames(A)<-A[,1]
A<-A[,-1]

# create a list described before
active<-list()
for(i in 1:(dim(A)[2]-2))
{
temp<-A[A[,i+1]==0,]
temp<-temp[temp[,i+2]==0,]
temp<-temp[temp[,i]==1,]
active[[i]]<-rownames(temp)
print(i)
}

# then the list records the agents that we should focus on each year

# year span
year<-2001:2013

# a vector to record transactions
trans<-NA

# hist of transactions
for(i in 1:length(year))
{
temp1<-X[X$list_year==year[i],]
temp1<-temp1[temp1$lagent_id %in% active[[i]],]
temp2<-X[X$close_year==year[i],]
temp2<-temp2[temp2$bagent_id %in% active[[i]],]
# transactions by  agent
agent<-c(temp1$lagent_id, temp1$bagent_id)
agent<-agent[agent!=""]
my<-as.data.frame(table(agent))
# calculate the double counted transactions
temp<-X[X$lagent_id %in% active[[i]],]
temp<-temp[temp$bagent_id %in% active[[i]],]
temp<-temp[temp$list_year==year[i],]
temp<-temp[temp$close_year==year[i],]
temp<-temp[temp$lagent_id==temp$bagent_id,]
mysub<-as.data.frame(table(temp$lagent_id))
# substract
my[fmatch(mysub$Var1, my$agent),2]<-my[fmatch(mysub$Var1, my$agent),2]-mysub[,2]
# add in
trans<-c(trans,my[,2])
print(i)
}

# plot
trans<-trans[-1]
trans<-log(trans)
mydata1<-data.frame(trans)

ggplot(mydata1, aes(x=trans)) + 
  geom_histogram(binwidth=0.5,color="black", fill="white")+
  theme_bw()+
  ylab("Freq")+
  xlab("Log(transactions)")+
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))+
  theme(text = element_text(size=20))

# percentile
trans<-exp(trans)
quantile(trans, c(.9995,.999,.995,.99,.95,.90,.85,.80))

# then the value of transactions

# a vector to record value
value<-NA

# hist of transactions
for(i in 1:length(year))
{
temp<-X[X$close_year==year[i],]
temp<-temp[temp$bagent_id!="",]
temp<-temp[temp$lagent_id %in% active[[i]],]
temp<-temp[temp$bagent_id %in% active[[i]],]
temp<-aggregate(temp$sale_price~temp$bagent_id, FUN=sum)
value<-c(value,temp[,2])
print(i)
}

# plot
value<-value[-1]
value<-log(value)
mydata1<-data.frame(value)

ggplot(mydata1, aes(x=value)) + 
  geom_histogram(binwidth=0.5,color="black", fill="white")+
  theme_bw()+
  ylab("Freq")+
  xlab("Log(Value of Transactions)")+
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))+
  theme(text = element_text(size=20))

# percentile
value<-exp(value)
quantile(value, c(.9995,.999,.995,.99,.95,.90,.85,.80))

# then the degree

# clean data
X<-X[X$lagent_id!=X$bagent_id,]
X<-X[X$bagent_id!="",]

# a vector
deg<-NA

# loop
for(i in 1:length(year))
{
temp1<-X[X$list_year==year[i],]
temp2<-X[X$close_year==year[i],]
# connection list
temp1<-data.frame(temp1$lagent_id, temp1$bagent_id)
colnames(temp1)<-c("agent1","agent2")
temp2<-data.frame(temp2$lagent_id, temp2$bagent_id)
colnames(temp2)<-c("agent2","agent1")
temp2<-temp2[,c(2,1)]
# degree
temp<-rbind(temp1,temp2)
temp<-unique(temp)
temp$num<-1
degree<-aggregate(temp$num~temp$agent1, FUN=sum)
# remove
degree<-degree[degree[,1] %in% active[[i]],]
# add in
deg<-c(deg, degree[,2])
print(i)
}

# plot
deg<-deg[-1]
deg<-log(deg)
mydata1<-data.frame(deg)

ggplot(mydata1, aes(x=deg)) + 
  geom_histogram(binwidth=0.5,color="black", fill="white")+
  theme_bw()+
  ylab("Freq")+
  xlab("Log(Degree)")+
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))+
  theme(text = element_text(size=20))

# percentile
deg<-exp(deg)
quantile(deg, c(.9995,.999,.995,.99,.95,.90,.85,.80))
