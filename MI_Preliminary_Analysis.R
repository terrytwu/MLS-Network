
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

# The code file for generating analysis in the document "Summary_Aug15_2018"
# for the Michigan data set
# First Version: Sep 1st
# This Version: Sep 1st

## 1. Extract the network link

# set the working directory and read data
setwd("/Users/terrywu/Dropbox/MLS Network")
X<-read.dta13("full_w_deeds_prepped_wc41.dta")

# extract the id of each agent (no duplication)
agent<-c(X$bagent_id, X$lagent_id)
agent<-unique(agent)

# remove empty agent
agent<-agent[agent!=""]

# so the variable agent is the list of all agents (either buying or listing)
# in order to construct a network, we are interested in whether
# the property is sold or not, so we take a subset

# subset
Y<-data.frame(X$lagent_id, X$bagent_id, X$sale2)
# change the var name
colnames(Y)<-c("listing_agent","buying_agent","sale")
# exclude sale2==0
Y<-Y[which(Y$sale==1),]

# write out the edge-list
Z<-Y[,-3]
write.csv(Z,"/Users/terrywu/Dropbox/MLS Network/Analysis Sept 1/Edge_list.csv",row.names = F)  

# another type of edge list
Z<-aggregate(Y$sale~Y$listing_agent+Y$buying_agent, FUN=sum)
# change var name
colnames(Z)<-c("listing_agent","buying_agent","num_edge")

# write out the edge-list
write.csv(Z,"/Users/terrywu/Dropbox/MLS Network/Analysis Sept 1/Edge_list_2.csv",row.names = F)  

rm(list=ls())

## 2. Construct a panel dataset

# set the working directory and read data
setwd("/Users/terrywu/Dropbox/MLS Network")
X<-read.dta13("full_w_deeds_prepped_wc41.dta")

# in order to consturct a balanced dataset
# we need to know the time span
year<-c(X$list_year, X$close_year)
year<-unique(year)
year<-sort(year)

# we need 6 variables

# extract agent id and name
agent<-c(X$lagent_id, X$bagent_id)
agent<-unique(agent)
agent<-agent[!is.na(agent)]
agent<-agent[agent!=""]

# extract name and id
Y<-data.frame(X$lagent_id, X$lagent_name)
colnames(Y)<-c("ID","Name")
Y<-unique(Y[1:2])

# create a empty dataset
mydata<-data.frame(rep(agent, length(year)),NA,NA,NA,NA,NA)
colnames(mydata)<-c("agent_id","agent_name","num_closed_transaction","num_listed_transaction","year","office_name")
mydata[,c(3,4)]<-0

# year to the dataset
mydata$year<-rep(year, each=length(agent))

# agent name into the dataset
temp<-data.frame(agent, NA)
colnames(temp)<-c("id","name")
colnames(Y)<-c("id","name")
Y$name<-as.character(Y$name)
a<-fmatch(Y$id,temp$id)
temp[a,2]<-as.character(Y[,2])
mydata$agent_name<-rep(temp[,2],length(year))

# create a balanced panel dataset

# listing num by year for each agent
X$Num<-1
temp<-aggregate(X$Num~X$lagent_id+X$list_year, FUN = sum)
colnames(temp)<-c("agent_id","year","num_listed_transaction")

# merge
for(i in 1:length(year))
{
temp2<-temp[temp$year==i+2000,]  
submydata<-mydata[mydata$year==i+2000,]
id<-fmatch(temp2$agent_id,submydata$agent_id)
submydata[id,4]<-temp2[,3]
mydata[mydata$year==i+2000,]<-submydata
print(i)
}

# buying num by year for each agent
temp<-aggregate(X$Num~X$bagent_id+X$close_year, FUN = sum)
colnames(temp)<-c("agent_id","year","num_closed_transaction")
temp<-temp[temp$agent_id!="",]
temp<-temp[!is.na(temp$agent_id),]

# merge
for(i in 1:length(year))
{
temp2<-temp[temp$year==i+2000,]  
submydata<-mydata[mydata$year==i+2000,]
id<-fmatch(temp2$agent_id,submydata$agent_id)
submydata[id,3]<-temp2[,3]
mydata[mydata$year==i+2000,3]<-submydata[,3]
print(i)
}

# office name
temp<-data.frame(X$lagent_id, X$lagent_office_name, X$list_year)
temp<-unique(temp[,1:3])
colnames(temp)<-c("agent_id","office_name","year")
temp1<-data.frame(X$bagent_id, X$bagent_office_name, X$close_year)
temp1<-unique(temp1[,1:3])
colnames(temp1)<-c("agent_id","office_name","year")
temp<-rbind(temp,temp1)
temp<-unique(temp[,1:3])
temp<-temp[!is.na(temp$office_name),]
temp<-temp[temp$agent_id!="",]

# merge
for(i in 1:length(year))
{
temp1<-temp[temp$year==2000+i,]  
temp1<-temp1[,1:2]
temp1<-temp1[!duplicated(temp1[,1]),]
submydata<-mydata[mydata$year==i+2000,]
id<-fmatch(temp1$agent_id, submydata$agent_id)
submydata[id,6]<-as.character(temp1[,2])
mydata[mydata$year==i+2000,6]<-submydata[,6]
print(i)
}

# save the dataset
colnames(mydata)<-c("agent_id","agent_name","num_closed_transaction","num_listed_transaction","year","office_name")
write.csv(mydata,"/Users/terrywu/Dropbox/MLS Network/Analysis Sept 1/MLS_Panel.csv", row.names = F)

## Then the analysis of the dataset

rm(list=setdiff(ls(), "X"))

## 3. Sales Analysis (Average)

# exclude sale2==0
X<-X[X$sale2==1,]

# first of all, sales by year
sale_year<-X$close_year

# form a dataset
mydata<-data.frame(table(sale_year))

# sales by year graph
ggplot(data=mydata, aes(x=sale_year, y=Freq)) +
  geom_bar(stat="identity",fill="skyblue")+
  xlab("Year")+ylab("Freq")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_hline(yintercept=mean(mydata$Freq), linetype="dashed", color = "coral",size=1.3)

# average
mean(mydata$Freq)

# then sales by agents
# read dataset
X<-read.dta13("full_w_deeds_prepped_wc41.dta")

# agent list
agent<-c(X$lagent_id,X$bagent_id)
agent<-unique(agent)
agent<-agent[agent!=""]

# average sales of agents by year
mydata$Freq<-mydata$Freq/length(agent)

# avg sales by year graph (average by all)
ggplot(data=mydata, aes(x=sale_year, y=Freq)) +
  geom_bar(stat="identity",fill="skyblue")+
  xlab("Year")+ylab("Average")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_hline(yintercept=mean(mydata$Freq), linetype="dashed", color = "coral",size=1.3)

# average
mean(mydata$Freq)

# create a matrix to record the entry by each agent

# agent list
agent<-c(X$lagent_id,X$bagent_id)
agent<-unique(agent)
agent<-agent[!(is.na(agent))]
agent<-agent[agent!=""]

# year span
year<-c(X$close_year,X$list_year)
year<-unique(year)
year<-sort(year)

# create a matrix to show whether the agent is showed up in a given year
entry_matrix<-matrix(0, nrow = length(agent), ncol=length(year))

# loop to identify whether the agent is active in a given year
temp1<-data.frame(X$lagent_id, X$list_year)
colnames(temp1)<-c("agent","year")
temp1<-temp1[temp1$agent!="",]
temp2<-data.frame(X$bagent_id, X$close_year)
colnames(temp2)<-c("agent","year")
temp2<-temp2[temp2$agent!="",]
# aggregate 
temp1<-rbind(temp1,temp2)
temp1<-unique(temp1[1:2])
# a loop long to matrix
temp1<-temp1[order(temp1$year),]
for(i in 2001:2015)
{
temp<-temp1[temp1$year==i,]  
ID<-fmatch(temp$agent,agent)
entry_matrix[ID,i-2000]<-1
print(i)
}

# write out the matrix
rownames(entry_matrix)<-agent
colnames(entry_matrix)<-year
write.csv(entry_matrix,"/Users/terrywu/Dropbox/MLS Network/Analysis Sept 1/active_agent_by_year.csv")

# sales by active agent

# exclude sale2==0
X<-X[X$sale2==1,]

# first of all, sales by year
sale_year<-X$close_year

# form a dataset
mydata<-data.frame(table(sale_year))

# read active data
setwd("/Users/terrywu/Dropbox/MLS Network/Analysis Sept 1")
Y<-read.csv("active_agent_by_year.csv")

# calculate the number of active agents by year
active_num<-rep(NA, dim(Y)[2]-1)
active_num[1:2]<-colSums(Y[,-1])[1:2]

# a loop
for(i in 4:dim(Y)[2])
{
  temp<-Y[Y[,i]==1,]  
  temp<-temp[temp[,i-1]==1,]
  active_num[i-1]<-dim(temp)[1]
  print(i)
}

# average
mydata$ave<-mydata$Freq/active_num

# avg sales by year graph (average by all)
ggplot(data=mydata, aes(x=sale_year, y=ave)) +
  geom_bar(stat="identity",fill="skyblue")+
  xlab("Year")+ylab("Average")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

rm(list=ls())

## 4. Forced Sale Analysis

# set the working directory and read data
setwd("/Users/terrywu/Dropbox/MLS Network")
X<-read.dta13("full_w_deeds_prepped_wc41.dta")

# forced sales only
X<-X[X$forced_sale==1,]

# exclude sale2==0
X<-X[X$sale2==1,]

# first of all, forced sales by year
sale_year<-X$close_year

# form a dataset
mydata<-data.frame(table(sale_year))

# sales by year graph
ggplot(data=mydata, aes(x=sale_year, y=Freq)) +
  geom_bar(stat="identity",fill="skyblue")+
  xlab("Year")+ylab("Freq")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_hline(yintercept=mean(mydata$Freq), linetype="dashed", color = "coral",size=1.3)

# average
mean(mydata$Freq)

# then sales by agents

# agent list
agent<-c(X$lagent_id,X$bagent_id)
agent<-unique(agent)
agent<-agent[agent!=""]

# average sales of agents by year
mydata$Freq<-mydata$Freq/length(agent)

# avg sales by year graph (average by all)
ggplot(data=mydata, aes(x=sale_year, y=Freq)) +
  geom_bar(stat="identity",fill="skyblue")+
  xlab("Year")+ylab("Average")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_hline(yintercept=mean(mydata$Freq), linetype="dashed", color = "coral",size=1.3)

# average
mean(mydata$Freq)

rm(list=ls())

# sales by active agents

# first a entry matrix

# read the dataset
X<-read.dta13("full_w_deeds_prepped_wc41.dta")

# forced
X<-X[X$sale2==1,]
X<-X[X$forced_sale==1,]

# identify when the agent enters

# agent list
agent<-c(X$lagent_id,X$bagent_id)
agent<-unique(agent)
agent<-agent[!(is.na(agent))]
agent<-agent[agent!=""]

# year span
year<-c(X$close_year,X$list_year)
year<-unique(year)
year<-sort(year)

# create a matrix to show whether the agent is showed up in a given year
entry_matrix<-matrix(0, nrow = length(agent), ncol=length(year))

# loop to identify whether the agent is active in a given year
temp1<-data.frame(X$lagent_id, X$list_year)
colnames(temp1)<-c("agent","year")
temp1<-temp1[temp1$agent!="",]
temp2<-data.frame(X$bagent_id, X$close_year)
colnames(temp2)<-c("agent","year")
temp2<-temp2[temp2$agent!="",]
# aggregate 
temp1<-rbind(temp1,temp2)
temp1<-unique(temp1[1:2])
# a loop long to matrix
temp1<-temp1[order(temp1$year),]
for(i in 2001:2014)
{
  temp<-temp1[temp1$year==i,]  
  ID<-fmatch(temp$agent,agent)
  entry_matrix[ID,i-2000]<-1
  print(i)
}

# write out the matrix
rownames(entry_matrix)<-agent
colnames(entry_matrix)<-year

# entry_matrix rename
Y<-entry_matrix

# the average sales

# set the working directory and read data
setwd("/Users/terrywu/Dropbox/MLS Network")
X<-read.dta13("full_w_deeds_prepped_wc41.dta")

# forced
X<-X[X$forced_sale==1,]

# exclude sale2==0
X<-X[X$sale2==1,]

# first of all, sales by year
sale_year<-X$close_year

# form a dataset
mydata<-data.frame(table(sale_year))

# calculate the number of active agents by year
active_num<-rep(NA, dim(Y)[2])
active_num[1:2]<-colSums(Y[,])[1:2]

# a loop
for(i in 3:dim(Y)[2])
{
  temp<-Y[Y[,i]==1,]  
  temp<-temp[temp[,i-1]==1,]
  if(class(temp)=="numeric")
  {
    temp<-as.matrix(t(temp))
  }
  active_num[i]<-dim(temp)[1]
  print(i)
}

# average
mydata$ave<-mydata$Freq/active_num[-c(1,2)]

# avg sales by year graph (average by active agents)
mydata<-mydata[-1,]
ggplot(data=mydata, aes(x=sale_year, y=ave)) +
  geom_bar(stat="identity",fill="skyblue")+
  xlab("Year")+ylab("Average")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

rm(list=ls())

## 5. Non-forced Sales Analysis

# set the working directory and read data
setwd("/Users/terrywu/Dropbox/MLS Network")
X<-read.dta13("full_w_deeds_prepped_wc41.dta")

# non-forced sales only
X<-X[X$forced_sale!=1,]

# exclude sale2==0
X<-X[X$sale2==1,]

# first of all, forced sales by year
sale_year<-X$close_year

# form a dataset
mydata<-data.frame(table(sale_year))

# sales by year graph
ggplot(data=mydata, aes(x=sale_year, y=Freq)) +
  geom_bar(stat="identity",fill="skyblue")+
  xlab("Year")+ylab("Freq")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_hline(yintercept=mean(mydata$Freq), linetype="dashed", color = "coral",size=1.3)

# average
mean(mydata$Freq)

# then sales by agents

# agent list
agent<-c(X$lagent_id,X$bagent_id)
agent<-unique(agent)
agent<-agent[!is.na(agent)]
agent<-agent[agent!=""]

# average sales of agents by year
mydata$Freq<-mydata$Freq/length(agent)

# avg sales by year graph (average by all)
ggplot(data=mydata, aes(x=sale_year, y=Freq)) +
  geom_bar(stat="identity",fill="skyblue")+
  xlab("Year")+ylab("Average")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_hline(yintercept=mean(mydata$Freq), linetype="dashed", color = "coral",size=1.3)

# average
mean(mydata$Freq)

rm(list=ls())

# sales by active agents

# first a entry matrix

# read the dataset
X<-read.dta13("full_w_deeds_prepped_wc41.dta")

# non-forced
X<-X[X$sale2==1,]
X<-X[X$forced_sale!=1,]

# identify when the agent enters

# agent list
agent<-c(X$lagent_id,X$bagent_id)
agent<-unique(agent)
agent<-agent[!(is.na(agent))]
agent<-agent[agent!=""]

# year span
year<-c(X$close_year,X$list_year)
year<-unique(year)
year<-sort(year)

# create a matrix to show whether the agent is showed up in a given year
entry_matrix<-matrix(0, nrow = length(agent), ncol=length(year))

# loop to identify whether the agent is active in a given year
temp1<-data.frame(X$lagent_id, X$list_year)
colnames(temp1)<-c("agent","year")
temp1<-temp1[temp1$agent!="",]
temp2<-data.frame(X$bagent_id, X$close_year)
colnames(temp2)<-c("agent","year")
temp2<-temp2[temp2$agent!="",]
# aggregate 
temp1<-rbind(temp1,temp2)
temp1<-unique(temp1[1:2])
# a loop long to matrix
temp1<-temp1[order(temp1$year),]
for(i in 2001:2015)
{
  temp<-temp1[temp1$year==i,]  
  ID<-fmatch(temp$agent,agent)
  entry_matrix[ID,i-2000]<-1
  print(i)
}

# write out the matrix
rownames(entry_matrix)<-agent
colnames(entry_matrix)<-year

# entry_matrix rename
Y<-entry_matrix

# the average sales

# set the working directory and read data
setwd("/Users/terrywu/Dropbox/MLS Network")
X<-read.dta13("full_w_deeds_prepped_wc41.dta")

# non-forced
X<-X[X$forced_sale!=1,]

# exclude sale2==0
X<-X[X$sale2==1,]

# first of all, sales by year
sale_year<-X$close_year

# form a dataset
mydata<-data.frame(table(sale_year))

# calculate the number of active agents by year
active_num<-rep(NA, dim(Y)[2])
active_num[1:2]<-colSums(Y[,])[1:2]

# a loop
for(i in 3:dim(Y)[2])
{
  temp<-Y[Y[,i]==1,]  
  temp<-temp[temp[,i-1]==1,]
  if(class(temp)=="numeric")
  {
    temp<-as.matrix(t(temp))
  }
  active_num[i]<-dim(temp)[1]
  print(i)
}

# average
mydata$ave<-mydata$Freq/active_num

# avg sales by year graph (average by active agents)
mydata<-mydata[-1,]
ggplot(data=mydata, aes(x=sale_year, y=ave)) +
  geom_bar(stat="identity",fill="skyblue")+
  xlab("Year")+ylab("Average")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

rm(list=ls())

## 6. Network Characteristics

# read the dataset
setwd("/Users/terrywu/Dropbox/MLS Network/Analysis Sept 1")
X<-read.csv("Edge_list.csv",stringsAsFactors =  F)
X<-X[X$listing_agent!="",]
X<-X[X$buying_agent!="",]

# degree
# create a dataset with three columns
# outdegree, indegree and degree
# agent list
agent<-c(X[,1],X[,2])
agent<-unique(agent)
agent<-agent[agent!=""]
agent<-agent[!is.na(agent)]

# dataset
mydata<-data.frame(agent,NA, NA, NA)
colnames(mydata)<-c("Agent","Outdegree","Indegree","Degree")
mydata[,2]<-0
mydata[,3]<-0
mydata[,4]<-0
X$Num<-1
# Outdegree
temp<-aggregate(X$Num~X$listing_agent,FUN=sum)
mydata[fmatch(temp[,1],mydata$Agent),2]<-temp[,2]
# Indegree
temp<-aggregate(X$Num~X$buying_agent,FUN=sum)
mydata[fmatch(temp[,1],mydata$Agent),3]<-temp[,2]
# Degree
mydata[,4]<-mydata[,2]+mydata[,3]

# degree distribution
mydata1<-mydata[mydata$Outdegree<=20,]
ggplot(mydata1, aes(x=Outdegree)) + 
  geom_histogram(binwidth=1,color="black", fill="white")+
  theme_bw()+
  ylab("")+
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  theme(text = element_text(size=20))

mydata1<-mydata[mydata$Indegree<=20,]
ggplot(mydata1, aes(x=Indegree)) + 
  geom_histogram(binwidth=1,color="black", fill="white")+
  theme_bw()+
  ylab("")+
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  theme(text = element_text(size=20))

mydata1<-mydata[mydata$Degree<=20,]
ggplot(mydata1, aes(x=Degree)) + 
  geom_histogram(binwidth=1,color="black", fill="white")+
  theme_bw()+
  ylab("")+
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  theme(text = element_text(size=20))

# summary statistics of degrees
sum_data<-data.frame(NA, NA, NA, NA, NA, NA)
colnames(sum_data)<-names(summary(mydata$Outdegree))
sum_data[1,]<-as.numeric(summary(mydata$Outdegree))
sum_data<-rbind(sum_data,as.numeric(summary(mydata$Indegree)))
sum_data<-rbind(sum_data,as.numeric(summary(mydata$Degree)))
rownames(sum_data)<-c("Outdegree","Indegree","Degree")
# write out
xtable(sum_data,digits = 3)

# turn the edge list to a network object to compute the cc
X<-X[,-3]
g<-graph.data.frame(X,directed=TRUE)
coeff<-data.frame(as.vector(V(g)$name),transitivity(g,type="local"))
# summary statistics of cc
sum_data<-data.frame(NA, NA, NA, NA, NA, NA, NA)
colnames(sum_data)<-names(summary(coeff$transitivity.g..type....local..))
sum_data[1,]<-as.numeric(summary(coeff$transitivity.g..type....local..))
# write out
xtable(sum_data, digits = 3)

# summary statistics of directed cc
coeff<-data.frame(as.vector(V(g)$name),transitivity(g,type="barrat"))
sum_data<-data.frame(NA, NA, NA, NA, NA, NA, NA)
colnames(sum_data)<-names(summary(coeff$transitivity.g..type....barrat..))
sum_data[1,]<-as.numeric(summary(coeff$transitivity.g..type....barrat..))
# write out
xtable(sum_data, digits = 3)

# centrality
closeness.cent <- closeness(g, mode="all")
cent_data<-data.frame(NA, NA, NA, NA, NA, NA)
colnames(cent_data)<-names(summary(closeness.cent))
cent_data[1,]<-as.numeric(summary(closeness.cent))

eigen.cent<-eigen_centrality(g, directed=T)
cent_data<-rbind(cent_data,as.numeric(summary(eigen.cent$vector)))

bet.cent<-betweenness(g, directed=T)
cent_data<-rbind(cent_data,as.numeric(summary(bet.cent)))

cent_data$name<-c("Closeness","Eigenvector","Betweenness")

# write out
xtable(cent_data,digits = 8)

rm(list=ls())

## 7. Entry-Exit Analysis

# read the dataset
setwd("/Users/terrywu/Dropbox/MLS Network/Analysis Sept 1")
X<-read.csv("active_agent_by_year.csv")

# we would like to calculate entry and exit rate

# year span
year<-2001:2015

# number of active agents in each year
active_agent<-as.numeric(colSums (X[,-1], na.rm = FALSE, dims = 1))

### Enter
# for each year identify the number of agents
# that are not active of the previous year
# but active in the present year
thereof1<-rep(0,length(year))
for(i in 1:(length(year)-1))
{
  temp<-X[X[,i+2]==1,]
  temp<-temp[temp[,i+1]==0,]
  thereof1[i+1]<-dim(temp)[1]
}
# for each year identify the number of agents
# that are not active of the previous two years
thereof2<-rep(0,length(year))
for(i in 1:(length(year)-2))
{
  temp<-X[X[,i+3]==1,]
  temp<-temp[temp[,i+1]==0,]
  temp<-temp[temp[,i+2]==0,]
  thereof2[i+2]<-dim(temp)[1]
}

# for each year identify the number of agents
# that are not active of the previous three years
thereof3<-rep(0,length(year))
for(i in 1:(length(year)-3))
{
  temp<-X[X[,i+4]==1,]
  temp<-temp[temp[,i+1]==0,]
  temp<-temp[temp[,i+2]==0,]
  temp<-temp[temp[,i+3]==0,]
  thereof3[i+3]<-dim(temp)[1]
}

# for each year identify the number of agents
# that are not active of the previous four years
thereof4<-rep(0,length(year))
for(i in 1:(length(year)-4))
{
  temp<-X[X[,i+5]==1,]
  temp<-temp[temp[,i+1]==0,]
  temp<-temp[temp[,i+2]==0,]
  temp<-temp[temp[,i+3]==0,]
  temp<-temp[temp[,i+4]==0,]
  thereof4[i+4]<-dim(temp)[1]
}

# for each year identify the number of agents
# that are not active of the previous five years
thereof5<-rep(0,length(year))
for(i in 1:(length(year)-5))
{
  temp<-X[X[,i+6]==1,]
  temp<-temp[temp[,i+1]==0,]
  temp<-temp[temp[,i+2]==0,]
  temp<-temp[temp[,i+3]==0,]
  temp<-temp[temp[,i+4]==0,]
  temp<-temp[temp[,i+5]==0,]
  thereof5[i+5]<-dim(temp)[1]
}

# form a dataset
thereof1<-thereof1/active_agent
thereof2<-thereof2/active_agent
thereof3<-thereof3/active_agent
thereof4<-thereof4/active_agent
thereof5<-thereof5/active_agent
# dataset
mydata<-data.frame(rep(year,5),c(thereof1,thereof2,thereof3,thereof4,thereof5),c(rep("1 year",length(year)),rep("2 year",length(year)),rep("3 year",length(year)),rep("4 year",length(year)),rep("5 year",length(year))))
colnames(mydata)<-c("Year","Percent","supp")
# plot
mydata<-mydata[mydata$Percent!=0,]
ggplot(mydata, aes(x=Year, y=Percent, group=supp, color=supp)) +
  geom_line(size=3)+
  scale_color_brewer(palette="Paired")+
  theme_minimal()+
  theme_bw()+
  ylab("Ratio")+
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25))+
  theme(text = element_text(size=25))+
  theme(legend.position="bottom") +
  scale_x_discrete(limits=c(2002:2014))+
  theme(legend.title=element_blank())+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


### Exit
# for each year identify the number of agents
# that are not active of the next year
# but active in the present year
thereof1<-rep(0,length(year))
for(i in 1:(length(year)-1))
{
  temp<-X[X[,i+1]==1,]
  temp<-temp[temp[,i+2]==0,]
  thereof1[i]<-dim(temp)[1]
}
# for each year identify the number of agents
# that are not active of the next two years
thereof2<-rep(0,length(year))
for(i in 1:(length(year)-2))
{
  temp<-X[X[,i+1]==1,]
  temp<-temp[temp[,i+2]==0,]
  temp<-temp[temp[,i+3]==0,]
  thereof2[i]<-dim(temp)[1]
}

# for each year identify the number of agents
# that are not active of the next three years
thereof3<-rep(0,length(year))
for(i in 1:(length(year)-3))
{
  temp<-X[X[,i+1]==1,]
  temp<-temp[temp[,i+2]==0,]
  temp<-temp[temp[,i+3]==0,]
  temp<-temp[temp[,i+4]==0,]
  thereof3[i]<-dim(temp)[1]
}

# for each year identify the number of agents
# that are not active of the next four years
thereof4<-rep(0,length(year))
for(i in 1:(length(year)-4))
{
  temp<-X[X[,i+1]==1,]
  temp<-temp[temp[,i+2]==0,]
  temp<-temp[temp[,i+3]==0,]
  temp<-temp[temp[,i+4]==0,]
  temp<-temp[temp[,i+5]==0,]
  thereof4[i]<-dim(temp)[1]
}

# for each year identify the number of agents
# that are not active of the next five years
thereof5<-rep(0,length(year))
for(i in 1:(length(year)-5))
{
  temp<-X[X[,i+1]==1,]
  temp<-temp[temp[,i+2]==0,]
  temp<-temp[temp[,i+3]==0,]
  temp<-temp[temp[,i+4]==0,]
  temp<-temp[temp[,i+5]==0,]
  temp<-temp[temp[,i+6]==0,]
  thereof5[i]<-dim(temp)[1]
}

# form a dataset
thereof1<-thereof1/active_agent
thereof2<-thereof2/active_agent
thereof3<-thereof3/active_agent
thereof4<-thereof4/active_agent
thereof5<-thereof5/active_agent
# dataset
mydata<-data.frame(rep(year,5),c(thereof1,thereof2,thereof3,thereof4,thereof5),c(rep("1 year",length(year)),rep("2 year",length(year)),rep("3 year",length(year)),rep("4 year",length(year)),rep("5 year",length(year))))
colnames(mydata)<-c("Year","Percent","supp")
# plot
mydata<-mydata[mydata$Percent!=0,]
ggplot(mydata, aes(x=Year, y=Percent, group=supp, color=supp)) +
  geom_line(size=3)+
  scale_color_brewer(palette="Paired")+
  theme_minimal()+
  theme_bw()+
  ylab("Ratio")+
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25))+
  theme(text = element_text(size=25))+
  theme(legend.position="bottom") +
  scale_x_discrete(limits=c(2001:2014))+
  theme(legend.title=element_blank())+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

rm(list=ls())

## 8. Superstar Analysis (Disappear and Transfer)

# read the dataset
setwd("/Users/terrywu/Dropbox/MLS Network")
load("Michigan_Full.RData")

# construct a matrix
# to record the number of transcations of each agent in each year

# identify when the agent enters

# agent list
agent<-c(X$lagent_id,X$bagent_id)
agent<-unique(agent)
agent<-agent[!(is.na(agent))]
agent<-agent[agent!=""]

# year span
year<-c(X$close_year,X$list_year)
year<-unique(year)
year<-sort(year)

# create a matrix to record the number of transaction of each agent by year
trans_matrix<-matrix(0, nrow = length(agent), ncol=length(year))

# loop to identify the num of transcations in a given year
temp1<-data.frame(X$lagent_id, X$list_year)
colnames(temp1)<-c("agent","year")
temp1<-temp1[temp1$agent!="",]
temp2<-data.frame(X$bagent_id, X$close_year)
colnames(temp2)<-c("agent","year")
temp2<-temp2[temp2$agent!="",]
# aggregate 
temp1<-rbind(temp1,temp2)
temp1$Num<-1
temp1<-aggregate(temp1$Num~temp1$agent+temp1$year, FUN=sum)
# a loop long to matrix
temp1<-temp1[order(temp1[,2]),]
for(i in 2001:2015)
{
  temp<-temp1[temp1[,2]==i,]  
  ID<-fmatch(temp[,1],agent)
  trans_matrix[ID,i-2000]<-temp[,3]
  print(i)
}

# write out the matrix
rownames(trans_matrix)<-agent
colnames(trans_matrix)<-year
write.csv(trans_matrix,"/Users/terrywu/Dropbox/MLS Network/Analysis Sept 1/transaction_of_agent_by_year.csv")

# subset the matrix to include the agents who have 20 transcations in 
# at least one of the years

# the max transactions by an agent
trans_max<-rowMaxs(trans_matrix, value = TRUE)
trans_matrix<-trans_matrix[trans_max>=20,]

# then we plot the transactions by year
# with 90% CI
quan_matrix<-matrix(NA,nrow=3, ncol=15)
for(i in 1:15)
{
quan_matrix[,i]<-quantile(trans_matrix[,i], c(.1, .5, .9)) 
}
# reshape
my<-data.frame(c(quan_matrix[1,],quan_matrix[2,],quan_matrix[3,]))
my$Year<-rep(2001:2015,3)
my$Group<-c(rep("10%",15),rep("Med",15),rep("90%",15))
colnames(my)[1]<-"Y"

# plot
ggplot(my, aes(Year, Y, group=factor(Group)))+ 
  geom_line(aes(color=factor(Group)),size=2.4)+
  theme_bw()+
  ylab("Num of Transactions")+
  xlab("Year")+
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25))+
  theme(text = element_text(size=25))+
  theme(legend.position="bottom") +
  scale_x_discrete(limits=c(2001:2015))+
  theme(legend.title=element_blank())+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# record the id of the agents with many transactions
id_super<-row.names(trans_matrix)

# extract the offices of these agents

# define an address list
office<-list()

# as a lagent
temp<-X[X$lagent_id %in% id_super,]  
temp1<-data.frame(temp$lagent_id, temp$lagent_office_name)
colnames(temp1)<-c("ID","Office")
temp<-X[X$bagent_id %in% id_super,]  
temp2<-data.frame(temp$bagent_id, temp$bagent_office_name)
colnames(temp2)<-c("ID","Office")

# merge
temp<-rbind(temp1, temp2)
temp$Office<-tolower(temp$Office)
temp<-unique(temp[,1:2])
temp$ID<-as.numeric(as.character(temp$ID)) 
temp<-temp[!is.na(temp$ID),]
temp<-temp[!is.na(temp$Office),]
temp<-temp[order(temp$ID),]

# aggregate to calculate the number of office of each star
num_office<-aggregate(temp$Office~temp$ID, FUN=function(x) length(unique(x)))

# to two subset
num_change<-num_office[num_office$`temp$Office`>1,]
num_no_change<-num_office[num_office$`temp$Office`==1,]

# transactions of two subsets
trans_matrix_change<-trans_matrix[row.names(trans_matrix)%in% num_change$`temp$ID`,]
trans_matrix_no_change<-trans_matrix[row.names(trans_matrix)%in% num_no_change$`temp$ID`,]

# quantitles 
A<-apply(trans_matrix_change, 2, quantile, probs = c(0.1,0.5, 0.9),  na.rm = TRUE)
B<-apply(trans_matrix_no_change, 2, quantile, probs = c(0.1,0.5, 0.9),  na.rm = TRUE)

# to a dataset and plot
my<-data.frame(c(A[1,],A[2,],A[3,],B[1,],B[2,],B[3,]))
my$Year<-rep(2001:2015,6)
my$Group<-c(rep("Change 10%",15),rep("Change Med",15),rep("Change 90%",15),rep("No Change 10%",15),rep("No Change Med",15),rep("No Change 90%",15))
colnames(my)[1]<-"Y"

# plot
ggplot(my, aes(Year, Y, group=factor(Group)))+ 
  geom_line(aes(color=factor(Group)),size=2.4)+
  theme_bw()+
  ylab("Num of Transactions")+
  xlab("Year")+
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25))+
  theme(text = element_text(size=25))+
  theme(legend.position="bottom") +
  scale_x_discrete(limits=c(2001:2015))+
  theme(legend.title=element_blank())+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

rm(list=ls())

## 9. Partnership Analysis

# Whether still work together the next year

# read the dataset
setwd("/Users/terrywu/Dropbox/MLS Network")
load("Michigan_Full.RData")

# subset the dataset
X<-data.frame(X$lagent_id, X$bagent_id, X$list_year, X$close_year)
colnames(X)<-c("lagent","bagent","list_year","close_year")
X<-X[X$bagent!="",]
X<-X[,-3]

# so we have a dataset of partnership and year
X<-unique(X)
X$lagent<-as.character(X$lagent)
X$bagent<-as.character(X$bagent)
X<-X[X$lagent!=X$bagent,]

# calculate the num of partners by year
num<-rep(NA, length(unique(X$close_year)))
for(i in 2001:2015)
{
num[i-2000]<-sum(count(X[X$close_year==i,-3])[,3])
}
mydata<-data.frame(num,2001:2015)
colnames(mydata)<-c("Num","Year")

# calculate the num of partners who work together in the next year
part<-data.frame(mydata$Year,NA)
for(i in 1:14)
{
  # next year
  temp1<-X[X$close_year==part[i+1,1],]
  a<-as.character(temp1$lagent)
  b<-as.character(temp1$bagent)
  aa<-paste(a,b,sep=",")
  
  # this year
  temp2<-X[X$close_year==part[i,1],] 
  a<-as.character(temp2$lagent)
  b<-as.character(temp2$bagent)
  bb<-paste(a,b,sep=",")
  part[i,2]<-as.numeric(table(aa %in% bb))[2]
  
  print(i)
}
# change NA to 0
mydata<-mydata[-15,]
part<-part[-15,]
mydata[,1]<-part[,2]/mydata[,1]

# plot
ggplot(mydata,aes(Year, Num))+
  geom_line(size=3)+
  theme_bw()+
  ylab("Ratio")+
  xlab("Year")+
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  theme(text = element_text(size=20))+
  scale_x_discrete(limits=c(2001:2014))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

rm(list=ls())

# Network Formation

# read the dataset
setwd("/Users/terrywu/Dropbox/MLS Network/Analysis Sept 1")
X<-read.csv("active_agent_by_year.csv")

# year span
year<-2001:2015

# create a matrix to whether the agent enters by year
# using the two-year threshold
enter_matrix<-matrix(0, nrow=dim(X)[1], ncol=length(year))
colnames(enter_matrix)<-year
rownames(enter_matrix)<-X[,1]

### Enter
# for each year identify the enters
# that are not active of the previous two years
for(i in 1:(length(year)-2))
{
  temp<-X[X[,i+3]==1,]
  temp<-temp[temp[,i+1]==0,]
  temp<-temp[temp[,i+2]==0,]
  ID<-fmatch(temp[,1], X[,1])
  enter_matrix[ID,i+2]<-1
  print(i)
}

# read the transaction data
Y<-read.csv("transaction_of_agent_by_year.csv")

# calculate the number of the new enteries each year
enter_agent<-as.numeric(colSums(enter_matrix,dims=1))

# calculate the list of partners for each new agent

# set the working directory and read data
setwd("/Users/terrywu/Dropbox/MLS Network")
load("Michigan_Full.RData")

# create a vector to record the num of
# agents who enter and work with star agents
count<-rep(NA, length(enter_agent))

# the superstars' IDs
row.names(Y)<-Y[,1]
Y<-Y[,-1]
Y<-Y[row.names(Y)!="",]
mnumer<-apply(Y,1,max,na.rm=TRUE)
Y<-Y[mnumer>=20,]
star_id<-row.names(Y)

# for each year identify the enters
# that are not active of the previous two years
# and count whether they work with super stats

# read data
M<-data.frame(X$lagent_id, X$bagent_id, X$close_year)
M<-M[M[,2]!="",]
M[,1]<-as.character(M[,1])
M[,2]<-as.character(M[,2])
M<-M[M[,1]!=M[,2],]

# so M is the partnership dataset

setwd("/Users/terrywu/Dropbox/MLS Network/Analysis Sept 1")
X<-read.csv("active_agent_by_year.csv")

# for each year, we identify the number of partnerships with superstars
for(i in 1:(length(year)-2))
{
  temp<-X[X[,i+3]==1,]
  temp<-temp[temp[,i+1]==0,]
  temp<-temp[temp[,i+2]==0,]
  temp_id<-temp[,1]
  temp_id<-as.character(temp_id)
  # a subset to record the partership in a given year
  temp<-M[M[,3]==i+2002,]
  temp1<-temp[temp[,1] %in% temp_id,]
  temp2<-temp[temp[,2] %in% temp_id,]
  # filp temp2
  temp2<-data.frame(temp2[,2],temp2[,1],temp2[,3])
  colnames(temp1)<-c("ID1","ID2","Y")
  colnames(temp2)<-c("ID1","ID2","Y")
  temp<-rbind(temp1, temp2)
  temp<-temp[,-3]
  temp<-unique(temp[,1:2])
  # count the agents who work with superstar when they enter
  temp<-temp[temp[,2] %in% star_id,]
  count[i+2]<-length(unique(temp[,1]))
}

# the ratio
ratio<-count[-c(1,2)]/enter_agent[-c(1,2)]
# plot
myd<-data.frame(ratio[-13],2003:2014)
colnames(myd)<-c("ratio","year")

# plot
ggplot(myd,aes(year, ratio))+
  geom_line(size=3)+
  theme_bw()+
  ylab("Ratio")+
  xlab("Year")+
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  theme(text = element_text(size=20))+
  scale_x_discrete(limits=c(2003:2014))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
