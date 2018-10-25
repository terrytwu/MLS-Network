
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
library(tidyverse)

## First Version: Oct 21, 2019
## This Version: Oct 21, 2019

# set the working directory and read transaction dataset
setwd("/Users/terrywu/Dropbox/MLS Network")
load("Michigan_Full.RData")

# removed all "999999"
X<-X[X$lagent_id!="999999",]
X<-X[X$bagent_id!="999999",]

# build an active dataframe
Y1<-data.frame(X$lagent_id, X$list_year)
colnames(Y1)<-c("agent","year")
Y2<-data.frame(X$bagent_id, X$close_year)
colnames(Y2)<-c("agent","year")
# merge
active<-rbind(Y1,Y2)
# clean
active<-active[active$agent!="",]
active<-active[!is.na(active$agent),]
active<-active[!is.na(active$year),]
active<-active[active$year!="",]
# remove duplicates
active<-unique(active[ , 1:2])

# excluding post-2013
active<-active[active$year<=2012,]

# addin a number
active$num<-1

# order by year
active<-active[order(active$year),]

# aggregate by year
A<-aggregate(active$num~active$agent+active$year, FUN=sum)
colnames(A)<-c("agent","year","num")

# long to wide
A<-reshape(A, idvar = "agent", timevar = "year", direction = "wide")
# change column and row name
colnames(A)[2:13]<-2001:2012
rownames(A)<-A[,1]
A<-A[,-1]

# NA to 0
A[is.na(A)]<-0

# year span
year<-2001:2012

# using 2-year def to create a list of exiting agents
exit<-list()
for(i in 1:(length(year)-2))
{
  temp<-A[A[,i+1]==0,]
  temp<-temp[temp[,i+2]==0,]
  temp<-temp[temp[,i]!=0,]
  exit[[i]]<-rownames(temp)
  print(i)
}

# the exiters are prematurely deceased inventors from 2001 to 2010
# save it
save(exit, file ="exit.RData")

# in the following we calculate a transaction dataset

# a dataframe with self-transactions in the same year
X1<-X[X$lagent_id==X$bagent_id,]
X1<-X1[X1$list_year==X1$close_year,]

# a dataframe without self-transactions in the same year
X2<-X[!(rownames(X) %in% rownames(X1)),]

# form the transaction dataset
Y1<-data.frame(X2$lagent_id, X2$list_year)
colnames(Y1)<-c("agent","year")
Y2<-data.frame(X2$bagent_id, X2$close_year)
colnames(Y2)<-c("agent","year")
# add self-transaction in the same year
X1<-data.frame(X1$lagent_id, X1$list_year)
colnames(X1)<-c("agent","year")
# merge
active<-rbind(Y1,Y2,X1)
# clean
active<-active[active$agent!="",]
active<-active[!is.na(active$agent),]
active<-active[!is.na(active$year),]
active<-active[active$year!="",]

# order by year
active<-active[order(active$year),]

# excluding post-2013
trans<-active[active$year<=2012,]

# addin a number
trans$num<-1

# aggregate by year
A<-aggregate(trans$num~trans$agent+trans$year, FUN=sum)
colnames(A)<-c("agent","year","num")

# save A which is the full transaction data
write.csv(A,"/Users/terrywu/Dropbox/MLS Network/Placebo/transaction_pre2013_full.csv",row.names = F)

# we then calculate the number of listings by agent
A<-data.frame(X$lagent_id, X$list_year)
colnames(A)<-c("agent","list_year")

# clean
A<-A[A$agent!="",]
A<-A[!is.na(A$agent),]
A<-A[A$list_year!="",]
A<-A[!is.na(A$list_year),]

# aggregate 
A$num<-1
A<-aggregate(A$num~A$agent+A$list_year, FUN=sum)
colnames(A)<-c("agent","year","num_listing")

# augment it to create a balanced dataset
df_b<-data.frame(year = rep(unique(A$year), length(unique(A$agent))),
                 agent = rep(unique(A$agent), each = length(unique(A$year))))
A<-left_join(df_b, A)

# NA to 0 
A[is.na(A)]<-0
A<-A[A$year<=2012,]

# cumulative listings
A$csum <- ave(A$num_listing, A$agent, FUN=cumsum)

# save
write.csv(A,"/Users/terrywu/Dropbox/MLS Network/Placebo/listings_pre2013_full.csv",row.names = F)

# then we create a dataframe with zip codes and year
A<-data.frame(X$lagent_id, X$list_year, X$lagent_office_zip)
colnames(A)<-c("agent","year","zip")
B<-data.frame(X$bagent_id, X$close_year, X$bagent_office_zip)
colnames(B)<-c("agent","year","zip")
# merge
A<-rbind(A, B)

# clean
A<-A[A$agent!="",]
A<-A[A$year!="",]
A<-A[A$zip!="",]
A<-A[!is.na(A$zip),]
A<-unique(A[,1:3])

# extract 4 and 3 digits
A$zip3<-substr(A$zip, 1,3)
A$zip4<-substr(A$zip, 1,4)

# save the dataframe
write.csv(A,"/Users/terrywu/Dropbox/MLS Network/Placebo/office_zip.csv",row.names = F)

# clean the environment
rm(list=ls())

# read all relavant datasets
setwd("/Users/terrywu/Dropbox/MLS Network/Placebo")
load("exit.RData")
listings<-read.csv("listings_pre2013_full.csv",stringsAsFactors = F)
office<-read.csv("office_zip.csv",stringsAsFactors = F)
trans<-read.csv("transaction_pre2013_full.csv",stringsAsFactors = F)

# create a dataframe s.t.
# agent, year, cum_listings, zip3, zip4
A<-listings[,-3]
B<-office[,-3]
# merge
df<-merge(A, B, by=c("agent","year"))

# we then match placebo deceased inventors with three measures
placebo<-list()

# loop through years
for(i in 1:length(exit))
{
# exiters
A<-exit[[i]]
# their cum listings in that year
temp<-df[df$year==2000+i,]
temp<-temp[temp$agent %in% A,]
temp<-temp[,c(3,5)]
# temp is the exiter's zip and cum listings

# then match
# first, extract the year and non-exiters
temp2<-df[df$year==2000+i,]
temp2<-temp2[!(temp2$agent %in% A),]
temp2<-temp2[,-c(2,4)]

# match by two conditions: same cum and same zip4
m<-merge(temp2,temp,by = c("csum", "zip4"))

# extract the agents
m<-unique(m$agent)

# add in
placebo[[i]]<-m

# print the process
print(i)
}

# save the placebo
save(placebo, file ="placebo.RData")

# remove the environment
rm(list=ls())

# read the dataframe
setwd("/Users/terrywu/Dropbox/MLS Network")
load("Michigan_Full.RData")

# removed all "999999"
X<-X[X$lagent_id!="999999",]
X<-X[X$bagent_id!="999999",]

# form a network dataframe
A<-data.frame(X$lagent_id, X$bagent_id, X$close_year)
colnames(A)<-c("agent1","agent2","year")

# clean
A<-A[A$agent2!="",]
A<-A[A$year!="",]
A<-unique(A[,1:3])
A$agent1<-as.character(A$agent1)
A$agent2<-as.character(A$agent2)
A<-A[A$agent1!=A$agent2,]

# then A is a network dataframe
write.csv(A,"/Users/terrywu/Dropbox/MLS Network/Placebo/net.csv",row.names = F)

# read exiter dataset
load("/Users/terrywu/Dropbox/MLS Network/Placebo/exit.RData")

# real survivor, connected to exiters ever the years before
real<-list()

# loop through year
for(i in 1:length(exit))
{
# the year network
temp<-A[A$year<=2000+i,]
temp<-temp[,1:2]
# form a network
temp1<-temp[temp$agent1 %in% exit[[i]],]
temp2<-temp[temp$agent2 %in% exit[[i]],]
colnames(temp2)<-c("agent2","agent1")
# merge
temp<-rbind(temp1,temp2)
# remove duplicates
temp<-unique(temp[,1:2])
temp<-temp[!(temp$agent2 %in% exit[[i]]),]
temp<-unique(temp[,1:2])

# the degree of those non-exiters
temp$num<-1
temp<-aggregate(temp$num~temp$agent2, FUN=sum)
colnames(temp)<-c("agent","deg")
# extract those degree 1
temp<-temp[temp$deg==1,]
agent<-temp$agent

# save
real[[i]]<-unique(agent)

# print the process
print(i)
}

# save
save(real, file ="/Users/terrywu/Dropbox/MLS Network/Placebo/real_survivor.RData")


# read placebo datasets
load("/Users/terrywu/Dropbox/MLS Network/Placebo/placebo.RData")

# placebo survivor inventors datasets
placebo_survivor<-list()

# loop through year
for(i in 1:length(placebo))
{
# the year network
temp<-A[A$year<=2000+i,]
temp<-temp[,1:2]
# form a network
temp1<-temp[temp$agent1 %in% placebo[[i]],]
temp2<-temp[temp$agent2 %in% placebo[[i]],]
colnames(temp2)<-c("agent2","agent1")
# merge
temp<-rbind(temp1,temp2)
# remove duplicates
temp<-unique(temp[,1:2])
temp<-temp[!(temp$agent2 %in% placebo[[i]]),]
temp<-unique(temp[,1:2])
  
# the degree of those non-exiters
temp$num<-1
temp<-aggregate(temp$num~temp$agent2, FUN=sum)
colnames(temp)<-c("agent","deg")
# extract those degree 1
temp<-temp[temp$deg==1,]
agent<-temp$agent
  
# save
placebo_survivor[[i]]<-unique(agent)
  
# print the process
print(i)
}

# save
save(placebo_survivor, file ="/Users/terrywu/Dropbox/MLS Network/Placebo/placebo_survivor.RData")
