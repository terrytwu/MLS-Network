
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


# this script calculates three datasets: transaction, prob of sales and prob of sales within 1%
# s.t. removing all relavant outcomes w.r.t to exiters

# read agent data: exiters, treatment and control agents
setwd("/Users/terrywu/Dropbox/MLS Network/Exiter Analysis 2")
load("agent.Rdata")

# set the working directory and read dataset
setwd("/Users/terrywu/Dropbox/MLS Network")
load("Michigan_Full.RData")

# removed all "999999"
X<-X[X$lagent_id!="999999",]
X<-X[X$bagent_id!="999999",]

# we first calculate the transactions per agent-year

# year span
year<-c(X$list_year, X$close_year)
year<-unique(year)
year<-year[!is.na(year)]
year<-year[order(year)]

# create a data frame such that the first column is agent id
# the second column is year
# the third column is num of transactions
trans<-data.frame(NA, NA, NA)
colnames(trans)<-c("Agent_ID","Year","Num_Transactions")

# add a number for calculating
X$Num<-1

# year to 2001 to 2010
year<-year[year<=2010]

# loop through years
for(i in 1:length(year))
{
  # as a listing agent
  temp1<-X[X$list_year==year[i],]
  temp1<-temp1[!is.na(temp1$lagent_id),]
  temp1<-temp1[temp1$lagent_id!="",]
  # excluding self-trans
  temp1<-temp1[temp1$lagent_id!=temp1$bagent_id,]
  # excluding exiters
  temp1<-temp1[!(temp1$bagent_id %in% exit[[i]]),]
  
  if(dim(temp1)[1]!=0)
  {
    A<-aggregate(temp1$Num~temp1$lagent_id+temp1$list_year, FUN=sum)
  }
  if(dim(temp1)[1]==0)
  {
    A<-data.frame(NA, NA, NA)
  }
  colnames(A)<-c("Agent_ID","Year","Num_Transactions")
  
  # as a buying agent
  temp1<-X[X$close_year==year[i],]
  temp1<-temp1[!is.na(temp1$bagent_id),]
  temp1<-temp1[temp1$bagent_id!="",]
  # excluding self-trans
  temp1<-temp1[temp1$lagent_id!=temp1$bagent_id,]
  # excluding exiters
  temp1<-temp1[!(temp1$lagent_id %in% exit[[i]]),]
  
  if(dim(temp1)[1]!=0)
  {
    B<-aggregate(temp1$Num~temp1$bagent_id+temp1$close_year, FUN=sum)
  }
  if(dim(temp1)[1]==0)
  {
    B<-data.frame(NA, NA, NA)
  }
  colnames(B)<-c("Agent_ID","Year","Num_Transactions")
  
  # merge A and B
  AB<-rbind(A,B)
  # aggregate 
  AB<-aggregate(AB$Num_Transactions~AB$Agent_ID+AB$Year, FUN=sum)
  colnames(AB)<-c("Agent_ID","Year","Num_Transactions")
  
  # calculate the self-transactions in that year
  temp1<-X[X$list_year==year[i],]
  temp1<-temp1[temp1$close_year==year[i],]
  temp1<-temp1[temp1$lagent_id==temp1$bagent_id,]
  temp1<-temp1[temp1$lagent_id!="",]
  # excluding exiters
  temp1<-temp1[!(temp1$bagent_id %in% exit[[i]]),]
  
  if(dim(temp1)[1]!=0)
  {
    C<-aggregate(temp1$Num~temp1$lagent_id+temp1$list_year, FUN=sum)
    colnames(C)<-c("Agent_ID","Year","Num_Transactions")
  }
  
  # combine
  AB<-rbind(AB, C)
  # aggregate 
  AB<-aggregate(AB$Num_Transactions~AB$Agent_ID+AB$Year, FUN=sum)
  colnames(AB)<-c("Agent_ID","Year","Num_Transactions")
  
  # into the main dataset
  trans<-rbind(trans, AB)
  
  # print the process
  print(year[i])
}

# clean trans data frame
trans<-trans[!is.na(trans$Agent_ID),]

# save this dataset
write.csv(trans,"/Users/terrywu/Dropbox/MLS Network/Exiter Analysis 2/Transaction_by_Agent_Year_Noexiters.csv",row.names = F)

# then probability of sales
prob_sale<-data.frame(NA, NA, NA)
colnames(prob_sale)<-c("agent_id","year","prob")

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
  prob_sale<-rbind(prob_sale, temp1)
  # print the progress
  print(i)
}
prob_sale<-prob_sale[-1,]


# save this dataset
write.csv(prob_sale,"/Users/terrywu/Dropbox/MLS Network/Exiter Analysis 2/Prob_Sale_Noexiters.csv",row.names = F)


# then probability of sales within 1% price
prob_sale<-data.frame(NA, NA, NA)
colnames(prob_sale)<-c("agent_id","year","prob")

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
  prob_sale<-rbind(prob_sale, temp1)
  # print the progress
  print(i)
}
prob_sale<-prob_sale[-1,]

# save this dataset
write.csv(prob_sale,"/Users/terrywu/Dropbox/MLS Network/Exiter Analysis 2/Prob_Sale1_Noexiters.csv",row.names = F)

