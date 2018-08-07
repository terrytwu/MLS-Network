
library(readstata13)
library(foreign)

# Construct a Balanced Panel Dataset from Chula Vista
# Aug 7, 2018

# set the working directory and read data
setwd("/Users/terrywu/Dropbox/MLS Network")
X<-read.dta13("SD9191_market_full_w_deeds_prepped.dta")

# extract all agent id
agent<-c(X$bagent_id, X$lagent_id)
agent<-unique(agent)
agent<-agent[agent!=""]

# create a dataset in which the first column is agent id
# the second column is agent name and the third column is office name
agent_id_name<-data.frame(agent,rep(NA, length(agent)),rep(NA, length(agent)))
for(i in 1:length(agent))
{
temp<-X[X$lagent_id==agent[i],]
agent_id_name[i,2]<-temp$lagent_name[1]
agent_id_name[i,3]<-temp$lagent_office_name[1]
# if the office name is missing
# we try to find it as a bagent
if(is.na(agent_id_name[i,3]))
{
temp<-X[X$bagent_id==agent[i],]
agent_id_name[i,3]<-temp$bagent_office_name[1]
}
print(i)
}
colnames(agent_id_name)<-c("agent_id","agent_name","office_name")

# in order to consturct a balanced dataset
# we need to know the time span
year<-c(X$list_year, X$close_year)
year<-unique(year)
year<-sort(year)

# we need 6 variables

# create a empty dataset
mydata<-data.frame(t(rep(1,6)))
colnames(mydata)<-c("agent_name","agent_id","num_closed_transaction","num_listed_transaction","year","office_name")
mydata<-mydata[-1,]

# loop for create a balanced panel dataset
# for each year
for(j in 1:length(year))
{
temp<-matrix(NA, nrow=length(agent),ncol = 6)
temp<-as.data.frame(temp)
temp[,c(1,2,6)]<-agent_id_name
temp[,5]<-rep(year[j],length(agent))
# for each agent, we need a loop to extract info needed
for(i in 1:length(agent))
{
# num_closed_transaction
X_subset<-X[X$close_year==year[j],] 
X_subset<-X_subset[!is.na(X_subset$close_year),]
X_subset<-X_subset[X_subset$bagent_id==agent[i],]
temp[i,3]<-0
  if(dim(X_subset)[1]>0)
  {
  temp[i,3]<-dim(X_subset)[1]
  }
# num_listed_transaction
X_subset<-X[X$list_year==year[j],] 
X_subset<-X_subset[!is.na(X_subset$list_year),]
X_subset<-X_subset[X_subset$lagent_id==agent[i],]
temp[i,4]<-0
if(dim(X_subset)[1]>0)
{
  temp[i,4]<-dim(X_subset)[1]
}
print(i)
}
# vertically combine
mydata<-rbind(mydata,temp)
print(j)
}

# save the dataset
colnames(mydata)<-c("agent_name","agent_id","num_closed_transaction","num_listed_transaction","year","office_name")
write.csv(mydata,"/Users/terrywu/Dropbox/MLS Network/MLS_Panel.csv", row.names = F)
