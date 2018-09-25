
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
# for answering Paul's email on Sep 10
# First Version: Sep 15th
# This Version: Sep 19th

# set the working directory and read data
setwd("/Users/terrywu/Dropbox/MLS Network")
load("Michigan_Full.RData")

# remove "999999"
X<-X[X$bagent_id!="999999",]
X<-X[X$lagent_id!="999999",]


# we first calculate the transactions per agent-year

# year span
year<-c(X$list_year, X$close_year)
year<-unique(year)
year<-year[!is.na(year)]

# create a data frame such that the first column is agent id
# the second column is year
# the third column is num of transactions
trans<-data.frame(NA, NA, NA)
colnames(trans)<-c("Agent_ID","Year","Num_Transactions")

# add a number for calculating
X$Num<-1
year<-year[order(year)]

# a loop
for(i in 1:length(year))
{
# as a listing agent
temp1<-X[X$list_year==year[i],]
temp1<-temp1[!is.na(temp1$lagent_id),]
A<-data.frame(temp1$lagent_id, temp1$list_year, temp1$Num)
colnames(A)<-c("Agent_ID","Year","Num_Transactions")
# as a buying agent
temp1<-X[X$close_year==year[i],]
temp1<-temp1[!is.na(temp1$bagent_id),]
B<-data.frame(temp1$bagent_id, temp1$close_year, temp1$Num)
colnames(B)<-c("Agent_ID","Year","Num_Transactions")
# merge
A<-rbind(A,B)
# aggregate
if(dim(A)[1]!=0)
{
A<-aggregate(A$Num_Transactions~A$Agent_ID+A$Year,FUN=sum)
}
colnames(A)<-c("Agent_ID","Year","Num_Transactions")
# calculate the double counted transactions
temp1<-X[X$lagent_id==X$bagent_id,]
temp1<-temp1[temp1$list_year==year[i],]
temp1<-temp1[temp1$close_year==year[i],]
temp1<-temp1[!is.na(temp1$lagent_id),]
# substract
if(dim(temp1)[1]!=0)
{
temp1<-aggregate(temp1$Num~temp1$lagent_id+temp1$list_year,FUN=sum)
id<-fmatch(temp1$`temp1$lagent_id`,A$Agent_ID)
A[id,3]<-A[id,3]-temp1[,3]
}
# merge
trans<-rbind(trans, A)
print(i)
}

# clean trans data frame
trans<-trans[!is.na(trans$Agent_ID),]
trans<-trans[!(trans$Agent_ID==""),]

# save this dataset
# write.csv(trans,"/Users/terrywu/Dropbox/MLS Network/Superstar Task/Transaction_by_Agent_Year.csv",row.names = F)

# calculate percentiles and form a superstar dataset by year
super_star_trans<-data.frame(NA,NA,NA)
colnames(super_star_trans)<-colnames(trans)
for(i in 1:length(year))
{
# percentiles
cutoff<-as.numeric(quantile(trans[trans$Year==2000+i,3], .95))
temp<-trans[trans$Year==2000+i,]
temp<-temp[temp$Num_Transactions>cutoff,]
super_star_trans<-rbind(super_star_trans,temp)
print(i)
}
super_star_trans<-super_star_trans[-1,]

# Question 1
# if you are a superstar in a given year, how likely are you 
# to be a superstar in the next year? 

# calculate the percentage
super_year<-super_star_trans$Year
super_year<-unique(super_year)
super_year<-super_year[order(super_year)]
# a loop
perct<-rep(NA, length(super_year)-1)
for(i in 1:length(perct))
{
A<-super_star_trans[super_star_trans$Year==super_year[i],]
B<-super_star_trans[super_star_trans$Year==super_year[i+1],]
perct[i]<-length(which(A[,1] %in% B[,1]))/length(A[,1])
print(i)
}
# plot
mydata<-data.frame(perct,super_year[-14])
colnames(mydata)<-c("X","Year")
ggplot(mydata, aes(x=Year,y=X)) + 
  geom_line(size=3)+
  theme_bw()+
  ylab("Percentage")+
  xlab("Year")+
  scale_x_discrete(limits=super_year[-14])+
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Question 2
# what is the distribution of superstars exiting across time periods? 

# calculate active dataset
# based on trans
active<-acast(trans, Agent_ID~Year, value.var="Num_Transactions")
active[is.na(active)]<-0

# 2-year
# create a list of exiting agents
exit<-list()
for(i in 1:(length(super_year)-1))
{
temp<-active[active[,i+1]==0,]
temp<-temp[temp[,i+2]==0,]
temp<-temp[temp[,i]!=0,]
exit[[i]]<-rownames(temp)
print(i)
}

# then we have a list of agents who exit next year
# calculateh percentage
pert<-rep(NA, length(exit))
for(i in 1:length(exit))
{
temp<-super_star_trans[super_star_trans$Year==2001+i-1,] 
pert[i]<-length(which(temp[,1] %in% exit[[i]]))/length(temp[,1])
print(i)
}

# plot
mydata<-data.frame(pert,2001:2013)
colnames(mydata)<-c("X","Year")
ggplot(mydata, aes(x=Year,y=X)) + 
  geom_line(size=3)+
  theme_bw()+
  ylab("Percentage")+
  xlab("Year")+
  scale_x_discrete(limits=super_year[-14])+
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 1-year
# create a list of exiting agents
exit<-list()
for(i in 1:(length(super_year)))
{
  temp<-active[active[,i+1]==0,]
  temp<-temp[temp[,i]!=0,]
  exit[[i]]<-rownames(temp)
  print(i)
}

# then we have a list of agents who exit next year
# calculateh percentage
pert<-rep(NA, length(exit))
for(i in 1:length(exit))
{
  temp<-super_star_trans[super_star_trans$Year==2001+i-1,] 
  pert[i]<-length(which(temp[,1] %in% exit[[i]]))/length(temp[,1])
  print(i)
}

# plot
mydata<-data.frame(pert[-14],2001:2013)
colnames(mydata)<-c("X","Year")
ggplot(mydata, aes(x=Year,y=X)) + 
  geom_line(size=3)+
  theme_bw()+
  ylab("Percentage")+
  xlab("Year")+
  scale_x_discrete(limits=super_year[-14])+
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# 3-year
# create a list of exiting agents
exit<-list()
for(i in 1:(length(super_year)-2))
{
  temp<-active[active[,i+1]==0,]
  temp<-temp[temp[,i+1]==0,]
  temp<-temp[temp[,i+2]==0,]
  temp<-temp[temp[,i]!=0,]
  exit[[i]]<-rownames(temp)
  print(i)
}

# then we have a list of agents who exit next year
# calculateh percentage
pert<-rep(NA, length(exit))
for(i in 1:length(exit))
{
  temp<-super_star_trans[super_star_trans$Year==2001+i-1,] 
  pert[i]<-length(which(temp[,1] %in% exit[[i]]))/length(temp[,1])
  print(i)
}

# plot
mydata<-data.frame(pert,2001:2012)
colnames(mydata)<-c("X","Year")
ggplot(mydata, aes(x=Year,y=X)) + 
  geom_line(size=3)+
  theme_bw()+
  ylab("Percentage")+
  xlab("Year")+
  scale_x_discrete(limits=super_year[-c(13,14)])+
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Question 3
# the key question that we want to know is whether the exits appear “random.” 
# If we take the sample of superstars, and match each superstar that exits in period t 
# to a superstar that doesn’t in period t, can we compare their average characteristics in the previous periods? 
# do they also look similar in the years before?

# we focus on the 2-year def
# for the super_star dataset, mark each superstar as exit or not
# 2-year

# create a list of exiting agents
exit<-list()
for(i in 1:(length(super_year)-1))
{
  temp<-active[active[,i+1]==0,]
  temp<-temp[temp[,i+2]==0,]
  temp<-temp[temp[,i]!=0,]
  exit[[i]]<-rownames(temp)
  print(i)
}

# mark
super_star_trans$Exit_Flag<-0
for(i in 1:length(exit))
{
temp<-super_star_trans[super_star_trans$Year==2001+i-1,] 
temp[which(temp[,1] %in% exit[[i]]),4]<-1
super_star_trans[super_star_trans$Year==2001+i-1,4]<-temp[,4]
print(i)
}

# then we separate two datasets
# such that one is who exit, the other is not
super_star_exit<-super_star_trans[super_star_trans$Exit_Flag==1,]
super_star_not_exit<-super_star_trans[super_star_trans$Exit_Flag!=1,]

# first of all, num of transactions

# define a function to summarize
my<-function(X)
{
result<-rep(NA, 6)
result[1]<-mean(X)
result[2]<-sd(X)
result[3]<-min(X)
result[4]<-max(X)
result[5]<-quantile(X, .95)
result[6]<-quantile(X, .05)
return(result)
}

# exit year
exit_year<-super_star_exit$Year
table(exit_year)

# then we plot the average transactions each year
# two groups, when is for superstar who exit, the other is for those who don't quit
A<-aggregate(super_star_trans$Num_Transactions~super_star_trans$Year+super_star_trans$Exit_Flag, FUN=mean)
# rename
colnames(A)<-c("Year","Group","Mean_Transaction")
A$Group[A$Group==1]<-"Exit"
A$Group[A$Group==0]<-"Not Exit"
A$Group<-as.factor(as.character(A$Group))
# plot
ggplot(A, aes(x=Year,y=Mean_Transaction, group=Group, color=Group))+
  geom_line(size=3)+
  theme_bw()+
  ylab("Transaction Mean")+
  xlab("Year")+
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  scale_x_discrete(limits=c(2001:2014))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.position="bottom")+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=25))

# calculate the degree of each agent by year
D<-data.frame(X$lagent_id, X$bagent_id, X$list_year, X$close_year)
colnames(D)<-c("listing_agent","buying_agent","list_year","sale_year")
D<-D[D$buying_agent!="",] 
D<-D[!is.na(D$sale_year),]
# exclude self-loop
D$listing_agent<-as.character(D$listing_agent)
D$buying_agent<-as.character(D$buying_agent)
D<-D[D$listing_agent!=D$buying_agent,]

# creat a dregee dataset
degree<-data.frame(NA, NA,NA)
colnames(degree)<-c("agent","num","year")
# year span
year<-unique(c(D$list_year, D$sale_year))
# a loop
for(i in 1:length(year))
{
# as a listing agent
temp1<-D[D$list_year==year[i],][,1:2]
colnames(temp1)<-c("agent1","agent2")
# as a buying agent
temp2<-D[D$sale_year==year[i],][,1:2]
colnames(temp2)<-c("agent2","agent1")
# merge
A<-rbind(temp1, temp2)
A<-unique(A[,1:2])
# calculate 
A$Num<-1
A<-aggregate(A$Num~A$agent1, FUN=sum)
A$year<-year[i]
colnames(A)<-c("agent","num","year")
# merge
degree<-rbind(degree, A)
print(i)
}
degree<-degree[-1,]
  
# for the degree dataset
# for each year, we include only the superstar
# and mark whether it is an exiting superstar

# a new dataset
degree2<-data.frame(NA, NA, NA, NA)
colnames(degree2)<-c("agent","num","year","exit_flag")
for(i in unique(super_star_trans$Year))
{
# in degree
temp1<-degree[degree$year==i,] 
# in super star
temp2<-super_star_trans[super_star_trans$Year==i,]
temp1<-temp1[temp1$agent %in% temp2$Agent_ID,]
# add temp1 a exit flag
temp1$exit_flag<-0
# match exti_flag
id<-fmatch(temp1$agent, temp2$Agent_ID)
temp1$exit_flag<-temp2[id,4]
# merge
degree2<-rbind(degree2,temp1)
print(i)
}

# then we calculate the average

# two groups, when is for superstar who exit, the other is for those who don't quit
A<-aggregate(degree2$num~degree2$year+degree2$exit_flag, FUN=mean)
# rename
colnames(A)<-c("Year","Group","Mean_Degree")
A$Group[A$Group==1]<-"Exit"
A$Group[A$Group==0]<-"Not Exit"
A$Group<-as.factor(as.character(A$Group))
# plot
ggplot(A, aes(x=Year,y=Mean_Degree, group=Group, color=Group))+
  geom_line(size=3)+
  theme_bw()+
  ylab("Mean Degree")+
  xlab("Year")+
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  scale_x_discrete(limits=c(2001:2014))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.position="bottom")+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=25))
  

# the above analysis focuses on a single year
# we then pool all years before the exiting year

# a data frame
my<-data.frame(NA, NA, NA)
colnames(my)<-c("Group","Mean_Transaction","Year")
# loop
for(i in unique(super_star_trans$Year))
{
# the exiting agent in that year
temp1<-super_star_trans[super_star_trans$Year==i,]
temp1<-temp1[temp1$Exit_Flag==1,]
# all transaction data before that year
temp2<-super_star_trans[super_star_trans$Year<=i,]
# mark temp2 as the exiting agent we should focus
temp2$Exit_Flag<-0
temp2$Exit_Flag[temp2$Agent_ID %in% temp1$Agent_ID]<-1
# calulate the average
A<-aggregate(temp2$Num_Transactions~temp2$Exit_Flag, FUN=mean)
colnames(A)<-c("Group","Mean_Transaction")
# add the mean data a year
A$Year<-i
# merge
my<-rbind(my, A)
# print
print(i)
}
my<-my[-1,]
# rename
A<-my
colnames(A)<-c("Group","Mean_Transaction","Year")
A$Group[A$Group==1]<-"Exit"
A$Group[A$Group==0]<-"Not Exit"
A$Group<-as.factor(as.character(A$Group))
# plot
ggplot(A, aes(x=Year,y=Mean_Transaction, group=Group, color=Group))+
  geom_line(size=3)+
  theme_bw()+
  ylab("Mean Transaction")+
  xlab("Year")+
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  scale_x_discrete(limits=c(2001:2014))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.position="bottom")+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=25))

# similar plot for degree
# a data frame
my<-data.frame(NA, NA, NA)
colnames(my)<-c("Group","Mean_Degree","Year")
# loop
for(i in unique(super_star_trans$Year))
{
# the exiting agent in that year
temp1<-super_star_trans[super_star_trans$Year==i,]
# all degree data before that year
temp2<-degree[degree$year<=i,]
# subset temp2 to superstars
temp2<-temp2[temp2$agent %in% temp1$Agent_ID,]
# mark temp2 as the exiting agent we should focus
temp2$Exit_Flag<-0
temp1<-temp1[temp1$Exit_Flag==1,]
temp2$Exit_Flag[temp2$agent %in% temp1$Agent_ID]<-1
# calulate the average
A<-aggregate(temp2$num~temp2$Exit_Flag, FUN=mean)
colnames(A)<-c("Group","Mean_Degree")
# add the mean data a year
A$Year<-i
# merge
my<-rbind(my, A)
# print
print(i)
}
my<-my[-1,]
# rename
A<-my
colnames(A)<-c("Group","Mean_Degree","Year")
A$Group[A$Group==1]<-"Exit"
A$Group[A$Group==0]<-"Not Exit"
A$Group<-as.factor(as.character(A$Group))
# plot
ggplot(A, aes(x=Year,y=Mean_Degree, group=Group, color=Group))+
  geom_line(size=3)+
  theme_bw()+
  ylab("Mean Degree")+
  xlab("Year")+
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  scale_x_discrete(limits=c(2001:2014))+
  scale_y_discrete(limits=seq(10,130,by=10))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.position="bottom")+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=25))



# Added on Sept 18
# then we do two similar graphs for agents
# Can you do a version of this comparison that matches 
# on number of transactions at time of exit? 
# E.g. for each agent in the treatment group, 
# pick them a matched control agent that has the 
# same number of transactions as the treatment agent 
#cin the year before exit for the treatment agent.

# for each exit agent, pick a matched agent
super_star_exit<-super_star_exit[order(super_star_exit$Year),]

# create a dataset to record those matched agents
control<-data.frame(NA, NA, NA, NA)
colnames(control)<-c(colnames(super_star_trans)[1:3],"Match_ID")
for(i in 1:length(super_star_exit$Agent_ID))
{
y<-super_star_exit$Year[i]
trs<-super_star_exit$Num_Transactions[i]
temp<-super_star_trans[super_star_trans$Year==y & super_star_trans$Num_Transactions==trs,]
temp<-temp[temp$Exit_Flag!=1,]
if(dim(temp)[1]!=0)
{
  temp$Match_ID<-i
control<-rbind(control, temp[,c(1:3,5)])
}
print(i)
}
control<-control[-1,]

# then we calculate a difference
# for each match, calculate the mean difference of transactions in each year
# then calculate the average of all matches in each year

# a dataset to record all the difference in mean
mean_diff<-data.frame(NA, NA,NA)
colnames(mean_diff)<-c("year","Diff","event_time")

# a loop
for(i in 1:length(super_star_exit$Agent_ID))
{
A<-super_star_exit$Agent_ID[i]
yr<-super_star_exit$Year[i]
B<-control[control$Match_ID==i,1]
if(length(B)!=0)
{
# subset of transaction data to include all years before
temp<-super_star_trans[super_star_trans$Year<=yr,]
temp<-temp[temp$Agent_ID %in% B,]
# aggregate the mean of control group
M<-aggregate(temp$Num_Transactions~temp$Year, FUN=mean)
colnames(M)<-c("year","num_trans")
# aggregate the mean of treatment agent
A<-super_star_trans[super_star_trans$Agent_ID==A,]
A<-A[A$Year<=yr,]
A<-A[,c(2,3)]
colnames(A)<-c("year","num_trans")
# so M is the mean by year of control group
# and A is the mean by year of treatment group
# for comparsion purpose, include only the mean
lap_year<-intersect(A$year, M$year)
A<-A[A$year %in% lap_year,]
M<-M[M$year %in% lap_year,]
# rank A and M by year
A<-A[order(A$year),]
M<-M[order(M$year),]
# the difference
M$Diff<-A[,2]-M[,2]
# remove the year that was matched
M<-M[M$year!=yr,]
M<-M[,c(1,3)]
# add event time
M$event_time<-M$year-yr
# merge
mean_diff<-rbind(mean_diff,M)
}
print(i)
}
mean_diff<-mean_diff[-1,]

# aggregate by year
A<-aggregate(mean_diff$Diff~mean_diff$event_time, FUN=mean)
colnames(A)<-c("yr","diff")

# plot
ggplot(A, aes(x=yr,y=diff))+
  geom_line(size=3)+
  theme_bw()+
  ylab("Diff Mean")+
  xlab("Year")+
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  scale_x_discrete(limits=c(-12:0))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.position="bottom")+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=25))

# then simiarly for degree

# a dataset to record all the difference in mean
mean_diff<-data.frame(NA, NA)
colnames(mean_diff)<-c("year","Diff")

# a loop
for(i in 1:length(super_star_exit$Agent_ID))
{
A<-super_star_exit$Agent_ID[i]
yr<-super_star_exit$Year[i]
B<-control[control$Match_ID==i,1]
if(length(B)!=0)
{
    # subset of transaction data to include all years before
    temp<-degree[degree$year<=yr,]
    temp<-temp[temp$agent %in% B,]
    # aggregate the mean of control group
    M<-aggregate(temp$num~temp$year, FUN=mean)
    colnames(M)<-c("year","num_trans")
    # aggregate the mean of treatment agent
    A<-degree[degree$agent==A,]
    A<-A[A$year<=yr,]
    A<-A[,c(3,2)]
    colnames(A)<-c("year","num_trans")
    # so M is the mean by year of control group
    # and A is the mean by year of treatment group
    # for comparsion purpose, include only the mean
    lap_year<-intersect(A$year, M$year)
    A<-A[A$year %in% lap_year,]
    M<-M[M$year %in% lap_year,]
    # rank A and M by year
    A<-A[order(A$year),]
    M<-M[order(M$year),]
    # the difference
    M$Diff<-A[,2]-M[,2]
    M<-M[,c(1,3)]
    # merge
    mean_diff<-rbind(mean_diff,M)
  }
  print(i)
}
mean_diff<-mean_diff[-1,]

# aggregate by year
A<-aggregate(mean_diff$Diff~mean_diff$year, FUN=mean)
colnames(A)<-c("yr","diff")

# plot
ggplot(A, aes(x=yr,y=diff))+
  geom_line(size=3)+
  theme_bw()+
  ylab("Diff Mean")+
  xlab("Year")+
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  scale_x_discrete(limits=c(2001:2014))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.position="bottom")+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=25))



# to convert to event time

# a dataset to record all the difference and 
# mark them the event time
mean_diff<-data.frame(NA, NA,NA)
colnames(mean_diff)<-c("year","Diff","event_time")

# a loop
for(i in 1:length(super_star_exit$Agent_ID))
{
  A<-super_star_exit$Agent_ID[i]
  yr<-super_star_exit$Year[i]
  B<-control[control$Match_ID==i,1]
  if(length(B)!=0)
  {
    # subset of transaction data to include all years before
    temp<-degree[degree$year<=yr,]
    temp<-temp[temp$agent %in% B,]
    # aggregate the mean of control group
    M<-aggregate(temp$num~temp$year, FUN=mean)
    colnames(M)<-c("year","num_trans")
    # aggregate the mean of treatment agent
    A<-degree[degree$agent==A,]
    A<-A[A$year<=yr,]
    A<-A[,c(3,2)]
    colnames(A)<-c("year","num_trans")
    # so M is the mean by year of control group
    # and A is the mean by year of treatment group
    # for comparsion purpose, include only the mean
    lap_year<-intersect(A$year, M$year)
    A<-A[A$year %in% lap_year,]
    M<-M[M$year %in% lap_year,]
    # rank A and M by year
    A<-A[order(A$year),]
    M<-M[order(M$year),]
    # the difference
    M$Diff<-A[,2]-M[,2]
    M<-M[,c(1,3)]
    # add event time
    M$event_time<-M$year-yr
    # merge
    mean_diff<-rbind(mean_diff,M)
  }
  print(i)
}
mean_diff<-mean_diff[-1,]

# aggregate by year
A<-aggregate(mean_diff$Diff~mean_diff$event_time, FUN=mean)
colnames(A)<-c("Time","diff")

# plot
ggplot(A, aes(x=Time,y=diff))+
  geom_line(size=3)+
  theme_bw()+
  ylab("Mean of Degree Difference")+
  xlab("Year")+
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  scale_x_discrete(limits=c(-12:0))+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=25))


############################################################
# Added on Sept 19, Control and Treatment Analysis
############################################################

# super_star_exit is the treatment dataset
# control is the control dataset which contains the match_id
# such that each id is matched to a superstar who exit

# a dataset to record all the difference in mean of trasactions
# from -6 to +6
mean_diff<-data.frame(NA, NA,NA,NA, NA)
colnames(mean_diff)<-c("year","num_trans","event_time","Group","diff")

# a loop
for(i in 1:length(super_star_exit$Agent_ID))
{
A<-super_star_exit$Agent_ID[i]
yr<-super_star_exit$Year[i]
B<-control[control$Match_ID==i,1]
if(length(B)!=0)
{
   # subset of transaction data to include all years before
    temp<-super_star_trans[super_star_trans$Year<=(yr+6),]
    temp<-temp[temp$Agent_ID %in% B,]
    # aggregate the mean of control group
    M<-aggregate(temp$Num_Transactions~temp$Year, FUN=mean)
    colnames(M)<-c("year","num_trans")
    # aggregate the mean of treatment agent
    A<-super_star_trans[super_star_trans$Agent_ID==A,]
    A<-A[A$Year<=yr,]
    A<-A[,c(2,3)]
    colnames(A)<-c("year","num_trans")
    # add event time
    M$event_time<-M$year-yr
    A$event_time<-A$year-yr
    # add group identity
    M$Group<-"Control"
    A$Group<-"Treatment"
    # order by year
    A<-A[order(A$year),]
    M<-M[order(M$year),]
    # include only overlap year
    over_year<-intersect(A$year, M$year)
    A<-A[A$year %in% over_year,]
    # for the event time before t=0, include only overlap year
    M<-M[M$year %in% over_year | M$year>yr,]
    # match event time id
    id<-fmatch(A$event_time,M$event_time)
    # calculate the difference
    M$diff<-M$num_trans
    M$diff[id]<-A$num_trans-M$diff[id]
    # merge
    mean_diff<-rbind(mean_diff,M)
  }
  print(i)
}
mean_diff<-mean_diff[-1,]

# aggregate by event_time
A<-aggregate(mean_diff$diff~mean_diff$event_time, FUN=mean)
colnames(A)<-c("yr","diff")

# add group
A$Group<-"Control"
A$Group[which(A$yr<=0)]<-"Difference"
A<-A[A$yr>=-6,]

# plot
ggplot(A, aes(x=yr,y=diff,group=Group,color=Group))+
  geom_line(size=3)+
  theme_bw()+
  ylab("Mean Difference or Control Mean")+
  xlab("Year")+
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  scale_x_discrete(limits=c(-6:6))+
  theme(legend.position="bottom")+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=25))

# similar for degree

# a dataset to record all the difference in mean of degrees
# from -6 to +6
mean_diff<-data.frame(NA, NA,NA,NA, NA)
colnames(mean_diff)<-c("year","num_trans","event_time","Group","diff")

# a loop
for(i in 1:length(super_star_exit$Agent_ID))
{
  A<-super_star_exit$Agent_ID[i]
  yr<-super_star_exit$Year[i]
  B<-control[control$Match_ID==i,1]
  if(length(B)!=0)
  {
    # subset of transaction data to include all years before
    temp<-degree[degree$year<=(yr+6),]
    temp<-temp[temp$agent %in% B,]
    # aggregate the mean of control group
    M<-aggregate(temp$num~temp$year, FUN=mean)
    colnames(M)<-c("year","num_trans")
    # aggregate the mean of treatment agent
    A<-degree[degree$agent==A,]
    A<-A[A$year<=yr,]
    A<-A[,c(2,3)]
    colnames(A)<-c("num_trans","year")
    # add event time
    M$event_time<-M$year-yr
    A$event_time<-A$year-yr
    # add group identity
    M$Group<-"Control"
    A$Group<-"Treatment"
    # order by year
    A<-A[order(A$year),]
    M<-M[order(M$year),]
    # include only overlap year
    over_year<-intersect(A$year, M$year)
    A<-A[A$year %in% over_year,]
    # match event time id
    id<-fmatch(A$event_time,M$event_time)
    # calculate the difference
    M$diff<-M$num_trans
    M$diff[id]<-A$num_trans-M$diff[id]
    # merge
    mean_diff<-rbind(mean_diff,M)
  }
  print(i)
}
mean_diff<-mean_diff[-1,]

# aggregate by event_time
A<-aggregate(mean_diff$diff~mean_diff$event_time, FUN=mean)
colnames(A)<-c("yr","diff")

# add group
A$Group<-"Control"
A$Group[which(A$yr<=0)]<-"Difference"
A<-A[A$yr>=-6,]

# plot
ggplot(A, aes(x=yr,y=diff,group=Group,color=Group))+
  geom_line(size=3)+
  theme_bw()+
  ylab("Mean Difference or Control Mean")+
  xlab("Year")+
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  scale_x_discrete(limits=c(-6:6))+
  theme(legend.position="bottom")+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=25))

# then we plot lines for two different sets
# one for treatment and the other for control


# we have the control 
control<-unique(control[,1:3])

# then two sets
A<-aggregate(control$Num_Transactions~control$Year, FUN=mean)
B<-aggregate(super_star_trans$Num_Transactions~super_star_trans$Year, FUN = mean)

# plot them
mydf<-data.frame(c(A[,1],B[,1]),c(A[,2],B[,2]))
colnames(mydf)<-c("year","num")
mydf$group<-c(rep("Treatment",dim(A)[1]),rep("Control",dim(B)[1]))
# plot
ggplot(mydf, aes(x=year,y=num,group=group,color=group))+
  geom_line(size=3)+
  theme_bw()+
  ylab("Transaction Mean")+
  xlab("Year")+
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  scale_x_discrete(limits=c(2001:2014))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.position="bottom")+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=25))


# then for degree
A<-degree[degree$agent %in% super_star_exit$Agent_ID,]
colnames(A)<-c("Agent_ID","Num","Year")
B<-super_star_exit[,c(1:2)]
# merge
C<-merge(A, B,by=c("Agent_ID","Year"))
# mean
E<-aggregate(C$Num~C$Year, FUN=mean)
# rename
colnames(E)<-c("Year","Num")
E$group<-"Treatment"
# for control group
A<-degree[degree$agent %in% control$Agent_ID,]
colnames(A)<-c("Agent_ID","Num","Year")
B<-control[,c(1:2)]
# merge
C<-merge(A, B,by=c("Agent_ID","Year"))
# mean
C<-aggregate(C$Num~C$Year, FUN=mean)
# rename
colnames(C)<-c("Year","Num")
C$group<-"Control"

# plot them
mydf<-data.frame(c(E[,1],C[,1]),c(E[,2],C[,2]),c(E[,3],C[,3]))
colnames(mydf)<-c("year","num","group")

# plot
ggplot(mydf, aes(x=year,y=num,group=group,color=group))+
  geom_line(size=3)+
  theme_bw()+
  ylab("Degree Mean")+
  xlab("Year")+
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  scale_x_discrete(limits=c(2001:2014))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.position="bottom")+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=25))

# a new control set

# for each exit agent, pick a matched agent
super_star_exit<-super_star_exit[order(super_star_exit$Year),]

# create a dataset to record those matched agents
control<-data.frame(NA, NA, NA, NA)
colnames(control)<-c(colnames(super_star_trans)[1:3],"Match_ID")
for(i in 1:length(super_star_exit$Agent_ID))
{
  y<-super_star_exit$Year[i]
  trs<-super_star_exit$Num_Transactions[i]
  temp<-super_star_trans[super_star_trans$Year==y & super_star_trans$Num_Transactions==trs,]
  temp<-temp[temp$Exit_Flag!=1,]
  if(dim(temp)[1]!=0)
  {
    temp$Match_ID<-i
    control<-rbind(control, temp[,c(1:3,5)])
  }
  print(i)
}
control<-control[-1,]

# the for each year

# add a match id 
super_star_exit$line_id<-1:dim(super_star_exit)[1]

# then we calculate a difference
# for each match, calculate the mean difference of transactions in each year
# then calculate the average of all matches in each year

# exit year
exit_year<-unique(super_star_exit$Year)

# a loop
setwd("/Users/terrywu/Dropbox/MLS Network/Superstar Task")
for(i in exit_year)
{
  temp<-super_star_exit[super_star_exit$Year==i,]
  A<-temp$Agent_ID
  B<-control[control$Match_ID  %in% temp$line_id,]
  B<-B$Agent_ID
  if(length(B)!=0)
  {
    # subset of transaction data to include all years before
    temp2<-super_star_trans[super_star_trans$Year<=i,]
    temp2<-temp2[temp2$Agent_ID %in% A,]
    # aggregate the mean of control group
    M<-aggregate(temp2$Num_Transactions~temp2$Year, FUN=mean)
    colnames(M)<-c("year","num_trans")
    # aggregate the mean of treatment agent
    A<-super_star_trans[super_star_trans$Agent_ID %in% B,]
    A<-A[,c(2,3)]
    colnames(A)<-c("year","num_trans")
    # aggregate
    A<-aggregate(A$num_trans~A$year, FUN=mean)
    colnames(A)<-c("year","num_trans")
    # event time
    M$event_time<-M$year-i
    A$event_time<-A$year-i
    # add group id
    M$group<-"Control"
    A$group<-"Treatment"
    # to one single date frame
    df<-data.frame(c(A[,1],M[,1]),c(A[,2],M[,2]),c(A[,3],M[,3]),c(A[,4],M[,4]))
    colnames(df)<-c("yr","diff","event_time","group")
    # plot
    ggplot(df, aes(x=event_time,y=diff,group=group,color=group))+
      geom_line(size=3)+
      theme_bw()+
      ylab("Mean Transaction")+
      xlab("Year")+
      theme(axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15))+
      scale_x_discrete(limits=unique(df$event_time))+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme(legend.position="bottom")+
      theme(legend.title=element_blank())+
      theme(text = element_text(size=25))
    # save
    Name<-paste("plot_","year_",i,".pdf",sep="")
    ggsave(Name)
  }
  print(i)
}

# them similiar for degrees


# exit year
exit_year<-unique(super_star_exit$Year)

# a loop
for(i in exit_year)
{
  temp<-super_star_exit[super_star_exit$Year==i,]
  A<-temp$Agent_ID
  B<-control[control$Match_ID  %in% temp$line_id,]
  B<-B$Agent_ID
  if(length(B)!=0)
  {
    # subset of transaction data to include all years before
    temp2<-degree[degree$agent %in% A,]
    temp2<-temp2[temp2$year<=i,]
    # aggregate the mean of control group
    M<-aggregate(temp2$num~temp2$year, FUN=mean)
    colnames(M)<-c("year","num_trans")
    # aggregate the mean of treatment agent
    A<-degree[degree$agent %in% B,]
    A<-A[,c(3,2)]
    colnames(A)<-c("year","num_trans")
    # aggregate
    A<-aggregate(A$num_trans~A$year, FUN=mean)
    colnames(A)<-c("year","num_trans")
    # event time
    M$event_time<-M$year-i
    A$event_time<-A$year-i
    # add group id
    M$group<-"Control"
    A$group<-"Treatment"
    # to one single date frame
    df<-data.frame(c(A[,1],M[,1]),c(A[,2],M[,2]),c(A[,3],M[,3]),c(A[,4],M[,4]))
    colnames(df)<-c("yr","diff","event_time","group")
    # plot
    ggplot(df, aes(x=event_time,y=diff,group=group,color=group))+
      geom_line(size=3)+
      theme_bw()+
      ylab("Mean Transaction")+
      xlab("Year")+
      theme(axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15))+
      scale_x_discrete(limits=unique(df$event_time))+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme(legend.position="bottom")+
      theme(legend.title=element_blank())+
      theme(text = element_text(size=25))
    # save
    Name<-paste("plot_deg_","year_",i,".pdf",sep="")
    ggsave(Name)
  }
  print(i)
}

