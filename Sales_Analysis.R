
library(readstata13)
library(ggplot2)
library(fastmatch)

### Sales Analysis
### Aug 15, 2018

# set the working directory and read data
setwd("/Users/terrywu/Dropbox/MLS Network")
X<-read.dta13("SD9191_market_full_w_deeds_prepped.dta")

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
  geom_hline(yintercept=mean(mydata$Freq), linetype="dashed", color = "coral",size=1.3)+
  coord_fixed(ratio=0.01)

# average
mean(mydata$Freq)

# then sales by agents
# read dataset
X<-read.dta13("SD9191_market_full_w_deeds_prepped.dta")

# agent list
agent<-c(X$lagent_id,X$bagent_id)
agent<-unique(agent)

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
  geom_hline(yintercept=mean(mydata$Freq), linetype="dashed", color = "coral",size=1.3)+
  coord_fixed(ratio=50)

# average
mean(mydata$Freq)

# then, excluding years before the agent enters
rm(list=ls())

# read the dataset
X<-read.dta13("SD9191_market_full_w_deeds_prepped.dta")

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
for(i in 1:length(agent))
{
temp1<-X[X$bagent_id==agent[i],]
temp2<-X[X$lagent_id==agent[i],]
temp<-rbind(temp1, temp2)
temp_year<-c(temp$list_year,temp$close_year)
temp_year<-names(table(temp_year))
temp_year<-as.numeric(temp_year)
ID<-fmatch(temp_year, year)
entry_matrix[i, ID]<-1
print(i)
}

# write out the matrix
rownames(entry_matrix)<-agent
colnames(entry_matrix)<-year
write.csv(entry_matrix,"/Users/terrywu/Dropbox/MLS Network/Analysis Aug 15/active_agent_by_year.csv")
