library(fastmatch)
library(xtable)
library(igraph)
library(ggplot2)
library(readstata13)

### Network Analysis
### Aug 16, 2018

### Network Formation

# read the dataset
setwd("/Users/terrywu/Dropbox/MLS Network/Analysis Aug 15")
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
df<-read.dta13("SD9191_market_full_w_deeds_prepped.dta")

# create a vector to record the num of
# agents who enter and work with star agents
count<-rep(NA, length(enter_agent))

# loop
for(j in 1:13)
{
# for year j, the agent list of new entries 
temp<-row.names(enter_matrix)[which(enter_matrix[,2+j]==1)]

# for these new entries, we need to know
# whether they work with stars
count[j+2]<-0
for(i in 1:enter_agent[j+2])
{
id<-temp[i]
listdata<-df[df$list_year==2002+j,]
listdata<-listdata[listdata$lagent_id==id,]
closedata<-df[df$close_year==2002+j,]
closedata<-closedata[closedata$bagent_id==id,]
part<-c(closedata$lagent_id, listdata$lagent_id)
part<-part[!is.na(part)]
trans<-Y[which(Y[,1] %in% part),j+2]
 if(length(which(trans>=20))>=1)
 {
   count[j+2]<-count[j+2]+1
 }
print(i)
}
}

# the ratio
ratio<-count/enter_agent
# plot
myd<-data.frame(ratio,year)
myd<-myd[count>0,]
myd<-myd[!is.na(myd[,1]),]

# plot
ggplot(myd,aes(year, ratio))+
  geom_line(size=3)+
  theme_bw()+
  ylab("Ratio")+
  xlab("Year")+
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  theme(text = element_text(size=20))+
  coord_fixed(ratio=100)+
  scale_x_discrete(limits=c(2003:2013))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
