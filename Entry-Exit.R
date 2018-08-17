library(fastmatch)
library(xtable)
library(igraph)
library(ggplot2)

### Network Analysis
### Aug 15, 2018

### Entry-Exit Analysis

# read the dataset
setwd("/Users/terrywu/Dropbox/MLS Network/Analysis Aug 15")
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

  
### Exist
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



  