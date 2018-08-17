
library(xtable)
library(igraph)
library(readstata13)
library(ggplot2)
library(fastmatch)
library(Rfast)
library(reshape2)

### Network Analysis
### Aug 16, 2018

### Whether still work together the next year

# read the dataset
setwd("/Users/terrywu/Dropbox/MLS Network")
X<-read.dta13("SD9191_market_full_w_deeds_prepped.dta")

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
X$num<-1
mydata<-aggregate(X$num~X$close_year, FUN=sum)
colnames(mydata)<-c("Year","Num")

# calculate the num of partners who work together in the next year
part<-data.frame(mydata$Year[-14],NA)
for(i in 1:13)
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
part[is.na(part[,2]),2]<-0
mydata<-mydata[-14,]
mydata[,2]<-part[,2]/mydata[,2]


# plot
ggplot(mydata,aes(Year, Num))+
  geom_line(size=3)+
  theme_bw()+
  ylab("Ratio")+
  xlab("Year")+
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  theme(text = element_text(size=20))+
  coord_fixed(ratio=500)+
  scale_x_discrete(limits=c(2001:2013))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



