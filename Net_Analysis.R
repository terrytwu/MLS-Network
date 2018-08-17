
library(fastmatch)
library(xtable)
library(igraph)
library(ggplot2)

### Network Analysis
### Aug 15, 2018

### In this set of analysis, we just pool years together

# read the dataset
setwd("/Users/terrywu/Dropbox/MLS Network")
X<-read.csv("Edge_list.csv")

# degree
# create a dataset with three columns
# outdegree, indegree and degree
# agent list
agent<-c(X[,1],X[,2])
agent<-unique(agent)

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
  theme(text = element_text(size=20))+
  coord_fixed(ratio=0.005)

mydata1<-mydata[mydata$Indegree<=20,]
ggplot(mydata1, aes(x=Indegree)) + 
  geom_histogram(binwidth=1,color="black", fill="white")+
  theme_bw()+
  ylab("")+
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  theme(text = element_text(size=20))+
  coord_fixed(ratio=0.005)

mydata1<-mydata[mydata$Degree<=20,]
ggplot(mydata1, aes(x=Degree)) + 
  geom_histogram(binwidth=1,color="black", fill="white")+
  theme_bw()+
  ylab("")+
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  theme(text = element_text(size=20))+
  coord_fixed(ratio=0.005)

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



