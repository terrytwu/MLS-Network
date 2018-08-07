
library(readstata13)


# Construct Networks Chula Vista dataset
# Aug 7, 2018

# set the working directory and read data
setwd("/Users/terrywu/Dropbox/MLS Network")
X<-read.dta13("SD9191_market_full_w_deeds_prepped.dta")

# extract the id of each agent (no duplication)
agent<-c(X$bagent_id, X$lagent_id)
agent<-unique(agent)

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
write.csv(Z,"/Users/terrywu/Dropbox/MLS Network/Edge_list.csv",row.names = F)  

# another type of edge list
Z<-aggregate(Y$sale~Y$listing_agent+Y$buying_agent, FUN=sum)
# change var name
colnames(Z)<-c("listing_agent","buying_agent","num_edge")

# write out the edge-list
write.csv(Z,"/Users/terrywu/Dropbox/MLS Network/Edge_list_2.csv",row.names = F)  
