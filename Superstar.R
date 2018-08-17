
library(xtable)
library(igraph)
library(readstata13)
library(ggplot2)
library(fastmatch)
library(Rfast)
library(reshape2)

### Network Analysis
### Aug 16, 2018

### Disappear and Transfer Analysis

# read the dataset
setwd("/Users/terrywu/Dropbox/MLS Network")
X<-read.dta13("SD9191_market_full_w_deeds_prepped.dta")

# construct a matrix
# to record the number of transcations of each agent in each year

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
trans_matrix<-matrix(0, nrow = length(agent), ncol=length(year))

# loop to identify the num of transcations in a given year
for(i in 1:length(agent))
{
  temp1<-X[X$bagent_id==agent[i],]
  temp2<-X[X$lagent_id==agent[i],]
  # num of transcations and years
  temp_trans1<-temp1$close_year
  temp_trans2<-temp2$list_year
  temp<-table(c(temp_trans1,temp_trans2))
  temp_year<-as.numeric(names(temp))
  temp_trans<-as.numeric(temp)
  ID<-fmatch(temp_year, year)
  trans_matrix[i, ID]<-temp_trans
  print(i)
}

# write out the matrix
rownames(trans_matrix)<-agent
colnames(trans_matrix)<-year
write.csv(trans_matrix,"/Users/terrywu/Dropbox/MLS Network/Analysis Aug 15/transaction_of_agent_by_year.csv")

# subset the matrix to include the agents who have 20 transcations in 
# at least one of the years

# the max transactions by an agent
trans_max<-rowMaxs(trans_matrix, value = TRUE)
trans_matrix<-trans_matrix[trans_max>=20,]

# plot
# reshape
df<-melt(trans_matrix)  #the function melt reshapes it from wide to long
df$rowid <- 1:7
ggplot(df, aes(Var2, value, group=factor(rowid)))+ 
geom_line(aes(color=factor(rowid)),size=2.4)+
  theme_bw()+
  ylab("Num of Transactions")+
  xlab("Year")+
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25))+
  theme(text = element_text(size=25))+
  theme(legend.position="none") +
  scale_x_discrete(limits=c(2001:2015))+
  theme(legend.title=element_blank())+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# record the id of the agents with many transactions
id_super<-row.names(trans_matrix)

# extract the offices of these agents

# define an address list
office<-list()
for(i in 1:length(id_super))
{
# as a lagent
temp<-X[X$lagent_id==id_super[i],]  
year_temp<-names(trans_matrix[i,as.numeric(trans_matrix[i,])!=0])
year_temp<-as.numeric(year_temp)
temp<-temp[temp$list_year %in% year_temp,]
temp1<-temp$lagent_office_name

# as a bagent
temp<-X[X$bagent_id==id_super[i],]  
year_temp<-names(trans_matrix[i,as.numeric(trans_matrix[i,])!=0])
year_temp<-as.numeric(year_temp)
temp<-temp[temp$close_year %in% year_temp,]
temp2<-temp$bagent_office_name

# assign
office[[i]]<-names(table(c(temp1,temp2)))

print(i)
}

# for the agents 6 and 7, they changed the office
# as a lagent of agent 6
temp<-X[X$lagent_id==id_super[6],]  
year_temp<-names(trans_matrix[6,as.numeric(trans_matrix[6,])!=0])
year_temp<-as.numeric(year_temp)
temp<-temp[temp$list_year %in% year_temp,]
temp1<-temp$lagent_office_name
my1<-data.frame(temp1, temp$list_year)
my1<-unique(my1[,1:2])
my1<-my1[order(my1[,2]),]
print(xtable(my1), include.rownames=FALSE)

# as a lagent of agent 7
temp<-X[X$lagent_id==id_super[7],]  
year_temp<-names(trans_matrix[7,as.numeric(trans_matrix[7,])!=0])
year_temp<-as.numeric(year_temp)
temp<-temp[temp$list_year %in% year_temp,]
temp1<-temp$lagent_office_name
my2<-data.frame(temp1, temp$list_year)
my2<-unique(my2[,1:2])
my2<-my2[order(my2[,2]),]
print(xtable(my2), include.rownames=FALSE)

