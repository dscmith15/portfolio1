library(XML)
library(jsonlite)
library("plyr")
library(R2HTML)
library(RCurl)
library(GeneralizedHyperbolic)

file.list <- readHTMLTable(getURL("https://www.dcsmithresearch.com/Experiments/dataP3/"),skip.rows=1:2)[[1]]$Name

# then create the list of paths
# as a note, this index starts at one
pFiles <- paste("https://www.dcsmithresearch.com/Experiments/dataP3/",file.list[!is.na(file.list)], sep="")
pFiles<- pFiles[pFiles != "https://www.dcsmithresearch.com/Experiments/dataP3/archive/"]


for (i in 1:length(pFiles)) {
  ptemp <- getURL(pFiles[i])
  ptemp <- fromJSON(ptemp)
  for (j in 1:length(ptemp$responses[ptemp$trial_index[ptemp$internal_node_id == '0.0-15.0']:max(ptemp$trial_index)+1])){
    ptemp[,ptemp$internal_node_id[ptemp$trial_index[ptemp$internal_node_id == '0.0-15.0']:max(ptemp$trial_index)+1][j]]<-fromJSON(ptemp$responses[ptemp$trial_index[ptemp$internal_node_id == '0.0-15.0']:max(ptemp$trial_index)+1][j])
  }
  ptemp$riskLiteracy=ptemp$riskLiteracy[complete.cases(ptemp$riskLiteracy)]
  ptemp$age = as.numeric(substr(ptemp[3,"responses"],8,9))
  ptemp$gender <- substr(ptemp[4,"responses"],8,nchar(ptemp[4,"responses"])-2)
  ptemp$income <- as.numeric(gsub(",", "", fromJSON(ptemp[5,"responses"])[1]))
  ptemp$job <- fromJSON(ptemp[5,"responses"])[2]
  ptemp <- ptemp[ptemp$timesseen==5,]
  ptemp <- ptemp[complete.cases(ptemp$timesseen),]
  ptemp$pnum <- i
  
  
  if (i==1){
    pfinal <- ptemp
    
  } else {
    pfinal <- rbind.fill(pfinal, ptemp)
  }
}
pfinal$gender<-as.factor(pfinal$gender)
pfinal$value <- as.factor(pfinal$value)
pfinal$income <- as.numeric(pfinal$income)
pfinal$job <- as.character(pfinal$job)
pfinal$odd <- as.factor(pfinal$odd)
pfinal$delay <- as.factor(pfinal$delay)
pfinal$delay <-ordered(pfinal$delay, levels = c("immediately", "in 6 hours", "in 33 hours", "in 6 days", "in 2 weeks"))
pfinal$odd <-ordered(pfinal$odd, levels= c('100%','80%','40%','25%','10%'))
colnames(pfinal)[19]<-"Gamer"
colnames(pfinal)[25]<-"DurationPlayed"
colnames(pfinal)[26]<-"Payer"
colnames(pfinal)[27]<-"MoneySpent"
colnames(pfinal)[28]<-"GamesPlayed"
pfinal$Payer[is.na(pfinal$Payer)&pfinal$Gamer=='Yes']<-'No'
pfinal$Payer<-as.factor(pfinal$Payer)
pfinal$Gamer<-as.factor(pfinal$Gamer)
pfinal$MoneySpent <- as.numeric(pfinal$MoneySpent)
#print questionable data
baddata<-unique(c(pfinal$workid[pfinal$delay == 'immediately' & pfinal$odd == "100%" & pfinal$value == 20 & pfinal$subvalue < 19.68],
         pfinal$workid[pfinal$delay == 'immediately' & pfinal$odd == "100%" & pfinal$value == 800 & pfinal$subvalue < 787.5]))
pfinal <- pfinal[! pfinal$workid %in% baddata,]

hist(aggregate(pfinal$MoneySpent~pfinal$pnum,FUN="mean")[,2],
     breaks=10,main="Distribution of Dollars Spent on Free-to-Play 
     Games per Month",xlab="USD spent", col = "light blue")
summary(pfinal$Gamer)/50
