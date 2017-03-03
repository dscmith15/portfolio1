library(XML)
library(jsonlite)
library("plyr")
library(R2HTML)
library(RCurl)
library(GeneralizedHyperbolic)

file.list <- readHTMLTable(getURL("https://www.dcsmithresearch.com/Experiments/dataP4/"),skip.rows=1:2)[[1]]$Name

# then create the list of paths
# as a note, this index starts at one
pFiles <- paste("https://www.dcsmithresearch.com/Experiments/dataP4/",file.list[!is.na(file.list)], sep="")


for (i in 1:length(pFiles)) {
  ptemp <- getURL(pFiles[i])
  ptemp <- fromJSON(ptemp)
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
    pfinal <- rbind(pfinal, ptemp)
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

#removes nonsense variables
drops <- c("url","trial_type", "trial_index", "time_elapsed", "internal_node_id","view_history", "responses")
pfinal <- pfinal[ , !(names(pfinal) %in% drops)]

pfinal$income[is.na(pfinal$income)] <- 0
#print questionable data
unique(c(pfinal$workid[pfinal$delay == 'immediately' & pfinal$odd == "100%" & pfinal$value == 20 & pfinal$subvalue < 19.68],
         pfinal$workid[pfinal$delay == 'immediately' & pfinal$odd == "100%" & pfinal$value == 2500 & pfinal$subvalue < 2460.9375],
        pfinal$workid[pfinal$delay == 'immediately' & pfinal$odd == "100%" & pfinal$value == 8018000 & pfinal$subvalue < 7892718.75]))

pfinal <- pfinal[pfinal$workid != unique(c(pfinal$workid[pfinal$delay == 'immediately' & pfinal$odd == "100%" & pfinal$value == 20 & pfinal$subvalue < 19.68],
         pfinal$workid[pfinal$delay == 'immediately' & pfinal$odd == "100%" & pfinal$value == 2500 & pfinal$subvalue < 2460.9375],
        pfinal$workid[pfinal$delay == 'immediately' & pfinal$odd == "100%" & pfinal$value == 8018000 & pfinal$subvalue < 7892718.75])),]

summary(pfinal$gender)/50
