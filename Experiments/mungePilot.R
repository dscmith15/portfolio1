# This file will take the data from the pilot experiment and output the results into simple arrays.

library(jsonlite)
library("plyr")
library(R2HTML)
library(minpack.lm)

setwd("/var/www/html/Experiments/data")

pFiles <- list.files();

for (i in 1:length(pFiles)) {
  ptemp <- fromJSON(pFiles[i])
  ptemp <- ptemp[ptemp$timesseen==5,]
  ptemp <- ptemp[3:nrow(ptemp),]
  
  ptemp <- data.frame(ptemp$rt,ptemp$deal,ptemp$delay,ptemp$odd,ptemp$subvalue)
  ptemp["pnum"]<-rep(i,nrow(ptemp))
  
  if (i==1){
    pfinal <- ptemp
    
  } else {
    pfinal <- rbind(pfinal, ptemp)
  }
}

pfinal <- pfinal[ ! pfinal$pnum %in% c(4,6,8,12,26),]



# to get the means for the report
resultTab <- aggregate(pfinal$ptemp.subvalue~pfinal$ptemp.delay*pfinal$ptemp.odd, FUN = "mean")

resultTab$`pfinal$ptemp.delay` <-ordered(resultTab$`pfinal$ptemp.delay`, levels = c("immediately", "in 1 month", "in 6 months", "in 2 years", "in 5 years"))
resultTab$`pfinal$ptemp.odd` <-ordered(resultTab$`pfinal$ptemp.odd`, levels= c('100%','80%','40%','25%','10%'))

immediatef <- resultTab[resultTab$`pfinal$ptemp.delay`=='immediately',]
immediatef <- with(immediatef, immediatef[order(`pfinal$ptemp.odd`),])
immediatef <- immediatef$`pfinal$ptemp.subvalue`

onemonthf <- resultTab[resultTab$`pfinal$ptemp.delay`=='in 1 month',]
onemonthf <- with(onemonthf, onemonthf[order(`pfinal$ptemp.odd`),])
onemonthf <- onemonthf$`pfinal$ptemp.subvalue`

sixmonthf <- resultTab[resultTab$`pfinal$ptemp.delay`=='in 6 months',]
sixmonthf <- with(sixmonthf, sixmonthf[order(`pfinal$ptemp.odd`),])
sixmonthf <- sixmonthf$`pfinal$ptemp.subvalue`

twoyearf <- resultTab[resultTab$`pfinal$ptemp.delay`=='in 2 years',]
twoyearf <- with(twoyearf, twoyearf[order(`pfinal$ptemp.odd`),])
twoyearf <- twoyearf$`pfinal$ptemp.subvalue`

fiveyearf <- resultTab[resultTab$`pfinal$ptemp.delay`=='in 5 years',]
fiveyearf <- with(fiveyearf, fiveyearf[order(`pfinal$ptemp.odd`),])
fiveyearf <- fiveyearf$`pfinal$ptemp.subvalue`

results <- rbind(immediatef, onemonthf, sixmonthf, twoyearf, fiveyearf)

results <- round(results)

setwd("/var/www/html/result/")
write.table(results, "results.csv", col.names = FALSE, row.names = FALSE, sep = ",")

pfinal["odd"] <- revalue(pfinal$ptemp.odd, c("100%"=1,"80%"=4,"40%"=1.5,"25%"=3,"10%"=9))
pfinal["delay"] <- revalue(pfinal$ptemp.delay, c("immediately"=1,"in 1 month"=4,"in 6 months"=1.5,"in 2 years"=3,"in 5 years"=9))

nlmodel <- nlsLM(ptemp.subvalue~(40000/((1+odd)*((1+delay)^.5))), data = pfinal, start=list(odd = 1.4, delay = 1.4))
print(nlmodel)

#setwd("/var/www/html/")
#HTML(pfinal, file = "pilotdata.html", Border = 1, innerBorder = 0,classfirstline = "firstline",
#  classfirstcolumn = "firstcolumn", classcellinside = "cellinside", append = TRUE)

setwd("/var/www/html/result/")
# to get the data for the histogram
histdat <- as.list(t(round(pfinal$ptemp.subvalue)))
write.table(histdat, "histdat.csv", col.names = FALSE, row.names = FALSE, sep = ",")

# to get the medians for the report
resultTab <- aggregate(pfinal$ptemp.subvalue~pfinal$ptemp.delay*pfinal$ptemp.odd, FUN = "median")

resultTab$`pfinal$ptemp.delay` <-ordered(resultTab$`pfinal$ptemp.delay`, levels = c("immediately", "in 1 month", "in 6 months", "in 2 years", "in 5 years"))
resultTab$`pfinal$ptemp.odd` <-ordered(resultTab$`pfinal$ptemp.odd`, levels= c('100%','80%','40%','25%','10%'))

immediatef <- resultTab[resultTab$`pfinal$ptemp.delay`=='immediately',]
immediatef <- with(immediatef, immediatef[order(`pfinal$ptemp.odd`),])
immediatef <- immediatef$`pfinal$ptemp.subvalue`

onemonthf <- resultTab[resultTab$`pfinal$ptemp.delay`=='in 1 month',]
onemonthf <- with(onemonthf, onemonthf[order(`pfinal$ptemp.odd`),])
onemonthf <- onemonthf$`pfinal$ptemp.subvalue`

sixmonthf <- resultTab[resultTab$`pfinal$ptemp.delay`=='in 6 months',]
sixmonthf <- with(sixmonthf, sixmonthf[order(`pfinal$ptemp.odd`),])
sixmonthf <- sixmonthf$`pfinal$ptemp.subvalue`

twoyearf <- resultTab[resultTab$`pfinal$ptemp.delay`=='in 2 years',]
twoyearf <- with(twoyearf, twoyearf[order(`pfinal$ptemp.odd`),])
twoyearf <- twoyearf$`pfinal$ptemp.subvalue`

fiveyearf <- resultTab[resultTab$`pfinal$ptemp.delay`=='in 5 years',]
fiveyearf <- with(fiveyearf, fiveyearf[order(`pfinal$ptemp.odd`),])
fiveyearf <- fiveyearf$`pfinal$ptemp.subvalue`

results <- rbind(immediatef, onemonthf, sixmonthf, twoyearf, fiveyearf)

results <- round(results)
write.table(results, "resultsmed.csv", col.names = FALSE, row.names = FALSE, sep = ",")

