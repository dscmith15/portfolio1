# This file will take the data from the pilot experiment and output the results into simple arrays.

library(jsonlite)

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

resultinf <- summary(nls(pfinal$ptemp.subvalue~(40000/((1-pfinal$ptemp.odd)*(1-pfinal$ptemp.delay)))))


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
cat("Non linear least squares hyperbolic ANOVA", resultinf, file="summary.txt", sep="n", append=TRUE)

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

# to get the se for the report
resultTab <- aggregate(pfinal$ptemp.subvalue~pfinal$ptemp.delay*pfinal$ptemp.odd, FUN = "sd")

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

results <-results/sqrt(29)

results <- round(results)

write.table(results, "error.csv", col.names = FALSE, row.names = FALSE, sep = ",")

