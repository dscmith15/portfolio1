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
pfinal$adjValue <- pfinal$subvalue/pfinal$value
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
baddata<-unique(c(pfinal$workid[pfinal$delay == 'immediately' & pfinal$odd == "100%" & pfinal$value == 20 & pfinal$subvalue < 19.68],
                  pfinal$workid[pfinal$delay == 'immediately' & pfinal$odd == "100%" & pfinal$value == 2500 & pfinal$subvalue < 2460.9375],
                  pfinal$workid[pfinal$delay == 'immediately' & pfinal$odd == "100%" & pfinal$value == 8018000 & pfinal$subvalue < 7892718.75]))
pfinal <- pfinal[! pfinal$workid %in% baddata,]
summary(pfinal$gender)/75


####################################################################################

pfinal20 <- pfinal[pfinal$value==20,]
pfinal2500 <- pfinal[pfinal$value==2500,]
pfinal8018000 <- pfinal[pfinal$value==8018000,]
hyper20fit <- hyperblm(subvalue~as.numeric(odd)+as.numeric(delay)+as.numeric(odd):as.numeric(delay), data = pfinal20)
hyper2500fit <- hyperblm(subvalue~as.numeric(odd)+as.numeric(delay)+as.numeric(odd):as.numeric(delay), data = pfinal2500)
hyper8018000fit <- hyperblm(subvalue~as.numeric(odd)+as.numeric(delay)+as.numeric(odd):as.numeric(delay), data = pfinal8018000)


hyper20fit_sum<-summary(hyper20fit, hessian = FALSE, nboots = 1000)
hyper2500fit_sum<-summary(hyper2500fit, hessian = FALSE, nboots = 1000)
hyper8018000fit_sum<-summary(hyper8018000fit, hessian = FALSE, nboots = 1000)

nls20<-nls(subvalue~20/((1+as.numeric(odd)*a)*(1+as.numeric(delay)*b)), data = pfinal20, start=c(a=2.5,b=1.5),trace = TRUE)
nls2500<-nls(subvalue~2500/((1+as.numeric(odd)*a)*(1+as.numeric(delay)*b)), data = pfinal2500, start=c(a=2.5,b=1.5),trace = TRUE)
nls8018000<-nls(subvalue~8018000/((1+as.numeric(odd)*a)*(1+as.numeric(delay)*b)), data = pfinal8018000, start=c(a=2.5,b=1.5),trace = TRUE)


#get some estimation of goodness of fit
cor(pfinal20$subvalue,predict(nls20))
cor(pfinal2500$subvalue,predict(nls2500))
cor(pfinal8018000$subvalue,predict(nls8018000))

###############################################################################

# making tables of the values
resultTab <- aggregate(pfinal20$subvalue~pfinal20$delay*pfinal20$odd, FUN = "mean")

resultTab$`pfinal20$delay` <-ordered(resultTab$`pfinal20$delay`, levels = c("immediately", "in 6 hours", "in 33 hours", "in 6 days", "in 2 weeks"))
resultTab$`pfinal20$odd` <-ordered(resultTab$`pfinal20$odd`, levels= c('100%','80%','40%','25%','10%'))

immediatef <- resultTab[resultTab$`pfinal20$delay`=='immediately',]
immediatef <- with(immediatef, immediatef[order(`pfinal20$odd`),])
immediatef <- immediatef$`pfinal20$subvalue`

onemonthf <- resultTab[resultTab$`pfinal20$delay`=='in 6 hours',]
onemonthf <- with(onemonthf, onemonthf[order(`pfinal20$odd`),])
onemonthf <- onemonthf$`pfinal20$subvalue`

sixmonthf <- resultTab[resultTab$`pfinal20$delay`=='in 33 hours',]
sixmonthf <- with(sixmonthf, sixmonthf[order(`pfinal20$odd`),])
sixmonthf <- sixmonthf$`pfinal20$subvalue`

twoyearf <- resultTab[resultTab$`pfinal20$delay`=='in 6 days',]
twoyearf <- with(twoyearf, twoyearf[order(`pfinal20$odd`),])
twoyearf <- twoyearf$`pfinal20$subvalue`

fiveyearf <- resultTab[resultTab$`pfinal20$delay`=='in 2 weeks',]
fiveyearf <- with(fiveyearf, fiveyearf[order(`pfinal20$odd`),])
fiveyearf <- fiveyearf$`pfinal20$subvalue`

results <- rbind(immediatef, onemonthf, sixmonthf, twoyearf, fiveyearf)

results20 <- round(results)
####################################

resultTab <- aggregate(pfinal2500$subvalue~pfinal2500$delay*pfinal2500$odd, FUN = "mean")

resultTab$`pfinal2500$delay` <-ordered(resultTab$`pfinal2500$delay`, levels = c("immediately", "in 6 hours", "in 33 hours", "in 6 days", "in 2 weeks"))
resultTab$`pfinal2500$odd` <-ordered(resultTab$`pfinal2500$odd`, levels= c('100%','80%','40%','25%','10%'))

immediatef <- resultTab[resultTab$`pfinal2500$delay`=='immediately',]
immediatef <- with(immediatef, immediatef[order(`pfinal2500$odd`),])
immediatef <- immediatef$`pfinal2500$subvalue`

onemonthf <- resultTab[resultTab$`pfinal2500$delay`=='in 6 hours',]
onemonthf <- with(onemonthf, onemonthf[order(`pfinal2500$odd`),])
onemonthf <- onemonthf$`pfinal2500$subvalue`

sixmonthf <- resultTab[resultTab$`pfinal2500$delay`=='in 33 hours',]
sixmonthf <- with(sixmonthf, sixmonthf[order(`pfinal2500$odd`),])
sixmonthf <- sixmonthf$`pfinal2500$subvalue`

twoyearf <- resultTab[resultTab$`pfinal2500$delay`=='in 6 days',]
twoyearf <- with(twoyearf, twoyearf[order(`pfinal2500$odd`),])
twoyearf <- twoyearf$`pfinal2500$subvalue`

fiveyearf <- resultTab[resultTab$`pfinal2500$delay`=='in 2 weeks',]
fiveyearf <- with(fiveyearf, fiveyearf[order(`pfinal2500$odd`),])
fiveyearf <- fiveyearf$`pfinal2500$subvalue`

results <- rbind(immediatef, onemonthf, sixmonthf, twoyearf, fiveyearf)

results2500 <- round(results)

####################################

resultTab <- aggregate(pfinal8018000$subvalue~pfinal8018000$delay*pfinal8018000$odd, FUN = "mean")

resultTab$`pfinal8018000$delay` <-ordered(resultTab$`pfinal8018000$delay`, levels = c("immediately", "in 6 hours", "in 33 hours", "in 6 days", "in 2 weeks"))
resultTab$`pfinal8018000$odd` <-ordered(resultTab$`pfinal8018000$odd`, levels= c('100%','80%','40%','25%','10%'))

immediatef <- resultTab[resultTab$`pfinal8018000$delay`=='immediately',]
immediatef <- with(immediatef, immediatef[order(`pfinal8018000$odd`),])
immediatef <- immediatef$`pfinal8018000$subvalue`

onemonthf <- resultTab[resultTab$`pfinal8018000$delay`=='in 6 hours',]
onemonthf <- with(onemonthf, onemonthf[order(`pfinal8018000$odd`),])
onemonthf <- onemonthf$`pfinal8018000$subvalue`

sixmonthf <- resultTab[resultTab$`pfinal8018000$delay`=='in 33 hours',]
sixmonthf <- with(sixmonthf, sixmonthf[order(`pfinal8018000$odd`),])
sixmonthf <- sixmonthf$`pfinal8018000$subvalue`

twoyearf <- resultTab[resultTab$`pfinal8018000$delay`=='in 6 days',]
twoyearf <- with(twoyearf, twoyearf[order(`pfinal8018000$odd`),])
twoyearf <- twoyearf$`pfinal8018000$subvalue`

fiveyearf <- resultTab[resultTab$`pfinal8018000$delay`=='in 2 weeks',]
fiveyearf <- with(fiveyearf, fiveyearf[order(`pfinal8018000$odd`),])
fiveyearf <- fiveyearf$`pfinal8018000$subvalue`

results <- rbind(immediatef, onemonthf, sixmonthf, twoyearf, fiveyearf)

results8018000 <- round(results)

########################################################################
#adjusted values

pfinal$value <- as.factor(pfinal$value)
AdjTab <- aggregate(pfinal$adjValue~pfinal$value*pfinal$odd, FUN = "mean")

hyperfit <- hyperblm(adjValue~as.numeric(odd)+as.numeric(value)+as.numeric(odd):as.numeric(value), data = pfinal)
hyperfit_sum<-summary(hyperfit, hessian = FALSE, nboots = 1000)
