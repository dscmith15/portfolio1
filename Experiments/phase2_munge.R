library(XML)
library(jsonlite)
library("plyr")
library(R2HTML)
library(RCurl)
library(lme4)
library(nlme)
library(MCMCpack)
library(brm)
library(multcomp)
library(caret)
library(MuMIn)

file.list <- readHTMLTable(getURL("https://www.dcsmithresearch.com/Experiments/dataP2/"),skip.rows=1:2)[[1]]$Name

# then create the list of paths
# as a note, this index starts at one
pFiles <- paste("https://www.dcsmithresearch.com/Experiments/dataP2/",file.list[!is.na(file.list)], sep="")

pFiles<- pFiles[pFiles != "https://www.dcsmithresearch.com/Experiments/dataP2/archive/"]
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

  if (mean(ptemp[1:25,"subvalue"])>625){
    ptemp$value[1:25]<-40000
    ptemp$value[26:50]<-800

  } else {
    ptemp$value[1:25]<-800
    ptemp$value[26:50]<-40000
  }


  if (i==1){
    pfinal <- ptemp

  } else {
    pfinal <- rbind.fill(pfinal, ptemp)
  }
}
pfinal$gender<-as.factor(pfinal$gender)
pfinal$adjValue <- pfinal$subvalue/pfinal$value
pfinal$value <- as.factor(pfinal$value)
pfinal$income <- as.numeric(pfinal$income)
pfinal$job <- as.character(pfinal$job)
pfinal$odd <- as.factor(pfinal$odd)
pfinal$delay <- as.factor(pfinal$delay)
pfinal$delay <-ordered(pfinal$delay, levels = c("immediately", "in 1 month", "in 6 months", "in 2 years", "in 5 years"))
pfinal$odd <-ordered(pfinal$odd, levels= c('100%','80%','40%','25%','10%'))


#valid checker
baddata<-unique(c(pfinal$workid[pfinal$delay == 'immediately' & pfinal$odd == "100%" & pfinal$value == 40000 & pfinal$subvalue < 39375.0],
                  pfinal$workid[pfinal$delay == 'immediately' & pfinal$odd == "100%" & pfinal$value == 800 & pfinal$subvalue < 787.5]))
pfinal <- pfinal[! pfinal$workid %in% baddata,]


#removes nonsense variables
drops <- c("url","trial_type", "trial_index", "time_elapsed", "internal_node_id","view_history", "responses")
pfinal <- pfinal[ , !(names(pfinal) %in% drops)]

pfinal$income[is.na(pfinal$income)] <- 0
colnames(pfinal)[12]<-"Gamer"
colnames(pfinal)[18]<-"DurationPlayed"
colnames(pfinal)[19]<-"Payer"
colnames(pfinal)[20]<-"MoneySpent"
colnames(pfinal)[21]<-"GamesPlayed"
pfinal$Payer[is.na(pfinal$Payer)&pfinal$Gamer=='Yes']<-'No'
pfinal$Payer<-as.factor(pfinal$Payer)
pfinal$Gamer<-as.factor(pfinal$Gamer)
pfinal$MoneySpent <- as.numeric(pfinal$MoneySpent)

summary(pfinal$Gamer)/50

#display pmf of risk lit
colSums(table(pfinal$pnum,pfinal$riskLiteracy)/50)/sum(colSums(table(pfinal$pnum,pfinal$riskLiteracy)/50))

pfinal800 <- pfinal[pfinal$value==800,]
pfinal40000 <- pfinal[pfinal$value==40000,]
hyper800fit <- hyperblm(subvalue~as.numeric(odd)+as.numeric(delay)+as.numeric(odd):as.numeric(delay), data = pfinal800)
hyper40000fit <- hyperblm(subvalue~as.numeric(odd)+as.numeric(delay)+as.numeric(odd):as.numeric(delay), data = pfinal40000)

hyper800fit_sum<-summary(hyper800fit, hessian = FALSE, nboots = 1000)
hyper40000fit_sum<-summary(hyper40000fit, hessian = FALSE, nboots = 1000)

######
#Baye
bay800fit <- MCMCregress(subvalue~as.numeric(odd)+as.numeric(delay)+as.numeric(odd):as.numeric(delay), data = pfinal800)
bay40kfit <- MCMCregress(subvalue~as.numeric(odd)+as.numeric(delay)+as.numeric(odd):as.numeric(delay), data = pfinal40000)

nls800<-nls(subvalue~800/((1+as.numeric(odd)*a)*(1+as.numeric(delay)*b)), data = pfinal800, start=c(a=2.5,b=1.5),trace = TRUE)
nls40k<-nls(subvalue~40000/((1+as.numeric(odd)*a)*(1+as.numeric(delay)*b)), data = pfinal40000, start=c(a=.7,b=.7),trace = TRUE)

#get some estimation of goodness of fit
cor(pfinal40000$subvalue,predict(nls40k))
cor(pfinal800$subvalue,predict(nls800))

# now I'll make a table of values for the 800 value
resultTab <- aggregate(pfinal800$subvalue~pfinal800$delay*pfinal800$odd, FUN = "mean")

resultTab$`pfinal800$delay` <-ordered(resultTab$`pfinal800$delay`, levels = c("immediately", "in 1 month", "in 6 months", "in 2 years", "in 5 years"))
resultTab$`pfinal800$odd` <-ordered(resultTab$`pfinal800$odd`, levels= c('100%','80%','40%','25%','10%'))

immediatef <- resultTab[resultTab$`pfinal800$delay`=='immediately',]
immediatef <- with(immediatef, immediatef[order(`pfinal800$odd`),])
immediatef <- immediatef$`pfinal800$subvalue`

onemonthf <- resultTab[resultTab$`pfinal800$delay`=='in 1 month',]
onemonthf <- with(onemonthf, onemonthf[order(`pfinal800$odd`),])
onemonthf <- onemonthf$`pfinal800$subvalue`

sixmonthf <- resultTab[resultTab$`pfinal800$delay`=='in 6 months',]
sixmonthf <- with(sixmonthf, sixmonthf[order(`pfinal800$odd`),])
sixmonthf <- sixmonthf$`pfinal800$subvalue`

twoyearf <- resultTab[resultTab$`pfinal800$delay`=='in 2 years',]
twoyearf <- with(twoyearf, twoyearf[order(`pfinal800$odd`),])
twoyearf <- twoyearf$`pfinal800$subvalue`

fiveyearf <- resultTab[resultTab$`pfinal800$delay`=='in 5 years',]
fiveyearf <- with(fiveyearf, fiveyearf[order(`pfinal800$odd`),])
fiveyearf <- fiveyearf$`pfinal800$subvalue`

results <- rbind(immediatef, onemonthf, sixmonthf, twoyearf, fiveyearf)

results800 <- round(results)

# now I'll make a table of values for the 40000 value
resultTab <- aggregate(pfinal40000$subvalue~pfinal40000$delay*pfinal40000$odd, FUN = "mean")

resultTab$`pfinal40000$delay` <-ordered(resultTab$`pfinal40000$delay`, levels = c("immediately", "in 1 month", "in 6 months", "in 2 years", "in 5 years"))
resultTab$`pfinal40000$odd` <-ordered(resultTab$`pfinal40000$odd`, levels= c('100%','80%','40%','25%','10%'))

immediatef <- resultTab[resultTab$`pfinal40000$delay`=='immediately',]
immediatef <- with(immediatef, immediatef[order(`pfinal40000$odd`),])
immediatef <- immediatef$`pfinal40000$subvalue`

onemonthf <- resultTab[resultTab$`pfinal40000$delay`=='in 1 month',]
onemonthf <- with(onemonthf, onemonthf[order(`pfinal40000$odd`),])
onemonthf <- onemonthf$`pfinal40000$subvalue`

sixmonthf <- resultTab[resultTab$`pfinal40000$delay`=='in 6 months',]
sixmonthf <- with(sixmonthf, sixmonthf[order(`pfinal40000$odd`),])
sixmonthf <- sixmonthf$`pfinal40000$subvalue`

twoyearf <- resultTab[resultTab$`pfinal40000$delay`=='in 2 years',]
twoyearf <- with(twoyearf, twoyearf[order(`pfinal40000$odd`),])
twoyearf <- twoyearf$`pfinal40000$subvalue`

fiveyearf <- resultTab[resultTab$`pfinal40000$delay`=='in 5 years',]
fiveyearf <- with(fiveyearf, fiveyearf[order(`pfinal40000$odd`),])
fiveyearf <- fiveyearf$`pfinal40000$subvalue`

results <- rbind(immediatef, onemonthf, sixmonthf, twoyearf, fiveyearf)

results40000 <- round(results)

############################################

bayfit <- MCMCregress(adjValue~as.numeric(odd)*as.numeric(delay)*as.numeric(value), data = pfinal)
sum_bay <- summary(bayfit)

hlfit <- lmer(adjValue~as.numeric(odd)*as.numeric(delay)*as.numeric(value)+(1|pnum), data = pfinal)
#hlfitcomp <- lmer(adjValue~as.numeric(odd)*as.numeric(delay)*as.numeric(value)+(as.numeric(odd)*as.numeric(delay)*as.numeric(value)|pnum), data = pfinal,control=lmerControl(optCtrl=list(maxfun=100000)))
#nlscomp<-nls(adjValue~(1)/((1+as.numeric(odd)*od)*(1+as.numeric(delay)*del)), data = pfinal, start=c(od=1,del=1.5),trace = TRUE)

repaov<-aov(adjValue~as.factor(odd)*as.factor(delay)*as.factor(value)+Error(pnum/(as.factor(odd)*as.factor(delay)*as.factor(value))), data = pfinal)

lme_aov <- lme(adjValue~as.factor(odd)*as.factor(delay)*as.factor(value),data = pfinal,random = ~1|pnum)
lme_aov_sum<-anova(lme_aov)
#summary(glht(lme_aov, linfct=mcp(Material = "Tukey")), test = adjusted(type = "bonferroni"))



nlH_mod <- lme(adjValue~as.factor(value)/((1+as.numeric(odd))*(1+as.numeric(delay))),
               random = ~1 | pnum,
               data=pfinal,
               control=lmeControl(optCtrl=list(maxIter=100000000)))
nle_mod <- lme(adjValue~(as.factor(value)*(exp((-as.numeric(odd))*exp(as.numeric(delay))))),
               random = ~1 | pnum,
               data=pfinal,
               control=lmeControl(optCtrl=list(maxIter=100000000)))



#se_hlm<-sqrt(sum((pfinal$adjValue-predict(hlfitcomp))^2)/2500)
se_nlH<-sqrt(sum((pfinal$adjValue-predict(nlH_mod))^2)/2500)
se_lme<-sqrt(sum((pfinal$adjValue-predict(lme_aov))^2)/2500)
se_exp<-sqrt(sum((pfinal$adjValue-predict(nle_mod))^2)/2500)

barplot(c(se_lme,se_nlH,se_exp))

#k folds

iters=100
for (i in 1:iters){
  samps<-sample(unique(pfinal$pnum),25)
  traindat<-pfinal[pfinal$pnum %in% samps,]
  testdat<-pfinal[!(pfinal$pnum %in% samps),]
  lme_aovk <- lme(adjValue~as.factor(odd)*as.factor(delay)*as.factor(value),data = traindat,random = ~1|pnum)
  nlH_modk <- lme(adjValue~as.factor(value)/((1+as.numeric(odd))*(1+as.numeric(delay))),
                  random = ~1 | pnum,
                  data=traindat,
                  control=lmeControl(optCtrl=list(maxIter=100000000)))
  if(i==1){
    se_nlHktrain<-sqrt(sum((traindat$adjValue-predict(nlH_modk,data = traindat))^2)/nrow(traindat))
    se_lmektrain<-sqrt(sum((traindat$adjValue-predict(lme_aovk,data = traindat))^2)/nrow(traindat))

    se_nlHktest<-sqrt(sum((testdat$adjValue-predict(nlH_modk,data = testdat))^2)/nrow(testdat))
    se_lmektest<-sqrt(sum((testdat$adjValue-predict(lme_aovk,data = testdat))^2)/nrow(testdat))
  }else{
    se_nlHktrain[i]<-sqrt(sum((traindat$adjValue-predict(nlH_modk,data = traindat))^2)/nrow(traindat))
    se_lmektrain[i]<-sqrt(sum((traindat$adjValue-predict(lme_aovk,data = traindat))^2)/nrow(traindat))

    se_nlHktest[i]<-sqrt(sum((testdat$adjValue-predict(nlH_modk,data = testdat))^2)/nrow(testdat))
    se_lmektest[i]<-sqrt(sum((testdat$adjValue-predict(lme_aovk,data = testdat))^2)/nrow(testdat))
  }

}
boxplot(se_lmektrain,se_nlHktrain,se_lmektest,se_nlHktest)
r.squaredGLMM(lme_aov)
