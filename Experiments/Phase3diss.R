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
library(lsmeans)

file.list <- readHTMLTable(getURL("https://www.dcsmithresearch.com/Experiments/dataP3/"),skip.rows=1:2)[[1]]$Name

# then create the list of paths
# as a note, this index starts at one
pFiles <- paste("https://www.dcsmithresearch.com/Experiments/dataP3/",file.list[!is.na(file.list)], sep="")
pFiles <- pFiles[pFiles != "https://www.dcsmithresearch.com/Experiments/dataP3/archive/"]



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
pfinal$adjValue <- pfinal$subvalue/pfinal$value
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


summary(pfinal$Gamer)/50

#display pmf of risk lit
colSums(table(pfinal$pnum,pfinal$riskLiteracy)/50)/sum(colSums(table(pfinal$pnum,pfinal$riskLiteracy)/50))

pfinal800 <- pfinal[pfinal$value==800,]
pfinal20 <- pfinal[pfinal$value==20,]
hyper800fit <- hyperblm(subvalue~as.numeric(odd)+as.numeric(delay)+as.numeric(odd):as.numeric(delay), data = pfinal800)
hyper20fit <- hyperblm(subvalue~as.numeric(odd)+as.numeric(delay)+as.numeric(odd):as.numeric(delay), data = pfinal20)
hyperadj <- hyperblm(adjval~as.numeric(odd)+as.numeric(delay)+as.numeric(as.factor(value))+as.numeric(odd):as.numeric(delay)+as.numeric(as.factor(value)):as.numeric(delay), data = pfinal)

hyper800fit_sum<-summary(hyper800fit, hessian = FALSE, nboots = 1000)
hyper20fit_sum<-summary(hyper20fit, hessian = FALSE, nboots = 1000)
hyperadj_sum<-summary(hyperadj, hessian = FALSE, nboots = 1000)

nls800<-nls(subvalue~800/((1+as.numeric(odd)*a)*(1+as.numeric(delay)*b)), data = pfinal800, start=c(a=.7,b=.1),control = list(maxiter = 500))
nls20<-nls(subvalue~20/((1+as.numeric(odd)*a)*(1+as.numeric(delay)*b)), data = pfinal20, start=c(a=2.5,b=0.09228),trace=TRUE,control = list(maxiter = 500))

#get some estimation of goodness of fit
cor(pfinal20$subvalue,predict(nls20))
cor(pfinal800$subvalue,predict(nls800))

# now I'll make a table of values for the 800 value
resultTab <- aggregate(pfinal800$subvalue~pfinal800$delay*pfinal800$odd, FUN = "mean")

resultTab$`pfinal800$delay` <-ordered(resultTab$`pfinal800$delay`, levels = c("immediately", "in 6 hours", "in 33 hours", "in 6 days", "in 2 weeks"))
resultTab$`pfinal800$odd` <-ordered(resultTab$`pfinal800$odd`, levels= c('100%','80%','40%','25%','10%'))

immediatef <- resultTab[resultTab$`pfinal800$delay`=='immediately',]
immediatef <- with(immediatef, immediatef[order(`pfinal800$odd`),])
immediatef <- immediatef$`pfinal800$subvalue`

onemonthf <- resultTab[resultTab$`pfinal800$delay`=='in 6 hours',]
onemonthf <- with(onemonthf, onemonthf[order(`pfinal800$odd`),])
onemonthf <- onemonthf$`pfinal800$subvalue`

sixmonthf <- resultTab[resultTab$`pfinal800$delay`=='in 33 hours',]
sixmonthf <- with(sixmonthf, sixmonthf[order(`pfinal800$odd`),])
sixmonthf <- sixmonthf$`pfinal800$subvalue`

twoyearf <- resultTab[resultTab$`pfinal800$delay`=='in 6 days',]
twoyearf <- with(twoyearf, twoyearf[order(`pfinal800$odd`),])
twoyearf <- twoyearf$`pfinal800$subvalue`

fiveyearf <- resultTab[resultTab$`pfinal800$delay`=='in 2 weeks',]
fiveyearf <- with(fiveyearf, fiveyearf[order(`pfinal800$odd`),])
fiveyearf <- fiveyearf$`pfinal800$subvalue`

results <- rbind(immediatef, onemonthf, sixmonthf, twoyearf, fiveyearf)

results800 <- round(results,2)

# now I'll make a table of values for the 40000 value
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

results20 <- round(results,2)

write.table(results20, "results40000.csv", col.names = FALSE, row.names = FALSE, sep = ",")
write.table(results800, "results800.csv", col.names = FALSE, row.names = FALSE, sep = ",")


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

posthoc_comp<-lsmeans(lme_aov,pairwise~odd*value)
sum_posthoc_comp <- summary(posthoc_comp)
meanresults <- as.data.frame(sum_posthoc_comp$lsmeans)
postresults <- as.data.frame(sum_posthoc_comp$contrasts)

write.csv(meanresults,file = "mean3output.csv")
write.csv(postresults,file = "comp3output.csv")
explore<-cbind(aggregate(pfinal$riskLiteracy~pfinal$pnum,FUN="mean")[2],
               aggregate(pfinal$adjValue~pfinal$pnum,FUN="sum")[2]/50,
               aggregate(pfinal$age~pfinal$pnum,FUN="mean")[2],
               aggregate(as.numeric(pfinal$gender)~pfinal$pnum,FUN = "mean")[2],
               aggregate(as.numeric(pfinal$Gamer)~pfinal$pnum,FUN = "mean")[2])
colnames(explore)[1]="Risk"
colnames(explore)[2]="AUC"
colnames(explore)[3]="age"
colnames(explore)[4]="gender"
colnames(explore)[5]="gamer"
factor(explore$Risk)
factor(explore$gender)
factor(explore$gamer)
explore$gender[explore$gender==1]='Female'
explore$gender[explore$gender==2]='Male'
explore$gender[explore$gender==3]='Other'
explore$gamer[explore$gamer==1]='No'
explore$gamer[explore$gamer==2]='Yes'

explore3<-explore