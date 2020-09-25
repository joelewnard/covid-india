
#############################################
### analysis ################################
#############################################

setwd('~/Google drive (jlewnard@berkeley.edu)/covid/india/update data/figure code/for github')

load('traceDatSaved.Rdata')

traceDat = traceDatSaved

traceDat$cSexB = traceDat$cSex; traceDat$primSexB = traceDat$primSex
traceDat$cSexB[traceDat$cSex=='T'] = NA; traceDat$primSexB[traceDat$primSex=='T'] = NA


agelb = c(seq(0,80,5))
ageub = c(seq(5,80,5),200)

agelbA = c(seq(0,20,5),40,65,80)
ageubA = c(seq(5,20,5),40,65,80,200)

traceDat$cAgeGrp = traceDat$primAgeGrp = traceDat$cAgeGrpA = traceDat$primAgeGrpA = NA
for (i in 1:length(agelb)){
  traceDat$cAgeGrp[traceDat$cAge>=agelb[i]&traceDat$cAge<ageub[i]] = i
  traceDat$primAgeGrp[traceDat$primAge>=agelb[i]&traceDat$primAge<ageub[i]] = i
  
  traceDat$cAgeGrpA[traceDat$cAge>=agelbA[i]&traceDat$cAge<ageubA[i]] = i
  traceDat$primAgeGrpA[traceDat$primAge>=agelbA[i]&traceDat$primAge<ageubA[i]] = i
}

prob0 = prob1 = prob2 = prob1N = prob2N = prob1X = prob2X = matrix(NA,length(agelb),length(agelb))
prop1 = prop2 = matrix(NA,length(agelb),length(agelb))
for (i in 1:length(agelb)) for (j in 1:length(agelb)){
  prob0[i,j] = mean(traceDat$cPos[traceDat$cAgeGrp==i&traceDat$primAgeGrp==j],na.rm=T)
  prob1[i,j] = mean(traceDat$cPos[traceDat$cAgeGrp==i&traceDat$primAgeGrp==j&traceDat$deg==1],na.rm=T)
  prob2[i,j] = mean(traceDat$cPos[traceDat$cAgeGrp==i&traceDat$primAgeGrp==j&traceDat$deg==2],na.rm=T)

  prob1X[i,j] = sum(traceDat$cPos[traceDat$cAgeGrp==i&traceDat$primAgeGrp==j&traceDat$deg==1],na.rm=T)
  prob2X[i,j] = sum(traceDat$cPos[traceDat$cAgeGrp==i&traceDat$primAgeGrp==j&traceDat$deg==2],na.rm=T)
  prob1N[i,j] = sum(traceDat$cAgeGrp==i&traceDat$primAgeGrp==j&traceDat$deg==1&is.na(traceDat$cPos)==F,na.rm=T)
  prob2N[i,j] = sum(traceDat$cAgeGrp==i&traceDat$primAgeGrp==j&traceDat$deg==2&is.na(traceDat$cPos)==F,na.rm=T)
}

prob1Na = prob2Na = prob1Xa = prob2Xa = matrix(NA,length(agelbA),length(agelbA))
for (i in 1:length(agelbA)) for (j in 1:length(agelbA)){
  prob1Xa[i,j] = sum(traceDat$cPos[traceDat$cAgeGrpA==i&traceDat$primAgeGrpA==j&traceDat$deg==1],na.rm=T)
  prob2Xa[i,j] = sum(traceDat$cPos[traceDat$cAgeGrpA==i&traceDat$primAgeGrpA==j&traceDat$deg==2],na.rm=T)
  prob1Na[i,j] = sum(traceDat$cAgeGrpA==i&traceDat$primAgeGrpA==j&traceDat$deg==1&is.na(traceDat$cPos)==F,na.rm=T)
  prob2Na[i,j] = sum(traceDat$cAgeGrpA==i&traceDat$primAgeGrpA==j&traceDat$deg==2&is.na(traceDat$cPos)==F,na.rm=T)
}

probVals1 = probVals2 = relVals = array(NA,dim=c(length(agelbA),length(agelbA),3))
for (i in 1:length(agelbA)) for (j in 1:length(agelbA)){
  probVals1[i,j,] = qbeta(c(0.5,0.025,0.975),prob1Xa[i,j],prob1Na[i,j]-prob1Xa[i,j])
  probVals2[i,j,] = qbeta(c(0.5,0.025,0.975),prob2Xa[i,j],prob2Na[i,j]-prob2Xa[i,j])
}



modHR = glm(cPos~cSexB*primSexB+state+as.factor(primAgeGrpA)*as.factor(cAgeGrpA),
            family='poisson',subset=(deg==1),data=traceDat)
modLR = glm(cPos~cSexB*primSexB+state+as.factor(primAgeGrpA)*as.factor(cAgeGrpA),
            family='poisson',subset=(deg==2),data=traceDat)

library(MASS)
set.seed(1)
parsHR = mvrnorm(1e4,coef(modHR),vcov(modHR))
parsLR = mvrnorm(1e4,coef(modLR),vcov(modLR))

mmHR = (parsHR[,1]+parsHR[,2]+parsHR[,3]+parsHR[,19] + parsHR[,8]+parsHR[,15]+parsHR[,44])
mfHR = (parsHR[,1]+parsHR[,3] + parsHR[,8]+parsHR[,15]+parsHR[,44])
fmHR = (parsHR[,1]+parsHR[,2] + parsHR[,8]+parsHR[,15]+parsHR[,44])
ffHR = (parsHR[,1] + parsHR[,8]+parsHR[,15]+parsHR[,44])

mmLR = (parsLR[,1]+parsLR[,2]+parsLR[,3]+parsLR[,19] + parsLR[,8]+parsLR[,15]+parsLR[,44])
mfLR = (parsLR[,1]+parsLR[,3] + parsLR[,8]+parsLR[,15]+parsLR[,44])
fmLR = (parsLR[,1]+parsLR[,2] + parsLR[,8]+parsLR[,15]+parsLR[,44])
ffLR = (parsLR[,1] + parsLR[,8]+parsLR[,15]+parsLR[,44])


######## frequencies of number of secondary contacts and positive secondary contacts

tabTot = table(table(traceDat$id))
tot = c(0,tabTot[1:80],0,0,sum(tabTot[81:length(tabTot)]))

tabTotPos = table(table(traceDat$id[traceDat$cPos==1]))
totPos = rep(0,41); totPos[as.numeric(names(tabTotPos))+1] = tabTotPos

transm = unique(traceDat$id[traceDat$cPos==1])
nontransm = unique(traceDat$id[which((traceDat$id%in%transm)==F)])
tabTotPos = c(length(nontransm),tabTotPos)
names(tabTotPos)[1] = '0'

##### neg binom thing

contactsTot = rep(as.numeric(names(tabTot)),tabTot)
contactsPos = rep(as.numeric(names(tabTotPos)),tabTotPos)

library(fitdistrplus)
try = fitdist(contactsPos,distr='nbinom')
set.seed(1); disp = mvrnorm(1e5,try$estimate,vcov(try)); try$estimate; quantile(disp[,1],c(0.025,0.975))


##### neg binom thing

contactsTot = rep(as.numeric(names(tabTot)),tabTot)
contactsPos = rep(as.numeric(names(tabTotPos)),tabTotPos)

library('fitdistrplus')
try = fitdist(contactsPos,distr='nbinom')
set.seed(1); disp = mvrnorm(1e5,try$estimate,vcov(try)); try$estimate; quantile(disp[,1],c(0.025,0.975))


vals = 0:40
xquant = c(); for (i in 1:length(vals)){xquant[i] = mean(contactsPos<=vals[i])}
yquant = c(); for (i in 1:length(vals)){yquant[i] = sum(contactsPos[contactsPos<=vals[i]])/sum(contactsPos)}

  
xs = c(0.61,rep(xquant[1:(length(xquant)-1)],each=2),xquant[length(xquant)])
ys = c(0,yquant[1],rep(yquant[2:(length(yquant)-1)],each=2),yquant[length(yquant)],1)



set.seed(1)
posContact = sexContact = sexTot = posTot = rep(NA,1e4)
for (i in 1:1e3){
  sel = sample(which(traceDat$cPos==1),sum(traceDat$cPos,na.rm=T),replace=T)
  posContact[i] = mean(traceDat$cAge[sel],na.rm=T)
  sexContact[i] = mean(traceDat$cSex[sel]=='M',na.rm=T)
  
  sel = sample(which(analysisDat$daymed<=ymd('2020-08-01')),sum(analysisDat$daymed<=ymd('2020-08-01')),replace=T)
  posTot[i] = mean(analysisDat$age[sel],na.rm=T)
  sexTot[i] = mean(analysisDat$sex[sel]=='M',na.rm=T)
  print(i)
}
mean(posContact-posTot,na.rm=T); quantile(posContact-posTot,c(0.025,0.975),na.rm=T)
mean((sexContact-sexTot)/sexTot,na.rm=T); quantile((sexContact-sexTot)/sexTot,c(0.025,0.975),na.rm=T)
sum(analysisDat$daymed<=as.numeric(ymd('2020-07-01'))&analysisDat$state=='ap')



