setwd('~/Google drive (jlewnard@berkeley.edu)/covid/india/update data/figure code/for github')

##############################
#### from here fast ##########
##############################

agelb = c(0,5,18,30,40,50,65,75,85)
ageub = c(5,18,30,40,50,65,75,85,200)

popReg = c(8496750,8244591,8804037,9886046,10770281,11262480,
           10470966,10312661,9708644,9445234,
           8143502,6882501,5506989,
           4049114,2876560,
           1794455,1217108)
popRegM = c(4440469,4306888,4608239,5126774,5481605,5736081,5424737,5135102,4886857,4716046,3961524,3381159,2653865,1935495,1343961,837801,487581)
popRegF = popReg - popRegM

popUs = c(19736,20212,20827,20849,21254,23277,21932,21443,19584,20345,20355,21163,20592,17356,14131,9357,6050,5893)
popUsM = c(10094,10328,10650,10545,10716,11792,10935,10629,9628,9993,9930,10046,9818,8198,6691,4233,2519,2282)
popUsF = popUs-popUsM

uscasedat = read.csv('cdc data 8_17_20.csv',header=T)
agelb = c(0,5,18,30,40,50,65,75,85); ageub = c(5,18,30,40,50,65,75,85,200) ### CDC age groups
propCaseUs = uscasedat$cases[1:9]
propDeathUs = uscasedat$deaths[1:9]
uscases = uscasedat$cases[11]
usdeaths = uscasedat$deaths[11]


popReg = c(popReg[1], ## 0-4
           sum(popReg[2:3])+0.6*popReg[4], ## 5-9, 10-14, 15-17
           0.4*popReg[4]+sum(popReg[5:6]), ## 18-19, 20-24, 25-29
           sum(popReg[7:8]), ## 30-34, 35-39
           sum(popReg[9:10]), ## 40-44, 45-49
           sum(popReg[11:13]), ## 50-54, 55-59, 60-64
           sum(popReg[14:15]), ## 65-69, 70-74
           popReg[16]+0.5*popReg[17], ## 75-79, 80-84
           0.5*popReg[17])

popUs = c(popUs[1], ## 0-4
          sum(popUs[2:3])+0.6*popUs[4], ## 5-9, 10-14, 15-17
          0.4*popUs[4]+sum(popUs[5:6]), ## 18-19, 20-24, 25-29
          sum(popUs[7:8]), ## 30-34, 35-39
          sum(popUs[9:10]), ## 40-44, 45-49
          sum(popUs[11:13]), ## 50-54, 55-59, 60-64
          sum(popUs[14:15]), ## 65-69, 70-74
          sum(popUs[16:17]), ## 75-79, 80-84
          popUs[18])*1e3


load('casedat.Rdata')
for (i in 1:dim(casedat)[2]){
  assign(names(casedat)[i],casedat[,i])
}

########################################################
#### load in deaths not linked to cases already seen ###
########################################################

load('deathsDat.Rdata')
for (i in 1:dim(deathsDat)[2]){
  assign(names(deathsDat)[i],deathsDat[,i])
}


propCaseReg = propDeathReg = c()
for (i in 1:length(agelb)){
  propCaseReg[i] = sum(agegrp==i&daymed<=as.numeric(ymd('2020-08-01')),na.rm=T)
  propDeathReg[i] = sum(deathAgeGrp==i&deathDate<=as.numeric(ymd('2020-08-01')),na.rm=T)
}
propCaseUs = c(66110,246818,875795,682207,628649,853058,312648,181220,138736)
propDeathUs = c(31,44,651,1701,4139,20264,27226,33989,41026)
incReg = propCaseReg/popReg;# incReg = incReg/incReg[3]
incUs = propCaseUs/popUs; #incUs = incUs/incUs[3]

1e4*incReg
1e4*incUs

popUs
incUs/incUs[7]
incReg/incReg[7]

mortReg = propDeathReg/popReg; #mortReg = mortReg/mortReg[3]
mortUs = propDeathUs/popUs# mortUs = mortUs/mortUs[3]


setwd('~/Dropbox/covid india/revision/figures')

barplot.fn = function(objReg,objUs,yub,yby,let,lab){
  plot(1,ylim=c(0,yub),xlim=c(1,9.6),axes=F,ann=F)
  for (i in 1:9){
    polygon(x=i+c(0,0,0.3,0.3),y=c(0,rep(objReg[i]/sum(objReg),2),0),lty=0,col='darkslategray3')
    polygon(x=i+c(0.3,0.3,0.6,0.6),y=c(0,rep(objUs[i]/sum(objUs),2),0),lty=0,col='darkorchid3')
    lines(x=i+c(0,0,0.3,0.3),y=c(0,rep(objReg[i]/sum(objReg),2),0),lwd=0.5)
    lines(x=i+c(0.3,0.3,0.6,0.6),y=c(0,rep(objUs[i]/sum(objUs),2),0),lwd=0.5)
  }
  box(bty='l',lwd=0.5)
  axis(side=2,at=seq(0,yub,yby),labels=seq(0,yub,yby)*1e2,cex.axis=0.65,las=1,lwd=0,lwd.ticks=0.5)
  axis(side=1,at=1:10-0.2,lwd=0,lwd.ticks=0.5,labels=NA)
  text(y=-0.1*yub,x=1:9+0.3,agelabs,cex=0.65,srt=45,adj=1,xpd=T)
  mtext(side=2,'Proportion, %',cex=0.5,line=1.25)
  mtext(side=1,'Age group, y',cex=0.5,line=1.5)
  mtext(side=3,paste(let,') ',lab,sep=''),adj=0,at=-1,font=2,line=0.25,cex=0.5)
  #axis(side=1,at=1:9+0.6,lwd=0,lwd.ticks=0.5,labels=NA)
}





agelabs = c('0-4','5-17','18-29','30-39','40-49','50-64','65-74','75-84','85+')

pdf(file='fig4b.pdf',width=3,height=3.5)

layout(matrix(c(1,4,
                2,5,
                3,6),nrow=3,byrow=T),widths=c(1,1),heights=c(1,1,1))

par(mar=c(3,2,1,0.5))
par(tck=-0.03); par(mgp=c(3,0.25,0))


barplot.fn(popReg,popUs,0.25,0.05,'A','Population age distribution')
barplot.fn(propCaseReg,propCaseUs,0.3,0.06,'B','Reported case age distribution')
barplot.fn(propDeathReg,propDeathUs,0.4,0.08,'D','Reported death age distribution')

par(mar=c(0,2.5,0,0))
plot(1,axes=F,ann=F,type='n',xlim=c(0,100),ylim=c(0,100))
polygon(x=c(0,30,30,0),y=c(72,72,76,76),col='darkslategray3',lty=0)
lines(x=c(0,30,30,0,0),y=c(72,72,76,76,72),lwd=0.5)
lines(x=c(0,30),y=c(68,68),col='darkslategray4',lwd=0.75)
points(x=seq(5,25,10),y=rep(68,3),cex=0.75,col='darkslategray4',lwd=0.75,pch=21,bg='white')
text(x=40,y=72,'Andhra Pradesh\nand Tamil Nadu',cex=0.65,font=3,adj=0)

polygon(x=c(0,30,30,0),y=c(40,40,36,36),col='darkorchid3',lty=0)
lines(x=c(0,30,30,0,0),y=c(40,40,36,36,40),lwd=0.5)
lines(x=c(0,30),y=c(32,32),col='darkorchid4',lwd=0.75)
points(x=seq(5,25,10),y=rep(32,3),cex=0.75,col='darkorchid4',lwd=0.75,pch=21,bg='white')
text(x=40,y=36,'United States',cex=0.65,font=3,adj=0)

par(tck=-0.035)

par(mar=c(3,2.5,1,0))
plot(log10(incReg),ylim=c(-4,-1),xlim=c(0.5,9.5),col='darkslategray4',lwd=1,type='l',axes=F,ann=F)
points(log10(incReg),bg='white',col='darkslategray4',lwd=0.75,pch=21,cex=0.75)
lines(log10(incUs),col='darkorchid4',lwd=1)
points(log10(incUs),bg='white',col='darkorchid4',lwd=0.75,pch=21,cex=0.75)
box(bty='l',lwd=0.5)
axis(side=2,at=seq(-4,-1,1),labels=c(1,10,100,1000),lwd=0,lwd.ticks=0.5,cex.axis=0.65,las=1)
axis(side=2,at=log10(c(seq(2e-3,9e-3,1e-3),
                       seq(2e-2,9e-2,1e-2),
                       seq(2e-1,9e-1,1e-1))),lwd=0,lwd.ticks=0.5,labels=NA,tck=-0.02) 

axis(side=1,at=c(1:9),labels=NA,lwd=0,lwd.ticks=0.5)
mtext(side=2,expression(paste('Cases per ',10^4,sep='')),cex=0.5,line=1.5)
mtext(side=3,'C) Incidence trend by age',adj=0,at=-2.1,font=2,line=0.25,cex=0.5)
text(x=1:9,y=-4.3,xpd=T,srt=45,adj=1,agelabs,cex=0.65)
mtext(side=1,'Age group, y',cex=0.5,line=1.5)

plot(log10(mortReg),ylim=c(-7,-2),xlim=c(0.5,9.5),col='darkslategray4',lwd=1,type='l',axes=F,ann=F)
points(log10(mortReg),bg='white',col='darkslategray4',lwd=0.75,pch=21,cex=0.75)
lines(log10(mortUs),col='darkorchid4',lwd=1)
points(log10(mortUs),bg='white',col='darkorchid4',lwd=0.75,pch=21,cex=0.75)
box(bty='l',lwd=0.5)
axis(side=2,at=seq(-7,-2,1),labels=c(0.001,0.01,0.1,1,10,100),lwd=0.5,lwd.ticks=0.5,cex.axis=0.65,las=1)
axis(side=1,at=c(-100,1:9),labels=NA,lwd=0.5,lwd.ticks=0.5)
mtext(side=2,expression(paste('Deaths per ',10^4,sep='')),cex=0.5,line=1.5)
mtext(side=3,'E) Mortality trend by age',adj=0,at=-2.1,font=2,line=0.25,cex=0.5)
text(x=1:9,y=-7.5,xpd=T,srt=45,adj=1,agelabs,cex=0.65)
mtext(side=1,'Age group, y',cex=0.5,line=1.5)


dev.off()
