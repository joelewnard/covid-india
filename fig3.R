
setwd('~/Google drive (jlewnard@berkeley.edu)/covid/india/update data/figure code/for github')
load(file='coxPars.Rdata')
load(file='surv.Rdata')
load(file='cfrDist.Rdata')


##############################
#### from here fast ##########
##############################


agelb = c(0,5,18,30,40,50,65,75,85)

agelabs = c('0-4','5-17','18-29','30-39','40-49','50-64','65-74','75-84','85+')

pdf(file='fig3.pdf',width=6,height=4.25)

layout(matrix(c(1,1,2,2,
                1,1,6,6,
                3,4,4,5),nrow=3,byrow=T),widths=c(1,0.25,0.75,1),heights=c(1.3,0.7,1.25))

par(mar=c(1,1,1,0.5))
par(tck=-0.03); par(mgp=c(3,0.3,0))


round0 = function(x,n){
  out = round(x,n)
  for (i in 1:length(out)){
    if (out[i]==round(x[i],n-1)){
      out[i] = paste(out[i],'0',sep='')
    }
    if (out[i]==round(x[i],n-2)){
      out[i] = paste(out[i],'00',sep='')
    }
  }
  return(out)
}


mat = array(NA,dim=c(12,3))
for (i in 1:12){
  mat[i,] = exp(c(mean(coxPars[,i],na.rm=T),quantile(coxPars[,i],c(0.025,0.975),na.rm=T)))
}

matText = round0(mat,2)
matText[2,] = c('0.044','0.016','0.094')
matText[3,] = c('0.021','0.010','0.034')
matText[4,] = c('0.041','0.033','0.051')

matText = matText[c(2:9,1,11:12,10),]
1-mat[12,]

plot(1,xlim=c(0,80),ylim=c(24,0),axes=F,ann=F,type='n')
mtext(side=3,adj=0,line=0,'A. Predictors of time to death',cex=0.5,font=2,at=-4)
text(x=c(1,30),y=0,c('Exposure','Adjusted hazard ratio'),
     adj=0,cex=0.7,font=2)
text(x=35,y=1,'(95% Conf. int.)',cex=0.7,font=1,adj=0)
text(x=1,y=c(2,13,17,22),adj=0,c('Age group',
                                 'Sex','Date of testing','State'),cex=0.7)
ys = c(3:11,14:15,18:20,23:24)
text(x=5,y=ys,c('0-4 years','5-17 years','18-29 years',
                '30-39 years',
                '40-49 years',
                '50-64 years',
                '65-74 years',
                '75-84 years',
                '85+ years',
                'Female','Male',
                'March 1 to April 30','May 1 to June 30','July 1 to August 1',
                'Andhra Pradesh','Tamil Nadu'),cex=0.7,adj=0)

text(x=45,y=c(8,14,18,23),adj=0.5,'Ref.',cex=0.7)
ys = c(3:7,9:11,15,19:20,24)
for (i in 1:length(ys)){
  text(y=ys[i],x=45,adj=0.5,paste(matText[i,1],' (',matText[i,2],', ',matText[i,3],')',sep=''),cex=0.7)
}
lines(x=c(-1,64),y=rep(-0.5,2),lwd=1)
lines(x=c(-1,64),y=rep(1.5,2),lwd=1)
lines(x=c(-1,64),y=rep(24.5,2),lwd=1)



par(mar=c(1,0.5,1,0.5))
plot(1,type='n',xlim=c(0,40),ylim=c(13,-0.5),axes=F,ann=F)
mtext(side=3,adj=0,line=0,'B. Case fatality ratios',cex=0.5,font=2,at=-3)
text(x=1,y=0,'Age group',adj=0,cex=0.7,font=2)
text(x=26,y=0,'Case fatality ratio (95% Conf. int.), %',cex=0.7,font=2,adj=0.5)
text(x=c(15,26,37),y=rep(1,3),c('All cases','Males','Females'),cex=0.7,font=1,adj=0.5)
#text(x=c(15,26,37),y=rep(2,3),c('(95% Conf. int.)','(95% Conf. int.)','(95% Conf. int)'),cex=0.7,font=1,adj=0.5)
ys = c(3:12)
text(x=1,y=ys,c('0-4 years','5-17 years','18-29 years',
                '30-39 years',
                '40-49 years',
                '50-64 years',
                '65-74 years',
                '75-84 years',
                '85+ years',
                'All ages'),cex=0.7,adj=0,font=c(rep(1,9),2))

mat = array(NA,dim=c(3,10,3))
for (i in 1:3) for (j in 1:10){
  mat[i,j,] = c(mean(cfrDist[i,j,],na.rm=T),
                quantile(cfrDist[i,j,],c(0.025,0.975),na.rm=T))*100
}
#mat[mat<1] = signif(mat[mat<1],2)
matText = round(mat,2)

#mat[1,3,]
matText[1,1,2] = '0'
matText[1,2,] = c('0.054','0.012','0.11')
matText[1,3,3] = '0.20'
matText[1,4,1] = '0.50'
matText[1,7,3] = '10.3'
matText[1,8,] = c('13.0','11.7','14.4')
matText[1,9,] = c('16.6','13.4','19.9')

matText[2,1,] = c('0.20','0','0.50')
matText[2,2,1:2] = c('0.022','0')
matText[2,3,2] = '0.097'
matText[2,7,] = c('11.5','10.6','12.5')
matText[2,8,] = c('16.0','14.1','17.9')
matText[2,9,] = c('20.5','16.1','25.1')

matText[3,1,2] = '0'
matText[3,2,] = c('0.093','0','0.20')
matText[3,7,3] = '7.60'
matText[3,8,3] = '10.4'
matText[3,9,3] = '15.6'

xs = c(15,26,37)
text(x=45,y=c(8,14,18,23),adj=0.5,'Ref.',cex=0.7)
for (i in 1:3){
  for (j in 1:10){
    text(y=ys[j],x=xs[i],adj=0.5,paste(matText[i,j,1],' (',matText[i,j,2],', ',matText[i,j,3],')',sep=''),cex=0.7,
         font=(ifelse(j==10,2,1)))
  }
}
lines(x=c(-1,43),y=rep(-0.5,2),lwd=1)
lines(x=c(-1,43),y=rep(1.5,2),lwd=1)
lines(x=c(-1,43),y=rep(12.5,2),lwd=1)

labs = c('C. All cases (30d follow-up cohort)',
         'D. Male cases (30d follow-up cohort)',
         'E. Female cases (30d follow-up cohort)'); 
par(mar=c(2.25,2.5,1,0.5)); par(tck=-0.02)

for (j in 1:3){
  plot(1,type='n',ylim=c(0.76,1),xlim=c(0,30),axes=F,ann=F)
  for (i in 1:length(agelb)){
    x = c(0,0,rep(1:29,each=2),30)
    y = c(0,rep(1-surv[j,i,],each=2))
    lines(1-y[1:60]~x[1:60],
          col=rgb((i-1)/(length(agelb)-1),0,1-(i-1)/(length(agelb)-1),1),lwd=0.75)
  }
  # box(bty='l',lwd=0.5)
  axis(side=1,at=c(-100,seq(0,30,5)),cex.axis=0.7,lwd=0.5,lwd.ticks=0.5,labels=NA)
  text(x=seq(0,30,5),y=0.735,srt=45,seq(0,30,5),xpd=T,cex=0.7,adj=1)
  axis(side=2,at=c(-100,0.76,0.775),labels=c(NA,0,NA),cex.axis=0.7,las=1,lwd=0.5,lwd.ticks=0.5)
  axis(side=2,at=c(0.78,seq(0.8,1,0.05)),labels=c(NA,seq(80,100,5)),cex.axis=0.7,las=1,lwd=0.5,lwd.ticks=0.5)
  mtext(side=3,adj=0,labs[j],font=2,cex=0.5,at=-7)
  mtext(side=2,'Surviving (%)',cex=0.5,line=1.5)
  mtext(side=1,'Days from testing',cex=0.5,line=1.25)
}

agelabs = c('Ages 0-4 years','Ages 5-17 years','Ages 18-29 years','Ages 30-39 years',
            'Ages 40-49 years','Ages 50-64 years','Ages 65-74 years','Ages 75-84 years','Ages 85+ years')
par(mar=c(0,0,0,0))
plot(ylim=c(0,100),xlim=c(0,100),1,type='n',axes=F,ann=F)
for (i in 1:4){
  lines(x=c(0,10),y=rep(90-i*12,2),col=rgb((i-1)/(length(agelabs)-1),0,1-(i-1)/(length(agelabs)-1),1),lwd=0.75)
  text(x=12,y=90-i*12,agelabs[i],adj=0,cex=0.7,font=3)
}
for (i in 5:9){
  lines(x=c(40,50),y=rep(90-(i-4)*12,2),col=rgb((i-1)/(length(agelabs)-1),0,1-(i-1)/(length(agelabs)-1),1),lwd=0.75)
  text(x=52,y=90-(i-4)*12,agelabs[i],adj=0,cex=0.7,font=3)
}

dev.off()







