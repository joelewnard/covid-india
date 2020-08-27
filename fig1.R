

library(zoo)
library(rgdal)
library(broom)


library(raster)
library(ggplot2)
library(lubridate)

library(maptools)
library(sp)

india = getData('GADM', country='IND', level=0)
states = getData('GADM', country='IND', level=1)
districts = getData('GADM', country='IND', level=2)
#
# Convert the polygons into data frames so we can make lines
poly2df <- function(poly) {
  # Convert the polygons into data frames so we can make lines
  # Number of regions
  n_regions <- length(poly@polygons)
  
  # Get the coords into a data frame
  poly_df <- c()
  for(i in 1:n_regions) {
    # Number of polygons for first region
    n_poly <- length(poly@polygons[[i]]@Polygons)
    print(paste("There are",n_poly,"polygons"))
    # Create progress bar
    pb <- txtProgressBar(min = 0, max = n_poly, style = 3)
    for(j in 1:n_poly) {
      poly_df <- rbind(poly_df, NA, 
                       poly@polygons[[i]]@Polygons[[j]]@coords)
      # Update progress bar
      setTxtProgressBar(pb, j)
    }
    close(pb)
    print(paste("Finished region",i,"of",n_regions))
  }
  poly_df <- data.frame(poly_df)
  names(poly_df) <- c('lon','lat')
  return(poly_df)
}
getSmallPolys <- function(poly, minarea=0.01) {
  # Get the areas
  areas <- lapply(poly@polygons, 
                  function(x) sapply(x@Polygons, function(y) y@area))
  
  # Quick summary of the areas
  print(quantile(unlist(areas)))
  
  # Which are the big polygons?
  bigpolys <- lapply(areas, function(x) which(x > minarea))
  length(unlist(bigpolys))
  
  # Get only the big polygons and extract them
  for(i in 1:length(bigpolys)){
    if(length(bigpolys[[i]]) >= 1 && bigpolys[[i]] >= 1){
      poly@polygons[[i]]@Polygons <- poly@polygons[[i]]@Polygons[bigpolys[[i]]]
      poly@polygons[[i]]@plotOrder <- 1:length(poly@polygons[[i]]@Polygons)
    }
  }
  return(poly)
}

indiaReduce = getSmallPolys(india,minarea=1)
indiaDF = poly2df(indiaReduce)

statesReduce = getSmallPolys(states,minarea=1)
statesDF = poly2df(statesReduce)

ap = statesReduce[statesReduce$NAME_1=='Andhra Pradesh',]
tn = statesReduce[statesReduce$NAME_1=='Tamil Nadu',]

districts = districts[districts$NAME_1%in%c('Andhra Pradesh','Tamil Nadu'),]

apDF = poly2df(ap)
tnDF = poly2df(tn)
districtsDF = poly2df(districts)


distnames = districts$NAME_2

distDFs = list()
for (i in 1:length(distnames)){
  dat = districts[districts$NAME_2==distnames[i],]
  distDFs[[i]] = poly2df(dat)
}



distpops = c(4157545,4252200,5250758,4979293,4601955,4129339,3019049,3461058,
             2753734,4370898,2388380,4010667,2936443, ### all ap here
             811540, ### ariyalur
             4995398, ### chennai
             3717522, ### coimbatore
             2801447, ### cuddalore
             1619910, ### dharmapuri
             2321834, ### dindigul
             2420705, ### erode
             4298262, ### kancheepuram
             2010719, ### kanniyakumari
             1144369, ### karur
             2020863, ### krishnagiri
             3266226, ### madurai
             1737742, ### nagapatinam
             1856155, ### namakkal
             607634, ### perambular
             1739777, ### pudokottai
             1455004, ### ramananthapuram
             3743332, ### salem
             1439580, ### sivaganga
             2586418, ### thanjavur
             790576, ### nilgiri
             1339385, ### theni
             4007844, ### tiruvallur
             1359139, ### thiruvarur
             1881504, ### thoothukkudi
             2926558, ### thiruchirappalli
             3308134, ### tirunelveli
             2665069, ### tiruppur
             2649825, ### tiruvannamali
             4231690, ### vellore
             3718410,### villuppuram
             2088029) ### virudhunagar

totinc = array(NA,dim=dim(totcas))
for (j in 1:4){
  totinc[,j] = totcas[,j]/distpops
}

totinc[,3] = totinc[,3]-totinc[,2]
totinc[,4] = totinc[,4]-totinc[,3]

####### lines for new cases
days = as.numeric(ymd('2020-03-01')):as.numeric(ymd('2020-08-01'))

months = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
days = dmy('1-3-2020')+0:180
daylabs = paste(day(days),' ',months[month(days)],sep='')



setwd('~/Google drive (jlewnard@berkeley.edu)/covid/india/update data/figure code/for github')


popAp = sum(distpops[1:13])
popTn = sum(popReg) - popAp

setwd('~/Google drive (jlewnard@berkeley.edu)/covid/india/update data')
load('totcas.Rdata')
load('testAvg.Rdata')
load('posAvg.Rdata')
load('testTot.Rdata')
load('posTot.Rdata')
load('newCas.Rdata')
load('totCas.Rdata')
load('newDeaths.Rdata')
load('totDeaths.Rdata')
load('casAvg.Rdata')
load('deathsAvg.Rdata')



setwd('~/Dropbox/covid india/revision/figures')

labtext = c(NA,'A) Incidence through 31 May, 2020','B) Incidence 1-30 June, 2020',
            'C) Incidence 1-31 July, 2020')

pdf(file='fig1.pdf',width=6,height=4.25)

layout(matrix(c(1,1,1,1,2,2,2,2,3,3,3,3,
                4,4,4,7,7,5,5,5,6,6,6,6,
                4,4,4,8,8,5,5,5,6,6,6,6),nrow=3,ncol=12,byrow=T),
       heights=c(2.5,0.75,1))

for (j in 2:4){
  par(mar=c(0,0.25,1,0))
  plot(districtsDF,type='n',axes=F,ann=F)
  mtext(side=3,labtext[j],line=0,at=75.75,cex=0.5,font=2,adj=0)
  
  for (i in 1:length(distnames)){
    polygon(distDFs[[i]],
            col=rgb((totinc[i,j]/max(totinc))^0.25,(totinc[i,j]/max(totinc))/6,(1-totinc[i,j]/max(totinc))/6,
                    (totinc[i,j]/max(totinc))^0.35),lty=0)
  }
  lines(districtsDF,col='white',lwd=0.5)
  lines(tnDF,col='black',lwd=0.5)
  lines(apDF,col='black',lwd=0.5)
  if (j==2){
    lines(x=c(80.75,80.3),y=c(13,13.025),lwd=0.5)# chennai
    text(x=80.8,y=13,'Chennai',cex=0.7,adj=0,xpd=T,font=3) 
    
    lines(x=c(80.1,78.95),y=c(11.2,11.25),lwd=0.5)## ariyalur
    text(x=80.15,y=11.2,'Perambalur',cex=0.7,adj=0,xpd=T,font=3) 
    
    lines(x=c(80.1,79.15),y=c(10.75,11.1),lwd=0.5)## ariyalur
    text(x=80.15,y=10.7,'Ariyalur',cex=0.7,adj=0,xpd=T,font=3) 
    
    lines(x=c(80.6,80),y=c(12.5,12.6),lwd=0.5) ## kancheepuram
    text(x=80.65,y=12.5,'Kancheepuram',cex=0.7,adj=0,xpd=T,font=3)
    lines(x=c(80.75,80.15),y=c(13.4,13.25),lwd=0.5) # tiruvallur
    text(x=80.8,y=13.4,'Tiruvallur',cex=0.7,adj=0,xpd=T,font=3)
    lines(x=c(78,78.3),y=c(16.3,15.8),lwd=0.5) ## kurnool
    text(x=77,y=16.5,'Kurnool',cex=0.7,adj=0,xpd=T,font=3)
    
    lines(x=c(80.2,79.75),y=c(12.05,12.1),lwd=0.5) ## villuppuram
    text(x=80.25,y=12.05,'Villuppuram',cex=0.7,adj=0,xpd=T,font=3)
    
    lines(x=c(80.1,79.65),y=c(11.65,11.6),lwd=0.5) ## cuddalore
    text(x=80.15,y=11.65,'Cuddalore',cex=0.7,adj=0,xpd=T,font=3)
    
    lines(x=c(77.8,77.7),y=c(8,8.35),lwd=0.5) ## tirunelveli
    text(x=77.9,y=8,'Tirunelveli',cex=0.7,adj=0,xpd=T,font=3)
    
    lines(x=c(78.45,78),y=c(8.5,8.7),lwd=0.5) ### thoothukudi
    text(x=78.5,y=8.5,'Toothukudi',cex=0.7,adj=0,xpd=T,font=3)
    
    lines(x=c(81.4,81),y=c(15.9,16.3),lwd=0.5) ## krishna
    text(x=81.5,y=15.9,'Krishna',cex=0.7,adj=0,xpd=T,font=3)
    
    lines(x=c(80.7,80.6),y=c(15.4,16.1),lwd=0.5) ## guntur
    text(x=80.8,y=15.4,'Guntur',cex=0.7,adj=0,xpd=T,font=3)
    
    text(x=84.08,y=14.5,'Anant',cex=0.7,adj=0,xpd=T,font=3)
  }
  if (j==3){
    lines(x=c(76.6,77.25),y=c(14.5,14.75),lwd=0.5) ## anantapur
    text(x=75.73,y=14.4225,'apur',cex=0.7,adj=0,xpd=T,font=3)
    
    lines(x=c(80.15,79.5),y=c(12.1,12.5),lwd=0.5) ## tiruvannamali
    text(x=80.2,y=12.1,'Tiruvannamalai',cex=0.7,adj=0,xpd=T,font=3)
    
    lines(x=c(78.3,78.8),y=c(13.1,12.85),lwd=0.5) ## vellore
    text(x=77,y=13.15,'Vellore',cex=0.7,adj=0,xpd=T,font=3)
    
    lines(x=c(80.65,79.75),y=c(15.2,15.6),lwd=0.5) ## chittoor
    text(x=80.75,y=15.2,'Prakasam',cex=0.7,adj=0,xpd=T,font=3)
    
    lines(x=c(80.55,79),y=c(14.8,14.5),lwd=0.5) ## chittoor
    text(x=80.65,y=14.8,'YSR (Kadapa)',cex=0.7,adj=0,xpd=T,font=3)
    
    lines(x=c(80.55,79.75),y=c(14.4,14.1),lwd=0.5) ## chittoor
    text(x=80.65,y=14.4,'Nellore',cex=0.7,adj=0,xpd=T,font=3)
    
    lines(x=c(80.4,79.7),y=c(14,13.6),lwd=0.5) ## chittoor
    text(x=80.5,y=14,'Chittoor',cex=0.7,adj=0,xpd=T,font=3)
    
    lines(x=c(78.5,78.7),y=c(9.3,8.8),lwd=0.5) ### ramanathapuram
    text(x=78.8,y=8.8,'Ramanathapuram',cex=0.7,adj=0,xpd=T,font=3)    
    
    lines(x=c(77.9,78.3),y=c(9.9,8.4),lwd=0.5) ### madurai
    text(x=78.4,y=8.4,'Madurai',cex=0.7,adj=0,xpd=T,font=3)
    
    lines(x=c(79.4,78.7),y=c(9.9,10.9),lwd=0.5)
    text(x=79.5,y=9.9,'Tiruchirappali',cex=0.7,adj=0,xpd=T,font=3)
    
    lines(x=c(79.4,78.5),y=c(9.35,9.9),lwd=0.5) 
    text(x=79.5,y=9.35,'Sivagangai',cex=0.7,adj=0,xpd=T,font=3)
    
    lines(x=c(80.1,79.5),y=c(10.7,10.7),lwd=0.5)## ariyalur
    text(x=80.15,y=10.7,'Tiruvarur',cex=0.7,adj=0,xpd=T,font=3) 
    
    lines(x=c(77.25,78),y=c(12.1,11.7),lwd=0.5) 
    text(x=76,y=12.1,'Salem',cex=0.7,adj=0,xpd=T,font=3)
    
    text(x=83.61,y=11,'Coimbatore',cex=0.7,adj=0,xpd=T,font=3)
    
    lines(x=c(81.15,81.4),y=c(17.75,17.1),lwd=0.5)
    text(x=81,y=17.75,'West\nGodavari',cex=0.7,adj=1,xpd=T,font=3)
    
    lines(x=c(77.1,77.5),y=c(9.9,9.9),lwd=0.5)
    text(x=75.9,y=9.9,'Theni',cex=0.7,adj=0,xpd=T,font=3)
    
    lines(x=c(84,84),y=c(19.25,18.5),lwd=0.5)
    text(x=83,y=19.5,'Srikakulam',cex=0.7,adj=0,xpd=T,font=3) 
    
    text(x=84.35,y=12.84,'Dha',cex=0.7,adj=0,xpd=T,font=3)
  }
  if (j==4){
    lines(x=c(76.4,76.9),y=c(11,11),lwd=0.5) 
    text(x=75.69,y=10.975,'ore',cex=0.7,adj=0,xpd=T,font=3)
    
    lines(x=c(76.25,76.5),y=c(12,11.5),lwd=0.5) 
    text(x=75.75,y=12.25,'Nilgiris',cex=0.7,adj=0,xpd=T,font=3)
    
    text(x=76.1,y=13.3,'Krishnagiri',cex=0.7,adj=0,xpd=T,font=3)
    lines(x=c(77.8,77.9),y=c(13.05,12.65),lwd=0.5)
    
    text(x=75,y=12.8,'Dharmapuri',cex=0.7,adj=0,xpd=T,font=3)
    lines(x=c(77.2,78),y=c(12.8,12.1),lwd=0.5)
    
    lines(x=c(78.3,78),y=c(8.4,9.4),lwd=0.5) 
    text(x=78.4,y=8.4,'Virudhunagar',cex=0.7,adj=0,xpd=T,font=3)
    
    lines(x=c(79.4,78.4),y=c(9.9,10.5),lwd=0.5) 
    text(x=79.5,y=9.9,'Karur',cex=0.7,adj=0,xpd=T,font=3)
    
    lines(x=c(79.4,77.75),y=c(9.5,10.35),lwd=0.5) 
    text(x=79.5,y=9.5,'Dindigul',cex=0.7,adj=0,xpd=T,font=3)
    
    lines(x=c(82.65,82),y=c(16.5,16.85),lwd=0.5)
    text(x=82.75,y=16.5,'East\nGodavari',cex=0.7,adj=0,xpd=T,font=3)
    
    lines(x=c(82.1,82.5),y=c(18.5,18),lwd=0.5)
    text(x=82,y=18.5,'Visakhapatnam',cex=0.7,adj=1,xpd=T,font=3) 
    
    lines(x=c(82.9,83.4),y=c(19,18.6),lwd=0.5)
    text(x=82.8,y=19,'Vizianagaram',cex=0.7,adj=1,xpd=T,font=3) 
    
    lines(x=c(76.5,77.3),y=c(8.1,8.3),lwd=0.5) 
    text(x=76,y=7.8,'Kanyakumari',cex=0.7,adj=0,xpd=T,font=3)
  }
}

levs = seq(-6.2,-1.8,0.1)
for (i in 1:length(levs)){
  polygon(x=c(83.75,84.5,84.5,83.75),y=8+c(i,i,i-1,i-1)*0.12,
          #col=rgb(levs[i]^0.5,levs[i]/4,0,)
          col=rgb(((10^levs[i])/(10^max(levs)))^0.25,((10^levs[i])/(10^max(levs)))/6,
                  (1-(10^levs[i])/(10^max(levs)))/6,
                  ((10^levs[i])/(10^max(levs)))^0.35),lty=0)
}

lines(x=c(83.75,84.5,84.5,83.75,83.75),y=8+c(1,1,length(levs),length(levs),1)*0.12,lwd=0.5)

vals = -(6:2)
for (i in 1:length(vals)){
  lines(y=8+rep(which(levs==vals[i]),2)*0.12,x=c(83.75,84.5),col='blue',lwd=0.25)  
  text(x=83.55,y=8+which(levs==vals[i])*0.12,1e4*(10^vals[i]),adj=1,cex=0.7,font=3)
}
text(font=2,x=84.5,y=14,'New cases\nper 10,000',adj=1,cex=0.7)

cols = c('darkslategray4','darkorchid4')
par(mar=c(2.5,3,2,1))
par(mgp=c(3,0.25,0)); par(tck=-0.025)

plot(log10(newCas[,1]+1),type='n',ylim=c(0,4.1),axes=F,ann=F)
for (j in 1:2){
  points(log10(newCas[,j]+1),col=cols[j],lwd=0.25,pch=21,cex=0.35)
  lines(log10(casAvg[,j]+1),col=cols[j],lwd=0.75)
}
text(y=c(3.6,4),x=1,cex=0.75,col=c('darkslategray4','darkorchid4'),c('Tamil Nadu','Andhra Pradesh'),font=3,adj=0)
box(bty='l',lwd=0.5)
axis(side=2,at=log10(c(0,1,10,100,1e3,1e4)+1),labels=c(0,1,10,100,1e3,'10,000'),las=1,cex.axis=0.675,lwd=0,lwd.ticks=0.5)
mtext(side=2,'Daily cases',cex=0.5,line=2)
mtext(side=3,'D) Daily cases by state',line=0,at=-70,cex=0.5,font=2,adj=0)
axis(side=1,at=seq(1,153,21),labels=NA,lwd=0,lwd.ticks=0.5)
text(x=seq(1,153,21),daylabs[seq(1,153,21)],y=-0.375,cex=0.7,xpd=T,srt=45,adj=1)

plot(log10(newDeaths[,1]+1),type='n',ylim=c(0,2.1),axes=F,ann=F)
for (j in 1:2){
  points(log10(newDeaths[,j]+1),col=cols[j],lwd=0.25,pch=21,cex=0.35)
  lines(log10(deathsAvg[,j]+1),col=cols[j],lwd=0.75)
}
box(bty='l',lwd=0.5)
axis(side=2,at=log10(c(0,1,10,100)+1),labels=c(0,1,10,100),las=1,cex.axis=0.675,lwd=0,lwd.ticks=0.5)
axis(side=2,at=log10(c(2:9,seq(20,90,10))+1),tck=-0.015,labels=NA,lwd=0,lwd.ticks=0.5)
mtext(side=2,'Daily deaths',cex=0.5,line=1.5)
mtext(side=3,'F) Daily deaths by state',line=0,at=-65,cex=0.5,font=2,adj=0)
axis(side=1,at=seq(1,153,21),labels=NA,lwd=0,lwd.ticks=0.5)
text(x=seq(1,153,21),daylabs[seq(1,153,21)],y=-0.1875,cex=0.7,xpd=T,srt=45,adj=1)

pops = c(popTn,popAp)
plot(log10(newDeaths[,1]),type='n',ylim=c(-4,2),axes=F,ann=F)
for (j in 1:2){
  lines(log10(totCas[,j]*1e-4),col=cols[j],lwd=0.75)
  lines(log10(totDeaths[,j]*1e-4),col=cols[j],lwd=0.75,lty=5) 
  box(bty='l',lwd=0.5)
}
axis(side=2,at=-4:2,labels=c('0.0001','0.001',0.01,0.1,1,10,100),lwd=0,lwd.ticks=0.5,las=1,cex.axis=0.675)
mtext(side=2,expression(paste('Events per ',10^4,sep='')),cex=0.5,line=2)
mtext(side=3,'G) Cumulative incidence and mortality by state',line=0,at=-50,cex=0.5,font=2,adj=0)
axis(side=1,at=seq(1,153,14),labels=NA,lwd=0,lwd.ticks=0.5)
text(x=seq(1,153,14),daylabs[seq(1,153,14)],y=-4.5,cex=0.7,xpd=T,srt=45,adj=1)
lines(x=c(1,30),y=rep(1.75,2),lwd=0.75,col='gray35')
lines(x=c(1,30),y=rep(1.25,2),lwd=0.75,col='gray35',lty=5)
text(x=35,y=c(1.75,1.25),col='gray35',font=3,c('Cases','Deaths'),adj=0,cex=0.7)

par(mar=c(0,1.5,2,1))
par(tck=-0.035)
plot(log10(testTot[1,]),type='n',ylim=c(-0.1,4.31),axes=F,ann=F)
for (j in 1:2){
  points(log10(testTot[j,]),col=cols[2-(j-1)],lwd=0.25,pch=21,cex=0.35)  
  lines(log10(testAvg[j,]),col=cols[2-(j-1)],lwd=0.75)
}
box(bty='l',lwd=0.5)
axis(side=2,at=log10(c(1,10,100,1e3,1e4)),
     labels=c(0.01,0.1,1,'10','100'),cex.axis=0.675,las=1,lwd=0,lwd.ticks=0.5)
axis(side=1,at=seq(1,length(days),21),labels=NA,lwd=0,lwd.ticks=0.5)
mtext(side=2,expression(paste('Tests (',10^3,')',sep='')),cex=0.5,line=1.25)
mtext(side=3,'E) Daily testing ramp-up',line=0,at=-50,cex=0.5,font=2,adj=0)

par(mar=c(2.5,1.5,0.5,1))
plot(posTot[1,],type='n',ylim=c(0,0.4),axes=F,ann=F)
for (j in 1:2){
  points(posTot[j,],col=cols[2-(j-1)],lwd=0.25,pch=21,cex=0.35)
  lines(posAvg[j,],col=cols[2-(j-1)],lwd=0.75)
}
mtext(side=2,'Pos. (%)',cex=0.5,line=1.25,xpd=T)
box(bty='l',lwd=0.5)
axis(side=2,at=seq(0,0.4,0.1),labels=seq(0,40,10),las=1,cex.axis=0.675,lwd=0,lwd.ticks=0.5)
axis(side=1,at=seq(1,length(days),21),labels=NA,lwd=0,lwd.ticks=0.5)
text(x=seq(1,105,21),daylabs[seq(1,105,21)],y=-0.05,cex=0.7,xpd=T,srt=45,adj=1)
#mtext(side=2,'Positive tests (%)',cex=0.6,line=1.75)
#mtext(side=3,'D) Positive results',line=0,at=-38,cex=0.6,font=2,adj=0)



dev.off()

