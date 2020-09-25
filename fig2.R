my.filled.contour = function (x = seq(0, 1, length.out = nrow(z)), y = seq(0, 1, 
                                                                           length.out = ncol(z)), z, xlim = range(x, finite = TRUE), 
                              ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
                              levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors, 
                              col = color.palette(length(levels) - 1), plot.title, plot.axes, 
                              key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
                              axes = TRUE, frame.plot = axes, ...) 
{
  if (missing(z)) {
    if (!missing(x)) {
      if (is.list(x)) {
        z <- x$z
        y <- x$y
        x <- x$x
      }
      else {
        z <- x
        x <- seq.int(0, 1, length.out = nrow(z))
      }
    }
    else stop("no 'z' matrix specified")
  }
  else if (is.list(x)) {
    y <- x$y
    x <- x$x
  }
  if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
    stop("increasing 'x' and 'y' values expected")
  mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
  on.exit(par(par.orig))
  w <- (3 + mar.orig[2L]) * par("csi") * 2.54
  #layout(matrix(c(2, 1), ncol = 2L), widths = c(1, lcm(w)))
  #par(las = las)
  #mar <- mar.orig
  #mar[4L] <- mar[2L]
  #mar[2L] <- 1
  #par(mar = mar)
  #plot.new()
  #plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i", 
  #            yaxs = "i")
  #rect(0, levels[-length(levels)], 1, levels[-1L], col = col)
  #if (missing(key.axes)) {
  #  if (axes) 
  #    axis(4)
  #}
  #else key.axes
  #box()
  #if (!missing(key.title)) 
  #  key.title
  #mar <- mar.orig
  #mar[4L] <- 1
  #par(mar = mar)
  #plot.new()
  plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
  .filled.contour(x, y, z, levels, col)
  if (missing(plot.axes)) {
    if (axes) {
      title(main = "", xlab = "", ylab = "")
      Axis(x, side = 1)
      Axis(y, side = 2)
    }
  }
  else plot.axes
  if (frame.plot) 
    box()
  if (missing(plot.title)) 
    title(...)
  else plot.title
  invisible()
}

filled.contour3 <-
  function (x = seq(0, 1, length.out = nrow(z)),
            y = seq(0, 1, length.out = ncol(z)), z, xlim = range(x, finite = TRUE), 
            ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
            levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors, 
            col = color.palette(length(levels) - 1), plot.title, plot.axes, 
            key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
            axes = TRUE, frame.plot = axes,mar, ...) 
  {
    # modification by Ian Taylor of the filled.contour function
    # to remove the key and facilitate overplotting with contour()
    # further modified by Carey McGilliard and Bridget Ferris
    # to allow multiple plots on one page
    
    if (missing(z)) {
      if (!missing(x)) {
        if (is.list(x)) {
          z <- x$z
          y <- x$y
          x <- x$x
        }
        else {
          z <- x
          x <- seq.int(0, 1, length.out = nrow(z))
        }
      }
      else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
      y <- x$y
      x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
      stop("increasing 'x' and 'y' values expected")
    # mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
    # on.exit(par(par.orig))
    # w <- (3 + mar.orig[2]) * par("csi") * 2.54
    # par(las = las)
    # mar <- mar.orig
    plot.new()
    # par(mar=mar)
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
    if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1) 
      stop("no proper 'z' matrix specified")
    if (!is.double(z)) 
      storage.mode(z) <- "double"
    .filled.contour(as.double(x), as.double(y), z, as.double(levels), 
                    col = col)
    if (missing(plot.axes)) {
      if (axes) {
        title(main = "", xlab = "", ylab = "")
        Axis(x, side = 1)
        Axis(y, side = 2)
      }
    }
    else plot.axes
    if (frame.plot) 
      box()
    if (missing(plot.title)) 
      title(...)
    else plot.title
    invisible()
  }

library('RColorBrewer')

agelb = c(seq(0,80,5))#,seq(30,80,10))
ageub = c(seq(5,80,5),200)#seq(30,80,10),200)#,seq(40,80,10),200)

setwd('~/Dropbox/covid india/revision/figures')

pdf(file='fig2.pdf',height=5,width=6)

layout(matrix(c(19,19,19,19,20,
                1, 1, 2, 3, 4,
                1, 1, 2, 2, 4,
                21,21,22,22,22,
                5, 9, 13,13,15,
                6, 10,13,13,15,
                7, 11,14,14,16,
                8, 12,14,14,16,
                23,23,17,17,18),nrow=9,byrow=T),
       heights=c(0.075,0.7,0.3,0.075,0.5,0.5,0.5,0.7,0.15),
       widths=c(1.1,1.1,1,1,1.55))

par(mar=rep(0,4))
par(mgp=c(3,0.3,0))


par(mgp=c(3,0.35,0))

par(tck=-0.03)
par(mar=c(2.5,2.75,1,0.5))
plot(1,type='n',axes=F,ann=F,ylim=log(c(1,25e3)),xlim=c(2,83))
for (i in 1:length(tot)){
    polygon(x=i+c(-0.5,-0.5,0.5,0.5),y=log(c(1,tot[i]+1,tot[i]+1,1)),lty=0,col='peachpuff') 
    lines(x=i+c(-0.5,-0.5,0.5,0.5),y=log(c(1,tot[i]+1,tot[i]+1,1)),lwd=0.5)
}
axis(side=2,at=c(-100,log(c(0,1,3,10,30,100,300,1e3,3e3,1e4,3e4)+1)),
     labels=c(NA,0,1,3,10,30,100,300,'1,000','3,000','10,000','30,000'),
     cex.axis=0.675,lwd=0.5,lwd.ticks=0.5,las=1)
axis(side=1,at=1+c(-100,seq(0,80,5),83),labels=NA,cex.axis=0.65,lwd=0.5,lwd.ticks=0.5,las=1)
text(x=1+c(seq(0,80,5)),y=-1,c(seq(0,80,5)),cex=0.675,srt=45,adj=1,xpd=T)
text(x=85,y=-1,srt=45,'++',cex=0.675,adj=1,xpd=T)
mtext(side=2,'Frequency',cex=0.5,line=2)
mtext(side=1,'Number of contacts',cex=0.5,line=1)
mtext(side=3,'A1. Total contacts tested',cex=0.5,adj=0,font=3,at=-18,line=0)


plot(1,type='n',axes=F,ann=F,ylim=log(c(1,1e5)),xlim=c(0,40))
for (i in 2:length(totPos)){
  polygon(x=i+c(-0.5,-0.5,0.5,0.5)-1,y=log(c(1,totPos[i]+1,totPos[i]+1,1)),lty=0,col='peachpuff') 
  lines(x=i+c(-0.5,-0.5,0.5,0.5)-1,y=log(c(1,totPos[i]+1,totPos[i]+1,1)),lwd=0.5)
}
polygon(x=0+c(-0.5,-0.5,0.5,0.5),y=log(c(1,totPos[1]+1,totPos[1]+1,1)),lty=0,col='mistyrose3') 
lines(x=0+c(-0.5,-0.5,0.5,0.5),y=log(c(1,totPos[1]+1,totPos[1]+1,1)),lwd=0.5)
axis(side=2,at=c(-100,log(c(0,1,10,100,1e3,1e4,1e5)+1)),
     labels=c(NA,0,1,10,100,'1,000','10,000','100,000'),
     cex.axis=0.675,lwd=0.5,lwd.ticks=0.5,las=1)
axis(side=1,at=c(-100,seq(0,40,5)),labels=NA,cex.axis=0.65,lwd=0.5,lwd.ticks=0.5,las=1)
text(x=c(seq(0,40,5)),y=-1.15,c(seq(0,40,5)),cex=0.675,srt=45,adj=1,xpd=T)
mtext(side=3,'A2. Total contacts positive',cex=0.5,adj=0,font=3,at=-10)
mtext(side=2,'Frequency',cex=0.5,line=2)
mtext(side=1,'Number of contacts',cex=0.5,line=1)

par(mar=c(2.5,2,1,0.5))
par(tck=-0.05)
plot(1,xlim=c(0.79,1),ylim=c(0,1),type='n',axes=F,ann=F,lwd=0.75)
polygon(x=c(-100,100,100,-100),y=c(-100,-100,100,100),col=rgb(1,0,0,0.125),lty=0)
lines(ys~xs,col='blue')
axis(side=1,at=c(-100,0.79,0.83),lwd=0.5,lwd.ticks=0.5,labels=NA)
axis(side=1,at=c(0.84,seq(0.85,1,0.01)),lwd=0.5,lwd.ticks=0.5,labels=NA)
axis(side=2,at=c(-100,seq(0,1,0.2)),cex.axis=0.6,lwd=0.5,lwd.ticks=0.5,las=1,
     labels=c(NA,seq(0,100,20)))
text(y=-0.12,x=c(0.79,seq(0.85,1,0.05)),c(0,seq(85,100,5)),srt=45,adj=1,xpd=T,cex=0.6)
mtext(side=2,'Prop. (%)',line=1.3,cex=0.5)
#mtext(side=3,'A3. Superspreading',font=3,adj=0,cex=0.5,at=0.42)
mtext(side=1,'Index case percentile',cex=0.5,line=0.75)


#### new sex thing
par(tck=-0.03)
par(mar=c(2.75,3.25,0,0.5))
plot(1,type='n',axes=F,ann=F,xlim=c(-3.25,-2.1),ylim=c(1.5,11.5))
qlines = function(obj,y,col){
  lines(x=quantile(obj,c(0.025,0.975)),y=rep(y,2),lwd=0.75,col=col)
  points(x=mean(obj),y=y,lwd=0.75,pch=21,cex=0.7,bg='white',col=col)
  lines(x=quantile(obj,rep(0.025,2)),y=y+c(-0.25,0.25),lwd=0.75,col=col)
  lines(x=quantile(obj,rep(0.975,2)),y=y+c(-0.25,0.25),lwd=0.75,col=col)
}
text(x=-3.25,y=11,'High-risk contacts',cex=0.7,font=1,adj=0)
qlines(mmHR,y=10,col='dark red')
qlines(mfHR,y=9,col='dark red')
qlines(fmHR,y=8,col='dark red')
qlines(ffHR,y=7,col='dark red')
text(x=-3.25,y=6,'Low-risk contacts',cex=0.7,font=1,adj=0)
qlines(mmLR,y=5,col='blue')
qlines(mfLR,y=4,col='blue')
qlines(fmLR,y=3,col='blue')
qlines(ffLR,y=2,col='blue')

axis(side=2,at=10:7,lwd=0.5,lwd.ticks=0.5,labels=c('M to M','M to F','F to M','F to F'),cex.axis=0.675,las=1)
axis(side=2,at=5:2,lwd=0.5,lwd.ticks=0.5,labels=c('M to M','M to F','F to M','F to F'),cex.axis=0.675,las=1)

mtext(side=2,'Direction of transmission',cex=0.5,line=2.25)
mtext(side=1,'Proportion positive (%)',cex=0.5,line=1)

axis(side=1,at=log(seq(0.04,0.12,0.01)),labels=NA,lwd=0.5,lwd.ticks=0.5)
text(x=log(c(0.04,0.05,0.06,0.07,0.08,0.1,0.12)),y=0.3,adj=1,cex=0.675,srt=45,xpd=T,
     c(4,5,6,7,8,10,12))
#### age loop
par(tck=-0.05)
for (i in 1:8){
  if ((i%in%c(4,8))==F){
    par(mar=c(0.5,2,1,0.5))
  } else{
    par(mar=c(3,2,1,0.5))
  }
  plot(1,type='n',xlim=c(0.75,8.25),ylim=c(0,0.75),axes=F,ann=F)
  #polygon(x=c(-100,100,100,-100),y=c(-100,-100,100,100),col='grey85',lty=0)
  lines(y=probVals1[i,,1],x=1:8+0.2,lwd=0.5,col='darkred')
  lines(y=probVals2[i,,1],x=1:8,lwd=0.5,col='blue')
  for (j in 1:8){
    lines(y=(probVals1[i,j,2:3]),x=rep(j,2)+0.25,lwd=0.75,col='darkred')
    points(y=(probVals1[i,j,1]),x=j+0.25,pch=21,bg='white',lwd=0.75,cex=0.75,col='darkred')
    
    lines(y=(probVals2[i,j,2:3]),x=rep(j,2),lwd=0.75,col='blue')
    points(y=(probVals2[i,j,1]),x=j,pch=21,bg='white',lwd=0.75,cex=0.75,col='blue')
  }
  box(bty='l',lwd=0.5)
  mtext(side=2,'Pos. (%)',cex=0.5,line=1.25)
  if (i!=8){
    mtext(side=3,paste('C',i,'. Contacts ages ',agelbA[i],'-',(ageubA[i]-1),'y',sep=''),cex=0.5,font=3,adj=0,at=-1.9)    
  } else{
    mtext(side=3,paste('C',i,'. Contacts ages ',agelbA[i],'y+',sep=''),cex=0.5,font=3,adj=0,at=-1.9)    
  }
  axis(side=2,at=seq(0,0.75,0.25),labels=seq(0,75,25),las=1,cex.axis=0.675,lwd=0,lwd.ticks=0.5)
  axis(side=1,at=1:8+0.125,labels=NA,las=1,cex.axis=0.675,lwd=0,lwd.ticks=0.5)
  if (i%in%c(4,8)){
    text(x=1:8+0.125,y=-0.12,
         c('0-4','5-9','10-14','15-19','20-39','40-64','65-79','80+'),
         srt=45,xpd=T,cex=0.675,adj=1)
    mtext(side=1,'Index case age (y)',line=2,cex=0.5)
  }
  
  if (i==5){
    lines(x=c(1.25,2.75),y=rep(0.65,2),col='dark red',lwd=0.5)
    lines(x=c(1.25,2.75),y=rep(0.5,2),col='blue',lwd=0.5)
    points(x=rep(2,2),y=c(0.65,0.5),col=c('dark red','blue'),pch=21,bg='white',lwd=0.75,cex=0.75)
    text(x=3,y=c(0.65,0.5),col=c('dark red','blue'),c('High risk contact','Low risk contact'),font=3,cex=0.675,adj=0)
  }
}

par(tck=-0.02)
yb<-colorRampPalette(c('white','pink','red'),alpha=T,bias=1)
par(mar=c(0.25,5,2,1))
filled.contour3(log(prob0),axes=F,ann=F,levels=seq(-4,-0.7,0.1),color.palette=yb)
box(bty='o',lwd=0.5)
axis(side=1,at=seq(0,1,1/17),lwd=0,lwd.ticks=0.5,labels=NA,las=1,cex.axis=0.675)
axis(side=2,at=seq(0,1,1/17),lwd=0,lwd.ticks=0.5,labels=NA,las=1,cex.axis=0.675)
axis(side=2,at=seq(0.5/17,1,1/17),
     labels=c('0-4',NA,'10-14',NA,
              '20-24',NA,'30-34',NA,
              '40-44',NA,'50-54',NA,
              '60-64',NA,'70-74',NA,'80+'),
     cex.axis=0.675,lwd=0,lwd.ticks=0,las=1)
mtext(side=2,'Index case age (y)',cex=0.5,line=2.5)
mtext(side=3,'C9. Proportion positive by age,\n       given exposure (all contacts)',cex=0.5,font=3,adj=0,at=-0.13,line=0)

par(mar=c(2.5,5,2,1))
filled.contour3(log(prob1),axes=F,ann=F,levels=seq(-4,-0.7,0.1),color.palette=yb)
box(bty='o',lwd=0.5)
axis(side=1,at=seq(0,1,1/17),lwd=0,lwd.ticks=0.5,labels=NA,las=1,cex.axis=0.675)
axis(side=2,at=seq(0,1,1/17),lwd=0,lwd.ticks=0.5,labels=NA,las=1,cex.axis=0.675)
axis(side=2,at=seq(0.5/17,1,1/17),
     labels=c('0-4',NA,'10-14',NA,
              '20-24',NA,'30-34',NA,
              '40-44',NA,'50-54',NA,
              '60-64',NA,'70-74',NA,'80+'),
     cex.axis=0.675,lwd=0,lwd.ticks=0,las=1)
text(x=seq(0.5/17,1,1/17),c('0-4',NA,'10-14',NA,
                            '20-24',NA,'30-34',NA,
                            '40-44',NA,'50-54',NA,
                            '60-64',NA,'70-74',NA,'80+'),y=-0.04,cex=0.675,srt=45,xpd=T,adj=1)
mtext(side=1,'Contact age (y)',cex=0.5,line=1.5)
mtext(side=2,'Index case age (y)',cex=0.5,line=2.5)
mtext(side=3,'C10. Proportion positive by age,\n        given exposure (high-risk contacts)',cex=0.5,font=3,adj=0,at=-0.15,line=0)



yb<-colorRampPalette(c('white','pink','red'),alpha=T,bias=0.25)
par(mar=c(0.25,1,2,1.5))
filled.contour3(log((prob1X+prob2X)/apply(prob1X+prob2X,1,sum)),axes=F,ann=F,
                levels=seq(-6.2,-1.6,0.05),color.palette=yb)
box(bty='o',lwd=0.5)
axis(side=1,at=seq(0,1,1/17),lwd=0,lwd.ticks=0.5,labels=NA,las=1,cex.axis=0.7)
axis(side=2,at=seq(0,1,1/17),lwd=0,lwd.ticks=0.5,labels=NA,las=1,cex.axis=0.7)
axis(side=2,at=seq(0.5/17,1,1/17),
     labels=NA,
     lwd=0,lwd.ticks=0,las=1)
mtext(side=3,'C11. Age distribution of index cases,\n        among all infected contacts',cex=0.5,font=3,adj=0,at=-0.15,line=0)


par(mar=c(2.5,1,2,1.5))
filled.contour3(log((prob1X)/apply(prob1X,1,sum)),axes=F,ann=F,
                levels=seq(-6.2,-1.6,0.05),color.palette=yb)
box(bty='o',lwd=0.5)
axis(side=1,at=seq(0,1,1/17),lwd=0,lwd.ticks=0.5,labels=NA,las=1,cex.axis=0.7)
axis(side=2,at=seq(0,1,1/17),lwd=0,lwd.ticks=0.5,labels=NA,las=1,cex.axis=0.7)
axis(side=2,at=seq(0.5/17,1,1/17),
     labels=NA,
     lwd=0,lwd.ticks=0,las=1)
mtext(side=3,'C12. Age distribution of index cases,\n        among infected high-risk contacts',cex=0.5,font=3,adj=0,at=-0.15,line=0)

text(x=seq(0.5/17,1,1/17),c('0-4',NA,'10-14',NA,
                            '20-24',NA,'30-34',NA,
                            '40-44',NA,'50-54',NA,
                            '60-64',NA,'70-74',NA,'80+'),y=-0.04,cex=0.675,srt=45,xpd=T,adj=1)
mtext(side=1,'Infected contact age (y)',cex=0.5,line=1.5)



par(tck=-0.3)
par(mar=c(0.25,6,1,3))
mat = matrix(rep(seq(-4,-0.7,0.01),2),byrow=F,ncol=2)

filled.contour3(mat,
                axes=F,ann=F,
                color.palette=yb,levels=seq(-4,-0.7,0.1))
box(bty='o',lwd=0.5)

labvals = c(0.02,0.05,0.1,0.2,0.5)
sel = c(); 
for (i in 1:length(labvals)){
  sel[i] = which.min(abs(mat[,1]-log(labvals[i])))
}
axis(side=3,at=sel/dim(mat)[1],
     labels=NA,lwd=0,lwd.ticks=0.5)
text(x=sel/dim(mat)[1]-0.025,y=2.1,xpd=T,cex=0.675,adj=0,
     c('2%','5%','10%','20%','50%'))




par(tck=-0.3)
par(mar=c(0.25,2.5,1,3))
mat = matrix(rep(seq(-6.2,-1.6,0.01),2),byrow=F,ncol=2)

filled.contour3(mat,
                axes=F,ann=F,
                color.palette=yb,levels=seq(-6.2,-1.6,0.05))
box(bty='o',lwd=0.5)

labvals = c(0.002,0.005,0.02,0.05,0.2)
sel = c(); 
for (i in 1:length(labvals)){
  sel[i] = which.min(abs(mat[,1]-log(labvals[i])))
}
axis(side=3,at=sel/dim(mat)[1],
     labels=NA,lwd=0,lwd.ticks=0.5)
text(x=sel/dim(mat)[1]-0.025,y=2.1,xpd=T,cex=0.675,adj=0,
     c('0.2%','0.5%','2%','5%','20%'))


par(mar=rep(0,4))

plot(xlim=c(0,1),ylim=c(0,1),axes=F,ann=F,1,type='n')
text(x=-0.035,y=0.5,'A. Contact distributions',cex=0.9,font=2,adj=0,xpd=T)

plot(xlim=c(0,1),ylim=c(0,1),axes=F,ann=F,1,type='n')
text(x=-0.04,y=0.5,'B. Case and contact sex',cex=0.9,font=2,adj=0,xpd=T)

plot(xlim=c(0,1),ylim=c(0,1),axes=F,ann=F,1,type='n')
text(x=-0.04,y=0.5,'C. Case and contact age',cex=0.9,font=2,adj=0,xpd=T)



dev.off()
