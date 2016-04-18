## ----prelims,include=FALSE,cache=FALSE-----------------------------------
options(
  keep.source=TRUE,
  stringsAsFactors=FALSE,
  encoding="UTF-8"
  )

set.seed(594709947L)
library(ggplot2)
theme_set(theme_bw())
library(plyr)
library(reshape2)
library(magrittr)
library(pomp)
stopifnot(packageVersion("pomp")>="1.4.5")

## ----region-diagram,echo=F-----------------------------------------------
op <- par(mar=c(0,1,0,1),mgp=c(2,1,0),font=4,family="sans")
plot(c(0,1),c(0,1),type='n',ann=F,bty='o',tcl=0)
t <- seq(0,1,by=0.001)
xyc <- rbind(a=c(0.89,0.64),aa=c(0.63,0.81),b=c(0.67,0.96),c=c(0.21,0.89),d=c(0.35,0.35),
             e=c(0.03,0.4),f=c(0.28,0.04),g=c(0.75,0.03),h=c(0.6,0.6))
basis <- periodic.bspline.basis(t,degree=2,nbasis=nrow(xyc),period=1)
xy <- basis%*%xyc
lines(xy[,1],xy[,2],lwd=1.5)
xyc <- rbind(a=c(0.37,0.33),b=c(0.51,0.33),c=c(0.51,0.23),d=c(0.37,0.23))
basis <- periodic.bspline.basis(t,degree=1,nbasis=nrow(xyc),period=1)
xy <- basis%*%xyc
lines(xy[,1],xy[,2],lwd=1.5)
text(x=c(0.05,0.5,0.9),y=c(0.95,0.75,0.1),labels=c("U","D","A"),cex=1.5)
arrows(0.88,0.1,0.44,0.28,lwd=1.5,length=0.1)
par(op)

## ----rejection-method-diagram,echo=F-------------------------------------
op <- par(mar=c(0,1,0,1),mgp=c(2,1,0),font=4,family="sans")
x <- seq(-5,10,by=0.01)
f <- 0.2*dnorm(x,mean=-2,sd=0.5)+0.5*dnorm(x,mean=1,sd=1)+0.3*dnorm(x,mean=6,sd=2)
g <- dnorm(x,mean=1,sd=5)
Mg <- 1.1*max(f)/max(g)*g
xx <- c(6.1,2.6,-4)
yy <- c(0.16,0.12,0.06)
plot(x,Mg,type='l',col='red',xlab='x',ylab="pdf",ylim=c(0,1.05*max(Mg)),bty='l')
lines(x,g,col='blue')
lines(x,f,col='black')
text(xx,yy,labels=c("M g","f","g"),col=c("red","black","blue"),font=3,cex=1.5)
par(op)

