library(pomp)
stopifnot(packageVersion("pomp")>"1.4.5")
library(plyr)
library(reshape2)
options(stringsAsFactors=FALSE)
library(ggplot2)
theme_set(theme_bw())
set.seed(1173489184)
f <- seq(0,1,length=100)
R0 <- -log(1-f)/f
plot(f~R0,type='l',xlab=expression(R[0]),ylab="fraction infected",bty='l')
url <- "http://kingaa.github.io/short-course/stochsim/bsflu_data.txt"
bsflu <- read.table(url,header=TRUE)
plot(B~day,data=bsflu,type='b',bty='l',
     main='boarding school influenza outbreak',
     xlab='day',ylab='Influenza cases')
plot(B~day,data=bsflu,type='b',log='y',bty='l',
     xlab='day',ylab='Influenza cases')
fit <- lm(log(B)~day,data=subset(bsflu,day<=4))
summary(fit)
coef(fit)
slope <- coef(fit)[2]; slope
coef(summary(fit))
slope.se <- coef(summary(fit))[2,2]
2.5*slope.se
fitfn <- function (interval) {
  fit <- lm(log(B)~day,data=subset(bsflu,day<=interval))
  slope <- coef(summary(fit))[2,1]
  slope.se <- coef(summary(fit))[2,2]
  c(interval=interval,R0.hat=slope*2.5+1,R0.se=slope.se*2.5)
}
ldply(2:10,fitfn) -> ests
ggplot(ests,mapping=aes(x=interval,y=R0.hat,
                        ymin=R0.hat-2*R0.se,
                        ymax=R0.hat+2*R0.se))+
  geom_point()+geom_errorbar(width=0.2)+
  labs(x="length of initial phase",y=expression("estimated"~R[0]))
## niamey <- read.csv("http://kingaa.github.io/short-course/parest/niamey.csv")
## ggplot(niamey,mapping=aes(x=biweek,y=measles,color=community))+
##   geom_line()+geom_point()
library(pomp)

closed.sir.ode <- Csnippet("
  DS = -Beta*S*I/N;
  DI = Beta*S*I/N-gamma*I;
  DR = gamma*I;
")

init <- Csnippet("
  S = N-1;
  I = 1;
  R = 0;
  ")

pomp(data=bsflu,times="day",t0=0,
     skeleton=closed.sir.ode,
     skeleton.type="vectorfield",
     initializer=init,
     statenames=c("S","I","R"),
     paramnames=c("Beta","gamma","N")) -> closed.sir

params <- c(Beta=1,gamma=1/13,N=763)
x <- trajectory(closed.sir,params=params,as.data.frame=TRUE)

ggplot(data=x,mapping=aes(x=time,y=I))+geom_line()

expand.grid(Beta=c(0.05,1,2),gamma=1/c(5,10,30),N=763) -> params1

x <- trajectory(closed.sir,params=t(params1),as=TRUE,times=seq(0,50))

ggplot(data=x,mapping=aes(x=time,y=I,group=traj))+
    geom_line()

ggplot(data=x,mapping=aes(x=time,y=I,group=traj))+
    geom_line()+scale_y_log10()

library(plyr)
mutate(params1,traj=seq_along(Beta)) -> params1
join(x,params1,by="traj") -> x

ggplot(data=x,mapping=aes(x=time,y=I,group=traj,
                          color=factor(Beta),linetype=factor(1/gamma)))+
  geom_line()+scale_y_log10()+
  labs(linetype=expression("IP"==1/gamma),color=expression(beta))

open.sir.ode <- Csnippet("
  DS = -Beta*S*I/N+mu*(N-S);
  DI = Beta*S*I/N-gamma*I-mu*I;
  DR = gamma*I-mu*R;
")

init <- Csnippet("
  S = S_0;
  I = I_0;
  R = N-S_0-I_0;
")

pomp(data=data.frame(time=seq(0,20,by=1/52),cases=NA),
     times="time",t0=-1/52,
     skeleton=open.sir.ode,
     skeleton.type="vectorfield",
     initializer=init,
     statenames=c("S","I","R"),
     paramnames=c("Beta","gamma","mu","S_0","I_0","N")
) -> open.sir

params <- c(mu=1/50,Beta=400,gamma=13,
            N=100000,S_0=100000/12,I_0=100)

x <- trajectory(open.sir,params=params,as=TRUE)

ggplot(data=x,mapping=aes(x=time,y=I))+geom_line()
ggplot(data=x,mapping=aes(x=S,y=I))+geom_path()

seasonal.sir.ode <- Csnippet("
  double Beta = beta0*(1+beta1*cos(2*M_PI*t));
  DS = -Beta*S*I/N+mu*(N-S);
  DI = Beta*S*I/N-gamma*I-mu*I;
  DR = gamma*I-mu*R;
")

pomp(open.sir,
     skeleton=seasonal.sir.ode,
     skeleton.type="vectorfield",
     initializer=init,
     statenames=c("S","I","R"),
     paramnames=c("beta0","beta1","gamma","mu","N","S_0","I_0")
     ) -> seas.sir

params <- c(mu=1/50,beta0=400,beta1=0.15,gamma=26,
            N=1e5,S_0=7000,I_0=50)

trajectory(seas.sir,params=params,as=TRUE) -> x
ggplot(x,mapping=aes(x=time,y=I))+geom_path()
ggplot(x,mapping=aes(x=S,y=I))+geom_path()

niamey <- read.csv("http://kingaa.github.io/short-course/parest/niamey.csv")
ggplot(niamey,mapping=aes(x=biweek,y=measles,color=community))+
  geom_line()+geom_point()
pomp(data=subset(niamey,community=="A",select=-community),
     times="biweek",t=0,
     skeleton=closed.sir.ode,
     skeleton.type="vectorfield",
     initializer=init,
     statenames=c("S","I","R"),
     paramnames=c("Beta","gamma","N","S_0","I_0")) -> niameyA
                
sse <- function (params) {
  x <- trajectory(niameyA,params=params)
  discrep <- x["I",,]-obs(niameyA)
  sum(discrep^2)
}
params <- c(Beta=NA,gamma=1,
            N=50000,S_0=10000,I_0=10)
f <- function (Beta) {
  params["Beta"] <- Beta
  sse(params)
}
beta <- seq(from=30,to=40,by=0.5)
SSE <- sapply(beta,f)
beta.hat <- beta[which.min(SSE)]
plot(beta,SSE,type='l')
abline(v=beta.hat,lty=2)

params["Beta"] <- beta.hat
coef(niameyA) <- params
x <- trajectory(niameyA,as.data.frame=TRUE)
dat <- join(as.data.frame(niameyA),x,by='time')
ggplot(dat,aes(x=time))+
  geom_line(aes(y=measles),color='black')+
  geom_line(aes(y=I),color='red')
beta <- seq(from=0,to=40,by=0.5)
SSE <- sapply(beta,f)
plot(beta,SSE,type='l')
beta.hat <- beta[which.min(SSE)]
abline(v=beta.hat,lty=2)
coef(niameyA,"Beta") <- beta.hat
x <- trajectory(niameyA,as.data.frame=TRUE)
dat <- join(as.data.frame(niameyA),x,by='time')
ggplot(dat,aes(x=time))+
  geom_line(aes(y=measles),color='black')+
  geom_line(aes(y=I),color='red')
grid <- expand.grid(Beta=seq(from=0,to=20,length=50),
                    S_0=seq(from=4000,to=20000,length=50),
                    N=50000,gamma=1,I_0=10)
x <- trajectory(niameyA,params=t(grid),as.data.frame=TRUE)
library(plyr)
join(x,as.data.frame(niameyA),by="time") -> x
ddply(x,~traj,summarize,sse=sum((measles-I)^2)) -> x
cbind(grid,x) -> grid

ggplot(data=grid,mapping=aes(x=Beta,y=S_0,z=sqrt(sse),fill=sqrt(sse)))+
         geom_tile()+geom_contour(bins=30)+
    labs(fill=expression(sqrt(SSE)),x=expression(beta),y=expression(S(0)))
## ?optim
coef(niameyA) <- params
f <- function (par) {
  params[c("S_0","I_0","Beta")] <- par
  sse(params)
}
optim(fn=f,par=c(10000,10,8)) -> fit
fit
p <- 0.3
n <- 50
k <- seq(0,50,by=1)
prob <- dbinom(x=k,size=n,prob=p) 
plot(k,prob,type='h',lwd=5,lend=1,
     ylab="probability")
k1 <- 18
n1 <- 50
p <- seq(0,1,by=0.001)
plot(p,dbinom(x=k1,size=n1,prob=p,log=TRUE),
     ylim=c(-10,-2),ylab="log-likelihood",
     type='l')
abline(h=dbinom(x=k1,size=n1,prob=k1/n1,log=TRUE)-
         0.5*qchisq(p=0.95,df=1),col='red')
abline(v=k1/n1,col='blue')
k2 <- 243
n2 <- 782
p <- seq(0,1,by=0.001)
plot(p,dbinom(x=k2,size=n2,prob=p,log=TRUE),
     ylim=c(-10,-2),ylab="log-likelihood",
     type='l')
abline(h=dbinom(x=k2,size=n2,prob=k2/n2,log=TRUE)-
         0.5*qchisq(p=0.95,df=1),col='red')
abline(v=k2/n2,col='blue')
n <- c(13,484,3200)
k <- c(4,217,1118)
dbinom(x=k,size=n,prob=0.2,log=TRUE)
sum(dbinom(x=k,size=n,prob=0.2,log=TRUE))
ll.fn <- function (p) {
  sum(dbinom(x=k,size=n,prob=p,log=TRUE))
}
p <- seq(0,1,by=0.001)
loglik <- sapply(p,ll.fn)
plot(p,loglik,type='l',ylim=max(loglik)+c(-10,0))
pomp(data=niameyA,
     skeleton=Csnippet("
  double incidence;
  incidence = b*S*I;
  DS = -incidence;
  DI = incidence-gamma*I;"),
     initializer=Csnippet("
  S = S_0;
  I = I_0;"),
     paramnames=c("b","gamma","S_0","I_0"),
     statenames=c("S","I")) -> niameyA
     
dat <- as.data.frame(niameyA)
loglik <- function (params) {
    x <- trajectory(niameyA,params=params)
    prediction <- x["I",,]
    sum(dnorm(x=dat$measles,mean=prediction,
              sd=params["sigma"],log=TRUE))
}
dat <- subset(niamey,community=="A")
params <- c(S_0=10000,I_0=10,gamma=1,b=NA,sigma=1)

f <- function (b) {
  par <- params
  par["b"] <- b
  loglik(par)
}

b <- seq(from=0,to=0.001,by=0.00002)
ll <- sapply(b,f)
plot(b,ll,type='l',ylab=expression(log(L)))
b.hat <- b[which.max(ll)]
abline(v=b.hat,lty=2)
poisson.loglik <- function (params) {
    x <- trajectory(niameyA,params=params)
    prediction <- x["I",,]
    sum(dpois(x=dat$measles,lambda=params["p"]*prediction,log=TRUE))
}

params <- c(S_0=20000,I_0=1,gamma=1,b=NA,p=0.2)

f <- function (log.b) {
  params[c("b")] <- exp(log.b) # un-transform 'b'
  -poisson.loglik(params)
}
library(bbmle)
guess <- list(log.b=log(0.0003))

fit <- mle2(f,start=guess)
fit
prof.b <- profile(fit)
plot(prof.b)
logit <- function (p) log(p/(1-p))    # the logit transform
expit <- function (x) 1/(1+exp(-x))   # inverse logit

f <- function (log.b, logit.p) {
  par <- params
  par[c("b","p")] <- c(exp(log.b),expit(logit.p))
  -poisson.loglik(par)
}

guess <- list(log.b=log(0.0001),logit.p=logit(0.2))
fit <- mle2(f,start=guess); fit
mle <- with(as.list(coef(fit)),c(b=exp(log.b),p=expit(logit.p))); mle
prof2 <- profile(fit)
plot(prof2)
f <- function (pars) {
  par <- params
  par[c("b","p")] <- as.numeric(pars)
  poisson.loglik(par)
}

b <- seq(from=0.00001,to=0.0002,length=50)
p <- seq(0,1,length=50)
grid <- expand.grid(b=b,p=p)
grid$loglik <- apply(grid,1,f)
grid <- subset(grid,is.finite(loglik))
ggplot(grid,aes(x=b,y=p,z=loglik,fill=loglik))+
  geom_tile()+geom_contour(binwidth=2000)
b <- seq(from=0.0000975,to=0.0000993,length=50)
p <- seq(0.36,0.41,length=50)
grid <- expand.grid(b=b,p=p)
grid$loglik <- apply(grid,1,f)
grid <- subset(grid,is.finite(loglik))
ggplot(grid,aes(x=b,y=p,z=loglik,fill=loglik))+
  geom_tile()+geom_contour(binwidth=1)
params[c("b","p")] <- mle
coef(niameyA) <- params
model.pred <- trajectory(niameyA)["I",,]

raply(2000,rpois(n=length(model.pred),lambda=params["p"]*model.pred)) -> simdat
aaply(simdat,2,quantile,probs=c(0.025,0.5,0.975)) -> quantiles
ggplot(data=cbind(dat,quantiles),mapping=aes(x=biweek))+
  geom_line(aes(y=measles),color='black')+
  geom_line(aes(y=`50%`),color='red')+
  geom_ribbon(aes(ymin=`2.5%`,ymax=`97.5%`),fill='red',alpha=0.2)
negbin.loglik <- function (params) {
  x <- trajectory(niameyA,params=params)
  prediction <- x["I",,]
  sum(dnbinom(x=dat$measles,
              mu=params["p"]*prediction,size=1/params["theta"],
              log=TRUE))
}

f <- function (log.b, logit.p, log.theta) {
  par <- params
  par[c("b","p","theta")] <- unname(c(exp(log.b),
                                      expit(logit.p),
                                      exp(log.theta)))
  -negbin.loglik(par)
}

guess <- list(log.b=log(0.0001),logit.p=logit(0.4),log.theta=0)
fit <- mle2(f,start=guess); fit

mle <- with(as.list(coef(fit)),
            c(b=exp(log.b),p=expit(logit.p),theta=exp(log.theta)))

params[c("b","p","theta")] <- mle
coef(niameyA) <- params
model.pred <- trajectory(niameyA)["I",,]

raply(2000,rnbinom(n=length(model.pred),
                   mu=params["p"]*model.pred,
                   size=1/params["theta"])) -> simdat
aaply(simdat,2,quantile,probs=c(0.025,0.5,0.975)) -> quantiles
ggplot(data=cbind(dat,quantiles),mapping=aes(x=biweek))+
  geom_line(aes(y=measles),color='black')+
  geom_line(aes(y=`50%`),color='red')+
  geom_ribbon(aes(ymin=`2.5%`,ymax=`97.5%`),fill='red',alpha=0.2)

