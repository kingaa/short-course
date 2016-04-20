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
days <- 2:10
slope <- numeric(length=length(days))
slope.se <- numeric(length=length(days))
for (k in seq_along(days)) {
  fit <- lm(log(B)~day,data=subset(bsflu,day<=days[k]))
  slope[k] <- coef(summary(fit))[2,1]
  slope.se[k] <- coef(summary(fit))[2,2]
}
R0.hat <- slope*2.5+1
R0.se <- slope.se*2.5
plot(slope~days,type='o')
plot(range(days),
     range(c(R0.hat-2*R0.se,R0.hat+2*R0.se),na.rm=T),
     type='n',bty='l',
     xlab="length of initial phase (da)",
     ylab=expression("estimated"~R[0]))
lines(R0.hat~days,type='o',lwd=2)
lines(R0.hat+2*R0.se~days,type='l')
lines(R0.hat-2*R0.se~days,type='l')
## niamey <- read.csv("http://kingaa.github.io/parest/niamey.csv",comment.char="#")
## plot(measles~biweek,data=niamey,subset=community=="A",type='b')
## plot(measles~biweek,data=niamey,subset=community=="B",type='b')
## plot(measles~biweek,data=niamey,subset=community=="C",type='b')
require(deSolve)
## ?ode

closed.sir.model <- function (t, x, params) {
  ## first extract the state variables
  S <- x[1]
  I <- x[2]
  R <- x[3]
  ## now extract the parameters
  beta <- params["beta"]
  gamma <- params["gamma"]
  ## now code the model equations
  dSdt <- -beta*S*I
  dIdt <- beta*S*I-gamma*I
  dRdt <- gamma*I
  ## combine results into a single vector
  dxdt <- c(dSdt,dIdt,dRdt) 
  ## return result as a list!
  list(dxdt)                              
}

params <- c(beta=400,gamma=365/13)
times <- seq(from=0,to=60/365,by=1/365/4) # returns a sequence
xstart <- c(S=0.999,I=0.001,R=0.000)     # initial conditions
require(deSolve)

out <- as.data.frame(
                     ode(
                         func=closed.sir.model,
                         y=xstart,
                         times=times,
                         parms=params
                         )
                     )
plot(I~time,data=out,type='l')

betavals <- c(20,50,500)
ips <- c(5,10,30)
gammavals <- 365/ips

## set some plot parameters
op <- par(mgp=c(2,1,0),mar=c(3,3,1,1),mfrow=c(3,3))

for (beta in betavals) {
  for (gamma in gammavals) {
    params <- c(beta=beta,gamma=gamma)
    out <- as.data.frame(
                         ode(
                             func=closed.sir.model,
                             y=xstart,
                             times=times,
                             parms=params
                             )
                         )    
    title <- bquote(list(beta==.(beta),"IP"==.(365/gamma)~"da"))
    plot(I~time,data=out,type='l',main=title)
  }
}
par(op)      # restore old settings


open.sir.model <- function (t, x, params) {
  beta <- params["beta"]
  mu <- params["mu"]
  gamma <- params["gamma"]
  dSdt <- mu*(1-x[1])-beta*x[1]*x[2]
  dIdt <- beta*x[1]*x[2]-(mu+gamma)*x[2]
  dRdt <- gamma*x[2]-mu*x[3]
  list(c(dSdt,dIdt,dRdt))
}


params <- c(mu=1/50,beta=400,gamma=365/13)


times <- seq(from=0,to=25,by=1/365)
out <- as.data.frame(
                     ode(
                         func=open.sir.model,
                         y=xstart,
                         times=times,
                         parms=params
                         )
                     )

op <- par(fig=c(0,1,0,1),mfrow=c(2,2),
          mar=c(3,3,1,1),mgp=c(2,1,0))
plot(S~time,data=out,type='l',log='y')
plot(I~time,data=out,type='l',log='y')
plot(R~time,data=out,type='l',log='y')
plot(I~S,data=out,log='xy',pch='.',cex=0.5)
par(op)                               
seas.sir.model <- function (t, x, params) {
  beta0 <- params["beta0"]
  beta1 <- params["beta1"]
  mu <- params["mu"]
  gamma <- params["gamma"]
  beta <- beta0*(1+beta1*cos(2*pi*t))
  dSdt <- mu*(1-x[1])-beta*x[1]*x[2]
  dIdt <- beta*x[1]*x[2]-(mu+gamma)*x[2]
  dRdt <- gamma*x[2]-mu*x[3]
  list(c(dSdt,dIdt,dRdt))
}

params <- c(mu=1/50,beta0=400,beta1=0.15,gamma=365/13)
xstart <- c(S=0.07,I=0.00039,R=0.92961)
times <- seq(from=0,to=30,by=7/365)
out <- as.data.frame(
                     ode(
                         func=seas.sir.model,
                         y=xstart,
                         times=times,
                         parms=params
                         )
                     )

op <- par(fig=c(0,1,0,1),mfrow=c(2,2),
          mar=c(3,3,1,1),mgp=c(2,1,0))
plot(S~time,data=out,type='l',log='y')
plot(I~time,data=out,type='l',log='y')
plot(R~time,data=out,type='l',log='y')
plot(I~S,data=out,log='xy',pch='.',cex=0.5)
par(op) 
rain <- read.csv(paste0(baseurl,"data/dacca_rainfall.csv"))
rain$time <- with(rain,year+(month-1)/12)

plot(rainfall~time,data=rain,type='l')

interpol <- with(rain,approxfun(time,rainfall,rule=2,method='constant'))

data.frame(time=seq(from=1920,to=1930,by=1/365)) -> smoothed.rain
smoothed.rain$rainfall <- sapply(smoothed.rain$time,interpol)

plot(rainfall~time,data=rain,col='black',cex=2,pch=16,log='')
lines(rainfall~time,data=smoothed.rain,col='red')


rain.sir.model <- function (t, x, params) {
  a <- params["a"]
  b <- params["b"]
  mu <- params["mu"]
  gamma <- params["gamma"]
  R <- interpol(t)
  beta <- a*R/(b+R)
  dSdt <- mu*(1-x[1])-beta*x[1]*x[2]
  dIdt <- beta*x[1]*x[2]-(mu+gamma)*x[2]
  dRdt <- gamma*x[2]-mu*x[3]
  list(c(dSdt,dIdt,dRdt))
}

params <- c(a=500,b=50,mu=1/50,gamma=365/13)
xstart <- c(S=0.07,I=0.00039,R=0.92961)
times <- seq(from=1920,to=1930,by=7/365)
out <- as.data.frame(
                     ode(
                         func=rain.sir.model,
                         y=xstart,
                         times=times,
                         parms=params
                         )
                     )

op <- par(fig=c(0,1,0,1),mfrow=c(2,2),
          mar=c(3,3,1,1),mgp=c(2,1,0))
plot(S~time,data=out,type='l',log='y')
plot(I~time,data=out,type='l',log='y')
plot(R~time,data=out,type='l',log='y')
plot(I~S,data=out,log='xy',pch='.',cex=1)
par(op) 


fit <- loess(log1p(rainfall)~time,data=rain,span=0.05)

data.frame(time=seq(from=1920,to=1930,by=1/365)) -> smoothed.rain
smoothed.rain$rainfall <- expm1(predict(fit,newdata=smoothed.rain))

plot(rainfall~time,data=rain,col='black',cex=2,pch=16,log='')
lines(rainfall~time,data=smoothed.rain,col='red')

url <- paste0(baseurl,"data/niamey.csv")
niamey <- read.csv(url,comment.char="#")
plot(measles~biweek,data=niamey,type='n')
lines(measles~biweek,data=subset(niamey,community=="A"),col=1)
lines(measles~biweek,data=subset(niamey,community=="B"),col=2)
lines(measles~biweek,data=subset(niamey,community=="C"),col=3)
legend("topleft",col=1:3,lty=1,bty='n',
       legend=paste("community",c("A","B","C")))
require(deSolve)

closed.sir.model <- function (t, x, params) {
  X <- x[1]
  Y <- x[2]
  Z <- x[3]

  beta <- params["beta"]
  gamma <- params["gamma"]
  pop <- params["popsize"]

  dXdt <- -beta*X*Y/pop
  dYdt <- beta*X*Y/pop-gamma*Y
  dZdt <- gamma*Y

  list(c(dXdt,dYdt,dZdt))
}
prediction <- function (params, times) {
  xstart <- params[c("X.0","Y.0","Z.0")]
  out <- ode(
             func=closed.sir.model,
             y=xstart,
             times=times,
             parms=params
             )
  out[,3]     # return the number of infectives
}
sse <- function (params, data) {
  times <- c(0,data$biweek/26)          # convert to years
  pred <- prediction(params,times)
  discrep <- pred[-1]-data$measles
  sum(discrep^2)                        # sum of squared errors
}
dat <- subset(niamey,community=="A")
params <- c(X.0=10000,Y.0=10,Z.0=39990,popsize=50000,
            gamma=365/13,beta=NA)
f <- function (beta) {
  params["beta"] <- beta
  sse(params,dat)
}
beta <- seq(from=0,to=1000,by=5)
SSE <- sapply(beta,f)
beta.hat <- beta[which.min(SSE)]
plot(beta,SSE,type='l')
abline(v=beta.hat,lty=2)
plot.window(c(0,1),c(0,1))
text(0.05,0.9,"A")
params["beta"] <- beta.hat
plot(measles~biweek,data=dat)
lines(dat$biweek,prediction(params,dat$biweek/26))
xdat <- subset(niamey,community=="A")
params <- c(X.0=NA,Y.0=10,Z.0=1,popsize=50000,
            gamma=365/13,beta=NA)
f <- function (beta, X.0) {
  params["beta"] <- beta
  params["X.0"] <- X.0 
  sse(params,dat)
  }
grid <- expand.grid(beta=seq(from=100,to=300,length=50),
                    X.0=seq(from=4000,to=20000,length=50))
grid$SSE <- with(grid,mapply(f,beta,X.0))
require(lattice)
contourplot(sqrt(SSE)~beta+X.0,data=grid,cuts=30)
## ?optim
dat <- subset(niamey,community=="A")
params <- c(X.0=NA,Y.0=NA,Z.0=1,popsize=50000,
            gamma=365/13,beta=NA)
f <- function (par) {
  params[c("X.0","Y.0","beta")] <- par
  sse(params,dat)
}
optim(fn=f,par=c(10000,10,220)) -> fit
fit
## dat <- subset(niamey,community=="A")
## f <- function (par) {
##   par <- as.numeric(par)
##   params <- c(X.0=exp(par[1]),Y.0=exp(par[2]),Z.0=1,popsize=50000,
##               gamma=par[4],beta=exp(par[3]))
##   sse(params,dat)
## }
## optim(fn=f,par=log(c(10000,10,220,13/365))) -> fit
## fit
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
require(deSolve)

closed.sir.model <- function (t, x, params) {
  inc <- params["b"]*x[1]*x[2]          # incidence
  list(c(-inc,inc-params["gamma"]*x[2]))
}
prediction <- function (params, times) {
  out <- ode(
             func=closed.sir.model,
             y=params[c("X.0","Y.0")],
             times=c(0,times),
             parms=params
             )
## return the Y variable only
## and discard Y(0)
  out[-1,3]
}
sse <- function (params, data) {
  times <- data$biweek/26               # convert to years
  pred <- prediction(params,times)
  discrep <- pred-data$measles
  sum(discrep^2)                        # sum of squared errors
}
loglik <- function (params, data) {
  times <- data$biweek/26
  pred <- prediction(params,times)
  sum(dnorm(x=data$measles,mean=pred,sd=params["sigma"],log=TRUE))
}
dat <- subset(niamey,community=="A")
params <- c(X.0=10000,Y.0=10,gamma=365/13,b=NA,sigma=1)

f <- function (b) {
  par <- params
  par["b"] <- b
  loglik(par,dat)
}

b <- seq(from=0,to=0.02,by=0.0001)
ll <- sapply(b,f)
plot(b,-ll,type='l',ylab=expression(-log(L)))
b.hat <- b[which.max(ll)]
abline(v=b.hat,lty=2)
poisson.loglik <- function (params, data) {
  times <- data$biweek/26
  pred <- prediction(params,times)
  sum(dpois(x=data$measles,lambda=params["p"]*pred[-1],log=TRUE))
}
dat <- subset(niamey,community=="A")
params <- c(X.0=20000,Y.0=1,gamma=365/13,b=NA,p=0.2)

## objective function (-log(L))
f <- function (log.b) {
  params[c("b")] <- exp(log.b) # un-transform 'b'
  -poisson.loglik(params,dat)
}
require(bbmle)
guess <- list(log.b=log(0.01))

fit0 <- mle2(f,start=guess)
fit0

fit <-  mle2(f,start=as.list(coef(fit0)))
fit
prof.b <- profile(fit)
plot(prof.b)
dat <- subset(niamey,community=="A")
params <- c(X.0=20000,Y.0=1,gamma=365/13,b=NA,p=NA)

logit <- function (p) log(p/(1-p))      # the logit transform
expit <- function (x) 1/(1+exp(-x))    # inverse logit

f <- function (log.b, logit.p) {
  par <- params
  par[c("b","p")] <- c(exp(log.b),expit(logit.p))
  -poisson.loglik(par,dat)
}

guess <- list(log.b=log(0.005),logit.p=logit(0.2))
fit0 <- mle2(f,start=guess); fit0
fit <-  mle2(f,start=as.list(coef(fit0))); fit

## now untransform the parameters:
mle <- with(
            as.list(coef(fit)),
            c(
              b=exp(log.b),
              p=expit(logit.p)
              )
            )
mle
prof2 <- profile(fit)
plot(prof2)
ci <- confint(prof2)
ci
ci[1,] <- exp(ci[1,])
ci[2,] <- expit(ci[2,])
rownames(ci) <- c("b","p")
ci
dat <- subset(niamey,community=="A")

## this time the objective function has to 
## take a vector argument
f <- function (pars) {
  par <- params
  par[c("b","p")] <- as.numeric(pars)
  poisson.loglik(par,dat)
}

b <- seq(from=0.001,to=0.005,length=50)
p <- seq(0,1,length=50)
grid <- expand.grid(b=b,p=p)
grid$loglik <- apply(grid,1,f)
grid <- subset(grid,is.finite(loglik))
require(lattice)
contourplot(loglik~b+p,data=grid,cuts=20)
b <- seq(from=0.00245,to=0.00255,length=50)
p <- seq(0.44,0.49,length=50)
grid <- expand.grid(b=b,p=p)
grid$loglik <- apply(grid,1,f)
grid <- subset(grid,is.finite(loglik))
require(lattice)
contourplot(loglik~b+p,data=grid,cuts=20)
params[c("b","p")] <- mle
times <- c(dat$biweek/26)
model.pred <- prediction(params,times)

nsim <- 1000
simdat <- replicate(
                    n=nsim,
                    rpois(n=length(model.pred),
                          lambda=params["p"]*model.pred)
                    )
quants <- t(apply(simdat,1,quantile,probs=c(0.025,0.5,0.975)))
matplot(times,quants,col="blue",lty=c(1,2,1),type='l') 
points(measles~times,data=dat,type='b',col='red')
loglik <- function (params, data) {
  times <- data$biweek/26
  pred <- prediction(params,times)
  sum(dnbinom(x=data$measles,
              mu=params["p"]*pred[-1],size=1/params["theta"],
              log=TRUE))
}

f <- function (log.b, logit.p, log.theta) {
  par <- params
  par[c("b","p","theta")] <- c(exp(log.b),
                               expit(logit.p),
                               exp(log.theta))
  -loglik(par,dat)
}

guess <- list(log.b=log(params["b"]),
              logit.p=logit(params["p"]),
              log.theta=log(1))
fit0 <- mle2(f,start=guess)
fit <-  mle2(f,start=as.list(coef(fit0)))
fit

prof3 <- profile(fit)
plot(prof3)

mle <- with(
            as.list(coef(fit)),
            c(
              b=exp(log.b),
              p=expit(logit.p),
              theta=exp(log.theta)
              )
            )

params[c("b","p","theta")] <- mle
times <- c(dat$biweek/26)
model.pred <- prediction(params,times)

nsim <- 1000
simdat <- replicate(
                    n=nsim,
                    rnbinom(n=length(model.pred),
                            mu=params["p"]*model.pred,
                            size=1/params["theta"])
                    )
quants <- t(apply(simdat,1,quantile,probs=c(0.025,0.5,0.975)))
matplot(times,quants,col="blue",lty=c(1,2,1),type='l') 
lines(times,simdat[,1],col='black')
points(measles~times,data=dat,type='b',col='red')
