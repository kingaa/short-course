library(plyr)
library(reshape2)
library(pomp)
library(ggplot2)
theme_set(theme_bw())
options(stringsAsFactors=FALSE)
stopifnot(packageVersion("pomp")>="1.4.5")
set.seed(594709947L)

read.table("http://kingaa.github.io/clim-dis/stochsim/bsflu_data.txt") -> bsflu

sir3_step <- Csnippet("
  double dN_SI = rbinom(S,1-exp(-Beta*I/N*dt));
  double dN_IR1 = rbinom(I,1-exp(-gamma*dt));
  double dN_R1R2 = rbinom(R1,1-exp(-eta*dt));
  double dN_R2R3 = rbinom(R2,1-exp(-phi*dt));
  S -= dN_SI;
  I += dN_SI - dN_IR1;
  R1 += dN_IR1 - dN_R1R2;
  R2 += dN_R1R2 - dN_R2R3;
  R3 += dN_R2R3;
")

sir3_init <- Csnippet("
  S = N-1;
  I = 1;
  R1 = 0;
  R2 = 0;
  R3 = 0;
")

rmeas <- Csnippet("B = rbinom(R1,rho1); C = rbinom(R2,rho2);")

pomp(bsflu,time="day",t0=0,
     rprocess=euler.sim(sir3_step,delta.t=1/6),
     initializer=sir3_init,
     rmeasure=rmeas,
     paramnames=c("N","Beta","gamma","eta","phi","rho1","rho2"),
     statenames=c("S","I","R1","R2","R3")) -> sir3

simulate(sir3,params=c(Beta=1.5,gamma=1,rho1=0.9,rho2=0.9,
                       N=763,eta=1/3,phi=1/1.8),
         nsim=20,as.data.frame=TRUE) -> sims

ggplot(sims,mapping=aes(x=time,y=B,group=sim,color=sim=="data"))+
  geom_line(color="red")+
  geom_line(aes(y=C),color="blue")+
  guides(color=FALSE)
