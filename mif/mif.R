library(pomp)
options(stringsAsFactors=FALSE)
stopifnot(packageVersion("pomp")>="1.4.7")
set.seed(557976883)
bsflu_data <- read.table("http://kingaa.github.io/short-course/stochsim/bsflu_data.txt")
statenames <- c("S","I","R1","R2")
paramnames <- c("Beta","mu_I","rho","mu_R1","mu_R2")
dmeas <- "
  lik = dpois(B,rho*R1+1e-6,give_log);
"

rmeas <- "
  B = rpois(rho*R1+1e-6);
  C = rpois(rho*R2);
"

rproc <- "
  double t1 = rbinom(S,1-exp(-Beta*I*dt));
  double t2 = rbinom(I,1-exp(-dt*mu_I));
  double t3 = rbinom(R1,1-exp(-dt*mu_R1));
  double t4 = rbinom(R2,1-exp(-dt*mu_R2));
  S -= t1;
  I += t1 - t2;
  R1 += t2 - t3;
  R2 += t3 - t4;
"

fromEst <- "
 TBeta = exp(Beta);
 Tmu_I = exp(mu_I);
 Trho = expit(rho);
"

toEst <- "
 TBeta = log(Beta);
 Tmu_I = log(mu_I);
 Trho = logit(rho);
"

init <- "
 S=762;
 I=1;
 R1=0;
 R2=0;
"
library(pomp)
pomp(
  data=bsflu_data,
  times="day",
  t0=0,
  rprocess=euler.sim(
    step.fun=Csnippet(rproc),
    delta.t=1/12
  ),
  rmeasure=Csnippet(rmeas),
  dmeasure=Csnippet(dmeas),
  fromEstimationScale=Csnippet(fromEst),
  toEstimationScale=Csnippet(toEst),
  initializer=Csnippet(init),
  statenames=statenames,
  paramnames=paramnames
) -> bsflu
params <- c(Beta=0.005,mu_I=2,rho=0.9,mu_R1=1/3,mu_R2=1/2)
y <- simulate(bsflu,params=params,nsim=10,as.data.frame=TRUE)
pf <- pfilter(bsflu,params=params,Np=1000)
(fixed_params <- with(bsflu_data,c(mu_R1=1/(sum(B)/512),mu_R2=1/(sum(C)/512))))
library(foreach)
library(doParallel)

registerDoParallel()
stew(file="pf.rda",{
  t_pf <- system.time(
    pf <- foreach(i=1:10,.packages='pomp',
                  .options.multicore=list(set.seed=TRUE),
                  .export=c("bsflu","fixed_params")
    ) %dopar% {
      pfilter(bsflu,params=c(Beta=0.01,mu_I=2,rho=0.9,fixed_params),Np=10000)
    }
  )
  n_pf <- getDoParWorkers()
},seed=625904618,kind="L'Ecuyer")

(L_pf <- logmeanexp(sapply(pf,logLik),se=TRUE))
results <- as.data.frame(as.list(c(coef(pf[[1]]),loglik=L_pf[1],loglik=L_pf[2])))
write.csv(results,file="bsflu_params.csv",row.names=FALSE)
stew(file="box_search_local.rda",{
  t_local_mif <- system.time({
    mifs_local <- foreach(i=1:20,
                          .packages='pomp',
                          .combine=c, 
                          .options.multicore=list(set.seed=TRUE),
                          .export=c("bsflu","fixed_params")
    ) %dopar%  
    {
      mif2(
        bsflu,
        start=c(Beta=0.01,mu_I=2,rho=0.9,fixed_params),
        Np=2000,
        Nmif=50,
        cooling.type="geometric",
        cooling.fraction.50=0.5,
        transform=TRUE,
        rw.sd=rw.sd(Beta=0.02,mu_I=0.02,rho=0.02)
      )
    }
  })
},seed=482947940,kind="L'Ecuyer")
stew(file="lik_local.rda",{
  t_local_eval <- system.time({
    results_local <- foreach(mf=mifs_local,
                             .packages='pomp',
                             .combine=rbind,
                             .options.multicore=list(set.seed=TRUE)
    ) %dopar% 
    {
      evals <- replicate(10, logLik(pfilter(mf,Np=20000)))
      ll <- logmeanexp(evals,se=TRUE)
      c(coef(mf),loglik=ll[1],loglik=ll[2])
    }
  })
},seed=900242057,kind="L'Ecuyer")
results_local <- as.data.frame(results_local)
results <- rbind(results,results_local[names(results)])
write.csv(results,file="bsflu_params.csv",row.names=FALSE)
params_box <- rbind(
  Beta=c(0.001,0.01),
  mu_I=c(0.5,3),
  rho = c(0.5,1)
)
stew(file="box_search_global.rda",{
  n_global <- getDoParWorkers()
  t_global <- system.time({
    mf1 <- mifs_local[[1]]
    guesses <- as.data.frame(apply(params_box,1,function(x)runif(300,x[1],x[2])))
    results_global <- foreach(guess=iter(guesses,"row"), 
                              .packages='pomp', 
                              .combine=rbind,
                              .options.multicore=list(set.seed=TRUE),
                              .export=c("mf1","fixed_params")
    ) %dopar% 
    {
      mf <- mif2(mf1,start=c(unlist(guess),fixed_params),
                 cooling.type='geometric')
      mf <- mif2(mf,Nmif=100)
      ll <- replicate(10,logLik(pfilter(mf,Np=100000)))
      ll <- logmeanexp(ll,se=TRUE)
      c(coef(mf),loglik=ll[1],loglik=ll[2])
    }
  })
},seed=1270401374,kind="L'Ecuyer")
results_global <- as.data.frame(results_global)
results <- rbind(results,results_global[names(results)])
write.csv(results,file="bsflu_params.csv",row.names=FALSE)
