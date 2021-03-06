#' ---
#' title: "Iterated filtering: principles and practice"
#' author: "Aaron A. King and Edward Ionides"
#' output:
#'   html_document:
#'     toc: yes
#' bibliography: ../course.bib
#' csl: ../ecology.csl
#' ---
#' 
#' \newcommand\prob[1]{\mathbb{P}\left[{#1}\right]}
#' \newcommand\expect[1]{\mathbb{E}\left[{#1}\right]}
#' \newcommand\var[1]{\mathrm{Var}\left[{#1}\right]}
#' \newcommand\dist[2]{\mathrm{#1}\left(#2\right)}
#' \newcommand\dlta[1]{{\Delta}{#1}}
#' \newcommand\lik{\mathcal{L}}
#' \newcommand\loglik{\ell}
#' 
#' [Licensed under the Creative Commons Attribution-NonCommercial license](http://creativecommons.org/licenses/by-nc/4.0/).
#' Please share and remix noncommercially, mentioning its origin.  
#' ![CC-BY_NC](../graphics/cc-by-nc.png)
#' This document has its origins in the [SISMID short course on Simulation-based Inference](https://kingaa.github.io/sbied/mif/mif.html) given by Aaron King and Edward Ionides.
#' 
#' Produced with **R** version `r getRversion()` and **pomp** version `r packageVersion("pomp")`.
#' 

## ----prelims,include=FALSE,purl=TRUE,cache=FALSE------------------------------
library(pomp)
options(stringsAsFactors=FALSE)
stopifnot(packageVersion("pomp")>="1.4.7")
set.seed(557976883)

#' 
#' ----------------------------
#' 
#' ## Introduction
#' 
#' This tutorial covers likelihood estimation via the method of iterated filtering.
#' It presupposes familiarity with building partially observed Markov process (POMP) objects in the **R** package **pomp** [@King2016]. 
#' **pomp** is available from [CRAN](http://cran.r-project.org/web/packages/pomp) and [github](http://kingaa.github.io/pomp).
#' This tutorial follows on from the [topic of carrying out particle filtering (also known as sequential Monte Carlo) via `pfilter` in **pomp**](../pfilter/pfilter.html). 
#' 
#' We have the following goals:
#' 
#' 1. Review the available options for inference on POMP models, to put iterated filtering in context.
#' 1. Understand how iterated filtering algorithms carry out repeated particle filtering operations, with randomly perturbed parameter values, in order to maximize the likelihood.
#' 1. Gain experience carrying out statistical investigations using iterated filtering in a relatively simple situation (fitting an SIR model to a boarding school flu outbreak).
#' 
#' Many, many statistical methods have been proposed for inference on POMP models [@He2010,@King2016]. The volume of research indicates both the importance and the difficulty of the problem. Let's start by considering three criteria to categorize inference methods: the plug-and-play property; full-information or feature-based; frequentist or Bayesian.
#' 
#' ### Plug-and-play (also called simulation-based) methods
#' 
#' - Inference methodology that calls `rprocess` but not `dprocess` is said to be *plug-and-play*.
#'   All popular modern Monte Carlo methods fall into this category. 
#' - Simulation-based is equivalent to plug-and-play. 
#'     + Historically, simulation-based meant simulating forward from initial conditions to the end of the time series. 
#'     + However, particle filtering methods instead consider each observation interval sequentially.
#'       They carry out multiple, carefully selected, simulations over each interval.
#'     + Plug-and-play methods can call `dmeasure`.
#'       A method that uses only `rprocess` and `rmeasure` is called "doubly plug-and-play".
#' - Two *non-plug-and-play* methods (expectation-maximization (EM) algorithms and Markov chain Monte Carlo (MCMC)) have theoretical convergence problems for nonlinear POMP models.
#'   The failures of these two workhorses of statistical computation have prompted development of alternative methodology.
#' 
#' ### Full-information and feature-based methods
#' 
#' - *Full-information* methods are defined to be those based on the likelihood function for the full data (i.e., likelihood-based frequentist inference and Bayesian inference).
#' - *Feature-based* methods either consider a summary statistic (a function of the data) or work with an an alternative to the likelihood.
#' - Asymptotically, full-information methods are statistically efficient and feature-based methods are not.
#'     + Loss of statistical efficiency could potentially be an acceptable tradeoff for advantages in computational efficiency.
#'     + However, good low-dimensional summary statistics can be hard to find. 
#'     + When using statistically inefficient methods, it can be hard to know how much information you are losing. 
#'     + Intuition and scientific reasoning can be inadequate tools to derive informative low-dimensional summary statistics [@shrestha11,@ionides11-statSci].
#' 
#' ### Bayesian and frequentist methods
#' 
#' - Recently, plug-and-play Bayesian methods have been discovered:
#'     + particle Markov chain Monte Carlo (PMCMC) [@andrieu10].
#'     + approximate Bayesian computation (ABC) [@toni09].
#' - Prior belief specification is both the strength and weakness of Bayesian methodology:
#'     + The likelihood surface for nonlinear POMP models often contains nonlinear ridges and variations in curvature. 
#'     + These situations bring into question the appropriateness of independent priors derived from expert opinion on marginal distributions of parameters.
#'     + They also are problematic for specification of "flat" or "uninformative" prior beliefs.
#'     + Expert opinion can be treated as data for non-Bayesian analysis.
#'       However, our primary task is to identify the information in the data under investigation, so it can be helpful to use methods that do not force us to make our conclusions dependent on quantification of prior beliefs.
#' 
#' ### Full-information, plug-and-play, frequentist methods
#' 
#' - Iterated filtering methods [@ionides06,@ionides15] are the only currently available, full-information, plug-and-play, frequentist methods for POMP models.
#' - Iterated filtering methods have been shown to solve likelihood-based inference problems for epidemiological situations which are computationally intractable for available Bayesian methodology [@ionides15].
#' 
#' ### Summary of POMP inference methodologies
#' 
#' ------------------------
#' 
#' |  |                 | __Frequentist__        | __Bayesian__        |
#' | --- | ----------------- | ------------------ | ------------- |
#' | __Plug-and-play__ | __Full-information__  | iterated filtering | particle MCMC |
#' | | __Feature-based__     | simulated moments  | ABC           |
#' | |                 | synthetic likelihood  |                 |
#' | | | | |
#' | __Not-plug-and-play__ | __Full-information__  |EM algorithm       | MCMC |
#' | |                  |Kalman filter      |
#' | | __Feature-based__     |Yule-Walker        | extended Kalman filter |
#' | |                   |extended Kalman filter |
#' 
#' -----------------------------------------------
#' 
#' 1. Yule-Walker is the method of moments for ARMA, a linear Gaussian POMP.
#' 1. The Kalman filter gives the exact likelihood for a linear Gaussian POMP. The extended Kalman filter gives an approximation for nonlinear models that can be used for quasi-likelihood or quasi-Bayesian inference.
#' 
#' 
#' ## An iterated filtering algorithm (IF2)
#' 
#' - We use the IF2 algorithm of @ionides15. 
#' - A particle filter is carried out with the parameter vector for each particle doing a random walk.
#' - At the end of the time series, the collection of parameter vectors is recycled as starting parameters for a new particle filter with a smaller random walk variance.
#' - Theoretically, this procedure converges toward the region of parameter space maximizing the maximum likelihood.
#' - Empirically, we can test this claim on examples.
#' 
#' ### IF2 algorithm pseudocode
#' 
#' __model input__:
#' Simulators for $f_{X_0}(x_0;\theta)$ and $f_{X_n|X_{n-1}}(x_n| x_{n-1}; \theta)$;
#' evaluator for $f_{Y_n|X_n}(y_n| x_n;\theta)$;
#' data, $y^*_{1:N}$ 
#' 
#' __algorithmic parameters__:
#' Number of iterations, $M$;
#' number of particles, $J$;
#' initial parameter swarm, $\{\Theta^0_j, j=1,\dots,J\}$;
#' perturbation density, $h_n(\theta|\varphi;\sigma)$;
#' perturbation scale, $\sigma_{1{:}M}$ 
#' 
#' __output__:
#' Final parameter swarm, $\{\Theta^M_j, j=1,\dots,J\}$ 
#' 
#' 1. $\quad$ For $m$ in $1{:} M$
#' 2. $\quad\quad\quad$ $\Theta^{F,m}_{0,j}\sim h_0(\theta|\Theta^{m-1}_{j}; \sigma_m)$ for $j$ in $1{:} J$
#' 3. $\quad\quad\quad$ $X_{0,j}^{F,m}\sim f_{X_0}(x_0 ; \Theta^{F,m}_{0,j})$ for $j$ in $1{:} J$
#' 4. $\quad\quad\quad$ For $n$ in $1{:} N$
#' 5. $\quad\quad\quad\quad\quad$ $\Theta^{P,m}_{n,j}\sim h_n(\theta|\Theta^{F,m}_{n-1,j},\sigma_m)$ for $j$ in $1{:} J$
#' 6. $\quad\quad\quad\quad\quad$ $X_{n,j}^{P,m}\sim f_{X_n|X_{n-1}}(x_n | X^{F,m}_{n-1,j}; \Theta^{P,m}_j)$ for $j$ in $1{:} J$
#' 7. $\quad\quad\quad\quad\quad$ $w_{n,j}^m = f_{Y_n|X_n}(y^*_n| X_{n,j}^{P,m} ; \Theta^{P,m}_{n,j})$ for $j$ in $1{:} J$
#' 8. $\quad\quad\quad\quad\quad$ Draw $k_{1{:}J}$ with $P[k_j=i]=  w_{n,i}^m\Big/\sum_{u=1}^J w_{n,u}^m$
#' 9.  $\quad\quad\quad\quad\quad$ $\Theta^{F,m}_{n,j}=\Theta^{P,m}_{n,k_j}$ and $X^{F,m}_{n,j}=X^{P,m}_{n,k_j}$ for $j$ in $1{:} J$
#' 10. $\quad\quad\quad$ End For
#' 11. $\quad\quad\quad$ Set $\Theta^{m}_{j}=\Theta^{F,m}_{N,j}$ for $j$ in $1{:} J$
#' 12. $\quad$ End For
#' 
#' __comments__:
#' 
#' - The $N$ loop (lines 4 through 10) is a basic particle filter applied to a model with stochastic perturbations to the parameters.
#' - The $M$ loop repeats this particle filter with decreasing perturbations.
#' - The superscript $F$ in $\Theta^{F,m}_{n,j}$ and $X^{F,m}_{n,j}$ denote solutions to the _filtering_ problem, with the particles $j=1,\dots,J$ providing a Monte Carlo representation of the conditional distribution at time $n$ given data $y^*_{1:n}$ for filtering iteration $m$.
#' - The superscript $P$ in $\Theta^{P,m}_{n,j}$ and $X^{P,m}_{n,j}$ denote solutions to the _prediction_ problem, with the particles $j=1,\dots,J$ providing a Monte Carlo representation of the conditional distribution at time $n$ given data $y^*_{1:n-1}$ for filtering iteration $m$.
#' - The _weight_ $w^m_{n,j}$ gives the likelihood of the data at time $n$ for particle $j$ in filtering iteration $m$.
#' 
#' 
#' ## Applying IF2 to the boarding school influenza outbreak
#' 
#' For a relatively simple epidemiological example of IF2, we consider fitting a stochastic SIR model to an influenza outbreak in a British boarding school [@anonymous78].
#' Reports consist of the number of children confined to bed for each of the 14 days of the outbreak.
#' The total number of children at the school was 763, and a total of 512 children spent time away from class.
#' Only one adult developed influenza-like illness, so adults are omitted from the data and model.
#' First, we read in the data:
#' 
## ----load_bbs-----------------------------------------------------------------
bsflu_data <- read.table("http://kingaa.github.io/short-course/stochsim/bsflu_data.txt")

#' 
#' Our model is a variation on a basic SIR Markov chain, with state $X(t)=(S(t),I(t),R_1(t),R_2(t),R_3(t))$ giving the number of individuals in the susceptible and infectious categories, and three stages of recovery.
#' The recovery stages, $R_1$, $R_2$ and $R_3$, are all modeled to be non-contagious.
#' $R_1$ consists of individuals who are bed-confined if they show symptoms;
#' $R_2$ consists of individuals who are convalescent if they showed symptoms;
#' $R_3$ consists of recovered individuals who have returned to school-work if they were symtomatic.
#' The observation on day $n$ of the observed epidemic (with $t_1$ being 22 January) consists of the numbers of children who are bed-confined and convalescent.
#' These measurements are modeled as $Y_n=(B_n,C_n)$ with $B_n\sim\mathrm{Poisson}(\rho R_1(t_n))$ and $C_n\sim\mathrm{Poisson}(\rho R_2(t_n))$.
#' Here, $\rho$ is a reporting rate corresponding to the chance of being symptomatic.
#' 

#' 
#' The index case for the epidemic was proposed to be a boy returning to Britain from Hong Kong, who was reported to have a transient febrile illness from 15 to 18 January.
#' It would therefore be reasonable to initialize the epidemic with $I(t_0)=1$ at $t_0=-6$.
#' This is a little tricky to reconcile with the rest of the data; for now, we avoid this issue by instead initializing with $I(t_0)=1$ at $t_0=0$.
#' All other individuals are modeled to be initially susceptible.
#' 
#' Our Markov transmission model is that each individual in $S$ transitions to $I$ at rate $\beta\,I(t)/N$;
#' each individual in $I$ transitions at rate $\mu_I$ to $R_1$.
#' Subsequently, the individual moves from $R_1$ to $R_2$ at  rate $\mu_{R_1}$, and finally from $R_2$ to $R_3$ at rate $\mu_{R_2}$.
#' Therefore, $1/\mu_I$ is the mean infectious time prior to bed-confinement; $1/\mu_{R1}$ is the mean duration of bed-confinement for symptomatic cases;
#' $1/\mu_{R2}$ is the mean duration of convalescence for symptomatic cases.
#' All rates have units $\mathrm{day}^{-1}$. 
#' 
#' This model has limitations and weaknesses, but writing down and fitting a model is a starting point for data analysis, not an end point.
#' In particular, one should try model variations.
#' For example, one could include a latency period for infections, or one could modify the model to give a better description of the bed-confinement and convalescence processes.
#' Ten individuals received antibiotics for secpondary infections, and they had longer bed-confinement and convalescence times.
#' Partly for this reason, we will initially fit only the bed-confinement data, using $Y_n=B_n$ for our `dmeasure`. 
#' 
#' We do not need a representation of $R_3$ since this variable has consequences neither for the dynamics of the state process nor for the data.
#' If we confine ourselves for the present to fitting only the bed-confinement data, then we need not track $R_2$.
#' For the code, we enumerate the state variables ($S$, $I$, $R_1$) and the parameters ($\beta$, $\mu_I$, $\rho$, $\mu_{R_1}$) as follows:
#' 
## ----bsflu_names--------------------------------------------------------------
statenames <- c("S","I","R1")
paramnames <- c("Beta","mu_I","mu_R1","rho")

#' 
#' In the codes below, we'll refer to the data variables by their names ($B$, $C$), as given in the `bsflu_data` data-frame:

#' 
#' Now, we write the model code:
#' 
## ----csnippets_bsflu----------------------------------------------------------
dmeas <- Csnippet("
  lik = dpois(B,rho*R1+1e-6,give_log);
")

rmeas <- Csnippet("
  B = rpois(rho*R1+1e-6);
")

rproc <- Csnippet("
  double N = 763;
  double t1 = rbinom(S,1-exp(-Beta*I/N*dt));
  double t2 = rbinom(I,1-exp(-mu_I*dt));
  double t3 = rbinom(R1,1-exp(-mu_R1*dt));
  S  -= t1;
  I  += t1 - t2;
  R1 += t2 - t3;
")

fromEst <- Csnippet("
 TBeta = exp(Beta);
 Tmu_I = exp(mu_I);
 Trho = expit(rho);
")

toEst <- Csnippet("
 TBeta = log(Beta);
 Tmu_I = log(mu_I);
 Trho = logit(rho);
")

init <- Csnippet("
 S = 762;
 I = 1;
 R1 = 0;
")

#' 
#' We build the `pomp` object:
#' 
## ----pomp_bsflu---------------------------------------------------------------
library(pomp)

pomp(
  data=subset(bsflu_data,select=-C),
  times="day",t0=0,
  rprocess=euler.sim(rproc,delta.t=1/12),
  rmeasure=rmeas,dmeasure=dmeas,
  fromEstimationScale=fromEst,toEstimationScale=toEst,
  initializer=init,
  statenames=statenames,
  paramnames=paramnames
) -> bsflu


#' 
#' 
#' ### Testing the codes.
#' 
#' To develop and debug code, it is useful to have example codes that run quickly.
#' Here, we run some simulations and a particle filter, simply to check that the codes are working correctly.
#' We'll use the following parameters, derived from our earlier explorations:
## ----start_params-------------------------------------------------------------
params <- c(Beta=2,mu_I=1,rho=0.9,mu_R1=1/3,mu_R2=1/2)

#' 
#' Now to run and plot some simulations:
## ----init_sim-----------------------------------------------------------------
y <- simulate(bsflu,params=params,nsim=10,as.data.frame=TRUE)


#' 
#' Before engaging in iterated filtering, it is a good idea to check that the basic particle filter is working since iterated filtering builds on this technique.
#' The simulations above check the `rprocess` and `rmeasure` codes;
#' the particle filter depends on the `rprocess` and `dmeasure` codes and so is a check of the latter.
#' 
## ----init_pfilter-------------------------------------------------------------
pf <- pfilter(bsflu,params=params,Np=1000)


#' 
#' These plots show the data along with the *effective sample size* of the particle filter (`ess`) and the log likelihood of each observation conditional on the preceding ones (`cond.logLik`).
#' 
#' ### Setting up the estimation problem.
#' 
#' Let's treat $\mu_{R_1}$ and  $\mu_{R_2}$ as known, and fix these parameters at the empirical means of the bed-confinement and convalescence times for symptomatic cases, respectively:
#' 
## ----fixed_params-------------------------------------------------------------
(fixed_params <- with(bsflu_data,c(mu_R1=1/(sum(B)/512),mu_R2=1/(sum(C)/512))))

#' 
#' We will estimate $\beta$, $\mu_I$, and $\rho$.
#' 
#' It will be helpful to parallelize most of the computations.
#' Most machines nowadays have multiple cores and using this computational capacity involves only:
#' 
#' i. the following lines of code to let R know you plan to use multiple processors; 
#' i. the parallel for loop provided by the **foreach** package; and
#' i. proper attention to the use of parallel random number generators.
#' 
## ----parallel-setup,cache=FALSE-----------------------------------------------
library(foreach)
library(doParallel)

registerDoParallel()

#' 
#' The first two lines load the **foreach** and **doParallel** packages, the latter being a "backend" for the **foreach** package.
#' The next line tells **foreach** that we will use the **doParallel** backend.
#' By default, **R** will guess how many concurrent **R** processes to run on this single multicore machine.
#' 
#' ### Running a particle filter.
#' 
#' We proceed to carry out replicated particle filters at an initial guess of $\beta=2$, $\mu_I=1$, and $\rho=0.9$.
#' 
## ----pf-----------------------------------------------------------------------
stew(file="pf.rda",{
  t_pf <- system.time(
    pf <- foreach(i=1:10,.packages='pomp',
                  .options.multicore=list(set.seed=TRUE),
                  .export=c("bsflu","fixed_params")
    ) %dopar% {
      pfilter(bsflu,params=c(Beta=2,mu_I=1,rho=0.9,fixed_params),Np=10000)
    }
  )
  n_pf <- getDoParWorkers()
},seed=625904618,kind="L'Ecuyer")

(L_pf <- logmeanexp(sapply(pf,logLik),se=TRUE))

#' 
#' In `r round(t_pf["elapsed"],2)` seconds on a `r n_pf`-core machine, we obtain an unbiased likelihood estimate of `r round(L_pf[1],1)` with a Monte Carlo standard error of `r signif(L_pf[2],2)`.
#' 
#' ### Building up a picture of the likelihood surface
#' 
#' Given a model and a set of data, the likelihood surface is well defined, though it may be difficult to visualize.
#' We can develop a progressively more complete picture of this surface by storing likelihood estimates whenever we compute them.
#' In particular, it is very useful to construct a database within which to store the likelihood of every point for which we have an estimated likelihood.
#' This will become progressively more complete as our parameter-space search goes on.
#' At this point, we've computed the likelihood at a single point.
#' Let's store this point, together with the estimated likelihood and our estimate of the standard error on that likelihood, in a CSV file:
## ----init_csv-----------------------------------------------------------------
results <- as.data.frame(as.list(c(coef(pf[[1]]),loglik=L_pf[1],loglik=L_pf[2])))
write.csv(results,file="bsflu_params.csv",row.names=FALSE)

#' 
#' ### A local search of the likelihood surface
#' 
#' Let's carry out a local search using `mif2` around this point in parameter space. 
#' For that, we need to set the `rw.sd` and `cooling.fraction.50` algorithmic parameters.
#' Since $\beta$ and $\mu_I$ will be estimated on the log scale, and we expect that multiplicative perturbations of these parameters will have roughly similar effects on the likelihood, we'll use a perturbation size of $0.02$, which we imagine will have a small but non-negligible effect.
#' For simplicity, we'll use the same perturbation size on $\rho$.
#' We fix `cooling.fraction.50=0.5`, so that after 50 `mif2` iterations, the perturbations are reduced to half their original magnitudes.
#' 
## ----box_search_local---------------------------------------------------------
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
        start=c(Beta=2,mu_I=1,rho=0.9,fixed_params),
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


#' 
#' Although the filtering carried out by `mif2` in the final filtering iteration generates an approximation to the likelihood at the resulting point estimate, this is not usually good enough for reliable inference.
#' Partly, this is because parameter perturbations are applied in the last filtering iteration.
#' Partly, this is because `mif2` is usually carried out with a smaller number of particles than is necessary for a good likelihood evaluation---the errors in `mif2` average out over many iterations of the filtering.
#' Therefore, we evaluate the likelihood, together with a standard error, using replicated particle filters at each point estimate:
#' 
## ----lik_local----------------------------------------------------------------
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

#' 
#' This investigation took  `r round(t_local_mif["elapsed"],0)` sec for the maximization and `r round(t_local_eval["elapsed"],0)` sec for the likelihood evaluation.
#' These repeated stochastic maximizations can also show us the geometry of the likelihood surface in a neighborhood of this point estimate:
#' 

#' 
#' We add these newly explored points to our database:
## ----local_database-----------------------------------------------------------
results <- rbind(results,results_local[names(results)])
write.csv(results,file="bsflu_params.csv",row.names=FALSE)

#' 
#' ### A global search of the likelihood surface using randomized starting values
#' 
#' When carrying out parameter estimation for dynamic systems, we need to specify beginning values for both the dynamic system (in the state space) and the parameters (in the parameter space).
#' By convention, we use  _initial values_ for the initialization of the dynamic system and _starting values_ for initialization of the parameter search.
#' 
#' Practical parameter estimation involves trying many starting values for the parameters.
#' One can specify a large box in parameter space that contains all parameter vectors which seem remotely sensible.
#' If an estimation method gives stable conclusions with starting values drawn randomly from this box, this gives some confidence that an adequate global search has been carried out. 
#' 
#' For our flu model, a box containing reasonable parameter values might be
#' 
## ----box_global---------------------------------------------------------------
params_box <- rbind(
  Beta=c(1,5),
  mu_I=c(0.5,3),
  rho = c(0.5,1)
)

#' 
#' We are now ready to carry out likelihood maximizations from diverse starting points.
#' 
## ----box_search_global--------------------------------------------------------
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
      mf <- mif2(mf1,start=c(unlist(guess),fixed_params))
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

#' 
#' In the above, we took advantage of a general **pomp** behavior:
#' re-running a command on an object (i.e., `mif2` on `mf1`) created by the same command preserves the algorithmic arguments.
#' 
#' Evaluation of the best result of this search gives a likelihood of `r round(max(results_global$loglik),1)` with a standard error of `r round(results_global$loglik.se[which.max(results_global$loglik)],2)`.
#' This took in `r round(t_global["elapsed"]/60,1)` minutes altogether on a machine with `r n_global` processors.
#' One can visualize aspects of the global geometry of the likelihood surface with a scatterplot matrix of these diverse parameter estimates:
#' 

#' 
#' We see that optimization attempts from diverse remote starting points end up with comparable likelihoods, even when the parameter values are quite distinct.
#' This gives us some confidence in our maximization procedure. 
#' 
#' 
#' --------------------------
#' 
#' #### Exercise: Fit more parameters.
#' 
#' Try to estimate $\beta$, $\mu_I$, $\rho$, and $\mu_{R1}$ simultaneously.
#' Does your estimate of $\mu_{R1}$ differ from the value we computed from the raw data?
#' How do you interpret the agreement or lack thereof?
#' 
#' --------------------------
#' 
#' #### Exercise: Modify the measurement model
#' 
#' The Poisson measurement model used here may not seem easy to interpret.
#' Formulate an alternative measurement model and maximize the likelihood to compare the alternative model.
#' 
#' --------------------------
#' 
#' #### Exercise: Construct a profile likelihood
#' 
#' How strong is the evidence about the contact rate, $\beta$, given this model and data? Use `mif2` to construct a profile likelihood. Due to time constraints, you may be able to compute only a preliminary version.
#' 
#' It is also possible to profile over the basic reproduction number, $R_0=\beta /\mu_I$. Is this more or less well determined that $\beta$ for this model and data?
#' 
#' --------------------------
#' 
#' #### Exercise: Check the model source code.
#' 
#' Check the source code for the `bsflu` `pomp` object.
#' Does the code implement the model described?
#' 
#' For various reasons, it can be surprisingly hard to make sure that the written equations and the code are perfectly matched.
#' Here are some things to think about:
#' 
#' 1. Papers should be written to be readable to as broad a community as possible.
#' Code must be written to run successfully.
#' People do not want to clutter papers with numerical details which they hope and belief are scientifically irrelevant.
#' What problems can arise due to this, and what solutions are available?
#' 1. Suppose that there is an error in the coding of `rprocess`.
#' Suppose that plug-and-play statistical methodology is used to infer parameters.
#' A conscientious researcher carries out a simulation study, using `simulate` to generate some realizations from the fitted model and checking that the inference methodology can successfully recover the known parameters for this model, up to some statistical error.
#' Will this procedure help to identify the error in `rprocess`?
#' If not, how does one debug `rprocess`?
#' What research practices help minimize the risk of errors in simulation code?
#' 
#' --------------------------
#' 
#' ## Choosing the algorithmic settings for IF2
#' 
#' * The initial parameter swarm, $\{ \Theta^0_j, j=1,\dots,J\}$, usually consists of $J$ identical replications of some starting parameter vector.
#' * $J$ is set to be sufficient for particle filtering. By the time of the last iteration ($m=M$) one should not have effective sample size close to 1. 
#' * Perturbations are usually chosen to be Gaussian, with $\sigma_m$ being a scale factor for iteration $m$:
#' $$h_n(\theta|\varphi;\sigma) \sim N[\varphi, \sigma^2_m V_n].$$
#' * $V_n$ is usually taken to be diagonal,
#' $$ V_n = \left( \begin{array}{ccccc}
#' v_{1,n}^2 & 0 & 0 & \cdots & 0 \\
#' 0 & v_{2,n}^2 &  0 & \cdots & 0 \\
#' 0 & 0 & v_{3,n}^2 & \cdots & 0 \\
#' \vdots & \vdots & \vdots & \ddots & \vdots \\
#' 0 & 0 & 0 & \cdots & v_{p,n}^2 \end{array}\right).$$
#' + If $\theta_i$ is a parameter that affects the dynamics or observations throughout the timeseries, it is called a __regular parameter__, and it is often appropriate to specify $$v_{i,n} = v_i.$$
#' + If $\theta_j$ is a parameter that affects only the initial conditions of the dynamic model, it is called an __initial value parameter__ (IVP) and it is appropriate to specify $$v_{j,n} = \left\{\begin{array}{ll} v_j & \mbox{if $n=0$} \\0 & \mbox{if $n>0$} \end{array}\right.$$
#' + If $\theta_k$ is a break-point parameter that models how the system changes at time $t_q$ then $\theta_k$ is like an IVP at time $t_q$ and it is appropriate to specify $$v_{j,n} = \left\{\begin{array}{ll} v_j & \mbox{if $n=q$} \\	0 & \mbox{if $n\neq q$} \end{array}\right.$$
#' * $\sigma_{1:M}$ is called a __cooling schedule__, following a thermodynamic analogy popularized by [simulated annealing](https://en.wikipedia.org/wiki/Simulated_annealing).
#' As $\sigma_m$ becomes small, the system cools toward a "freezing point".
#' If the algorithm is working sucessfully, the freezing point should be close to the lowest-energy state of the system, i.e., the MLE.
#' * It is generally helpful for optimization to provide transformations of the parameters so that (on the estimation scale) they are real-valued and have uncertainty on the order of 1 unit.
#' For example, one typically takes a logarithmic transformation of positive parameters and a logistic transformation of $[0,1]$ valued parameters.
#' + On this scale, it is surprisingly often effective to take $$v_i \sim 0.02$$ for regular parameters (RPs) and $$v_j \sim 0.1$$ for initial value parameters (IVPs).
#' * We suppose that $\sigma_1=1$, since the scale of the parameters is addressed by the matrix $V_n$.
#' Early on in an investigation, one might take $M=100$ and $\sigma_M=0.1$.
#' Later on, consideration of diagnostic plots may suggest refinements. 
#' * It is surprising that useful general advice exists for these quantities that could in principle be highly model-specific.
#' Here is one possible explanation: the precision of interest is often the second significant figure and there are often order 100 observations (10 monthly obsevations would be too few to fit a mechanistic model;
#' 1000 would be unusual for an epidemiological system). 
#' 
#' --------------------------
#' 
#' #### Exercise: Assessing and improving algorithmic parameters
#' 
#' Develop your own heuristics to try to improve the performance of `mif2` in the previous example.
#' Specifically, for a global optimization procedure carried out using random starting values in the specified box, let
#' $\hat\Theta_{\mathrm{max}}$ be a random Monte Carlo estimate of the resulting MLE, and let $\hat\theta$ be the true (unknown) MLE.
#' We can define the maximization error in the log likelihood to be
#' $$e = \ell(\hat\theta) - E[\ell(\hat\Theta_{\mathrm{max}})].$$
#' We cannot directly evaluate $e$, since there is also Monte Carlo error in our evaluation of $\ell(\theta)$, but we can compute it up to a known precision.
#' Plan some code to estimates $e$ for a search procedure using a computational effort of $JM=2\times 10^7$, comparable to that used for each mif computation in the global search.
#' Discuss the strengths and weaknesses of this quantification of optimization success.
#' See if you can choose $J$ and $M$ subject to this constraint, together with choices of `rw.sd` and the cooling rate, `cooling.fraction.50`, to arrive at a quantifiably better procedure.
#' Computationally, you may not be readily able to run your full procedure, but you could run a quicker version of it.
#' 
#' --------------------------
#' 
#' #### Exercise: Finding sharp peaks in the likelihood surface
#' 
#' Even in this small, 3 parameter example, it takes a considerable amount of computation to find the global maximum (with values of $\beta$ around 0.004) starting from uniform draws in the specified box.
#' The problem is that, on the scale on which "uniform" is defined, the peak around $\beta\approx 0.004$ is very narrow.
#' Propose and test a more favorable way to draw starting parameters for the global search, with better scale invariance properties.
#' 
#' --------------------------
#' 
#' #### Exercise: Adding a latent class
#' 
#' Modify the model to include a latent period between becoming exposed and becoming infectious. See what effect this has on the maximized likelihood.
#' 
#' --------------------------
#' 
#' ## [Back to course homepage](http://kingaa.github.io/short-course)
#' ## [**R** codes for this document](http://raw.githubusercontent.com/kingaa/short-course/master/mif/mif.R)
#' 
#' ----------------------
#' 
#' ## References
