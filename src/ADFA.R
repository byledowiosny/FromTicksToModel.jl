#Factor-Analysis-Approximate-Dynamic: time!!!
#The Approximate Dynamic Factor Analysis (ADFA) model has 
#many names, including the
#approximate factor model and approximate dynamic factor 
#model. An ADFA is a Dynamic
#Factor Analysis (DFA) in which the factor scores of the 
#dynamic factors are approximated
#with principal components. This is a combination of 
#principal components and common fac-
#tor analysis, in which the factor loadings of common 
#factors are estimated from the data and
#factor scores are estimated from principal components. 
#This is a two-stage model: principal
#components are estimated in the first stage and a 
#decision is made regarding how many prin-
#cipal components to retain, and ADFA is estimated in the 
#second stage. For more information
#on DFA, see section 33.
#========================================================
#Data
library(LaplacesDemon)
#========================================================
data(demonfx)
Y.orig <- as.matrix(demonfx)
Y <- diff(log(Y.orig[1:100,]))
Y.scales <- sqrt(.colVars(Y))
Y <- Y / matrix(Y.scales, nrow(Y), ncol(Y), byrow=TRUE)
T <- nrow(Y) #Number of time-periods
J <- ncol(Y) #Number of time-series
P <- 7 #Number of approximate factors
PCA <- prcomp(Y, scale=TRUE)
F <- PCA$x[,1:P]
mon.names <- "LP"
parm.names <- as.parm.names(list(Lambda=matrix(0,P,J), sigma=rep(0,J)))
pos.Lambda <- grep("Lambda", parm.names)
pos.sigma <- grep("sigma", parm.names)
PGF <- function(Data) {
Lambda <- rnorm(Data$P*Data$J)
sigma <- runif(Data$J)
return(c(Lambda, sigma))
}
MyData <- list(F=F, J=J, P=P, PGF=PGF, T=T, Y=Y, mon.names=mon.names,
		    parm.names=parm.names, pos.Lambda=pos.Lambda, pos.sigma=pos.sigma)
#Model
#=======================================================
Model <- function(parm, Data)
{
### Parameters
Lambda <- matrix(parm[Data$pos.Lambda], Data$P, Data$J)
sigma <- interval(parm[Data$pos.sigma], 1e-100, Inf)
parm[Data$pos.sigma] <- sigma
### Log-Prior
Lambda.prior <- sum(dnorm(Lambda, 0, 1, log=TRUE))
sigma.prior <- sum(dhalfcauchy(sigma, 25, log=TRUE))
### Log-Likelihood
mu <- tcrossprod(rbind(rep(0,Data$P), F[-Data$T,]), t(Lambda))
Sigma <- matrix(sigma, Data$T, Data$J, byrow=TRUE)
LL <- sum(dnorm(Data$Y[-1,], mu[-1,], Sigma[-1,], log=TRUE))
### Log-Posterior
LP <- LL + Lambda.prior + sigma.prior
Modelout <- list(LP=LP, Dev=-2*LL, Monitor=LP,
			yhat=rnorm(prod(dim(mu)), mu, Sigma), parm=parm)
return(Modelout)
}
#Initial Values
#=======================================================
Initial.Values <- c(rep(0,P*J), rep(1,J))
#=======================================================
set.seed(666)
Fit <- LaplacesDemon(Model, Data=MyData, Initial.Values,
                     Covar=NULL, Iterations=1000, 
                     Status=100, Thinning=1,
                     Algorithm="AFSS", 
                     Specs=list(A=500, B=NULL, m=100, 
                                n=0, w=1))
#=======================================================
Consort(Fit)
#=======================================================
plot(Fit, BurnIn=500, MyData, PDF=FALSE, Parms=NULL)
plot(BMK.Diagnostic(Fit$Posterior1[501:1000,]))
#=======================================================
Pred <- predict(Fit, Model, MyData, CPUs=1)
summary(Pred, Discrep="Chi-Square")
#=======================================================
plot(Pred, Style="Density", Rows=1:9)
plot(Pred, Style="Fitted")
#=======================================================
Laplace's Demon has been appeased, and suggests
the marginal posterior samples should be plotted
and subjected to any other MCMC diagnostic deemed
fit before using these samples for inference.
Concordance:  0.961668 