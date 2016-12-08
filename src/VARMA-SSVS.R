#VARMA(p,q)-SSVS: find errors
#Stochastic search variable selection (SSVS) is applied 
#to VARMA parameters. Note that the constants for the 
#mixture variances are typically multiplied by the 
#posterior standard deviations from an unrestricted VARMA 
#that was updated previously, and these are not included 
#in this example. Since an unrestricted VARMA model may be 
#difficult to identify, this should be performed only on 
#the AR parameters.
#========================================================
library(LaplacesDemon)
#========================================================
#Data
#========================================================
data(demonfx)
Y.orig <- as.matrix(demonfx[,1:3])
Y <- diff(log(Y.orig[1:100,]))
Y.scales <- sqrt(.colVars(Y))
Y <- Y / matrix(Y.scales, nrow(Y), ncol(Y), byrow=TRUE)
T <- nrow(Y)
J <- ncol(Y)
L.P <- c(1,5,20) #Autoregressive lags
L.Q <- c(1,2) #Moving average lags
P <- length(L.P) #Autoregressive order
Q <- length(L.Q) #Moving average order
mon.names <- "LP"
parm.names <- as.parm.names(list(alpha=rep(0,J),
				  Gamma.phi=array(0, dim=c(J,J,P)), Phi=array(0, dim=c(J,J,P)),
				  Gamma.theta=array(0, dim=c(J,J,Q)), Theta=array(0, dim=c(J,J,Q)),
				  sigma=rep(0,J)))
pos.alpha <- grep("alpha", parm.names)
pos.Gamma.phi <- grep("Gamma.phi", parm.names)
pos.Phi <- grep("Phi", parm.names)
pos.Gamma.theta <- grep("Gamma.theta", parm.names)
pos.Theta <- grep("Theta", parm.names)
pos.sigma <- grep("sigma", parm.names)
PGF <- function(Data) {
alpha <- rnorm(Data$J)
Gamma.phi <- rep(1, Data$J*Data$J*Data$P)
Phi <- runif(Data$J*Data$J*Data$P, -1, 1)
Gamma.theta <- rep(1, Data$J*Data$J*Data$Q)
Theta <- rnorm(Data$J*Data$J*Data$Q)
sigma <- runif(Data$J)
return(c(alpha, Gamma.phi, Phi, Gamma.theta, Theta, sigma))
 }
MyData <- list(J=J, L.P=L.P, L.Q=L.Q, P=P, Q=Q, PGF=PGF, T=T, Y=Y,
		    mon.names=mon.names, parm.names=parm.names, pos.alpha=pos.alpha,
		    pos.Gamma.phi=pos.Gamma.phi, pos.Phi=pos.Phi,
		    pos.Gamma.theta=pos.Gamma.theta, pos.Theta=pos.Theta,
		    pos.sigma=pos.sigma)
#Model
#=======================================================
Model <- function(parm, Data)
{
### Parameters
alpha <- parm[Data$pos.alpha]
Gamma.phi <- array(parm[Data$pos.Gamma.phi],
		    dim=c(Data$J, Data$J, Data$P))
Phi.Sigma <- Gamma.phi * 10
Phi.Sigma[Gamma.phi == 0] <- 0.1
Phi <- array(parm[Data$pos.Phi], dim=c(Data$J, Data$J, Data$P))
Gamma.theta <- array(parm[Data$pos.Gamma.theta],
		      dim=c(Data$J, Data$J, Data$Q))
Theta.Sigma <- Gamma.theta * 10
Theta.Sigma[Gamma.theta == 0] <- 0.1
Theta <- array(parm[Data$pos.Theta], dim=c(Data$J, Data$J, Data$Q))
sigma <- interval(parm[Data$pos.sigma], 1e-100, Inf)
parm[Data$pos.sigma] <- sigma
### Log-Prior
alpha.prior <- sum(dnormv(alpha, 0, 1000, log=TRUE))
Gamma.phi.prior <- sum(dbern(Gamma.phi, 0.5, log=TRUE))
Phi.prior <- sum(dnorm(Phi, 0, Phi.Sigma, log=TRUE))
Gamma.theta.prior <- sum(dbern(Gamma.theta, 0.5, log=TRUE))
Theta.prior <- sum(dnorm(Theta, 0, Theta.Sigma, log=TRUE))
sigma.prior <- sum(dhalfcauchy(sigma, 25, log=TRUE))
### Log-Likelihood
mu <- matrix(alpha, Data$T, Data$J, byrow=TRUE)
for (p in 1:Data$P)
mu[(1+Data$L.P[p]):Data$T,] <- mu[(1+Data$L.P[p]):Data$T,] +
Data$Y[1:(Data$T-Data$L.P[p]),] %*%
(Gamma.phi[, , p] * Phi[, , p])
epsilon <- Data$Y - mu
for (q in 1:Data$Q)
mu[(1+Data$L.Q[q]):Data$T,] <- mu[(1+Data$L.Q[q]):Data$T,] +
epsilon[1:(Data$T-Data$L.Q[q]),] %*%
(Gamma.theta[, , q] * Theta[, , q])
Sigma <- matrix(sigma, Data$T, Data$J, byrow=TRUE)
LL <- sum(dnorm(Data$Y[(1+Data$L.P[Data$P]):Data$T,],
		 mu[(1+Data$L.P[Data$P]):Data$T,],
		 Sigma[(1+Data$L.P[Data$P]):Data$T,], log=TRUE))
### Log-Posterior
LP <- LL + alpha.prior + Gamma.phi.prior + Phi.prior +
Gamma.theta.prior + Theta.prior + sigma.prior
Modelout <- list(LP=LP, Dev=-2*LL, Monitor=LP,
			yhat=rnorm(prod(dim(mu)), mu, Sigma), parm=parm)
return(Modelout)
}
#Initial Values
#=======================================================
Initial.Values <- c(colMeans(Y), rep(1,J*J*P), runif(J*J*P,-1,1),
				 rep(1,J*J*Q), rep(0,J*J*Q), rep(1,J))
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
#errors