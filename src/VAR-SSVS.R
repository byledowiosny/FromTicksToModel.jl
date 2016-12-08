#VAR(p)-SSVS: find errors
#Stochastic search variable selection (SSVS) is applied 
#to VAR autoregressive parameters. Note that the constants 
#for the mixture variances are typically multiplied by the 
#posterior standard deviations from an unrestricted VAR 
#that was updated previously, and these are not included
#in this example.
#========================================================
library(LaplacesDemon)
#========================================================
#Data
data(demonfx)
Y.orig <- as.matrix(demonfx[,1:3])
Y <- diff(log(Y.orig[1:100,]))
Y.scales <- sqrt(.colVars(Y))
Y <- Y / matrix(Y.scales, nrow(Y), ncol(Y), byrow=TRUE)
T <- nrow(Y)
J <- ncol(Y)
L <- c(1,5,20) #Autoregressive lags
P <- length(L) #Autoregressive order
mon.names <- "LP"
parm.names <- as.parm.names(list(alpha=rep(0,J),
				  Gamma=array(0, dim=c(J,J,P)), Phi=array(0, dim=c(J,J,P)),
				  sigma=rep(0,J)))
pos.alpha <- grep("alpha", parm.names)
pos.Gamma <- grep("Gamma", parm.names)
pos.Phi <- grep("Phi", parm.names)
pos.sigma <- grep("sigma", parm.names)
PGF <- function(Data) {
alpha <- rnorm(Data$J)
Gamma <- rep(1, Data$J*Data$J*Data$P)
Phi <- runif(Data$J*Data$J*Data$P, -1, 1)
sigma <- runif(Data$J)
return(c(alpha, Gamma, Phi, sigma))
} 
MyData <- list(J=J, L=L, P=P, PGF=PGF, T=T, Y=Y, mon.names=mon.names,
		      parm.names=parm.names, pos.alpha=pos.alpha, pos.Gamma=pos.Gamma,
		      pos.Phi=pos.Phi, pos.sigma=pos.sigma)
#Model
Model <- function(parm, Data)
{
### Parameters
alpha <- parm[Data$pos.alpha]
Gamma <- array(parm[Data$pos.Gamma], dim=c(Data$J, Data$J, Data$P))
Phi.Sigma <- Gamma * 10
Phi.Sigma[Gamma == 0] <- 0.1
Phi <- array(parm[Data$pos.Phi], dim=c(Data$J, Data$J, Data$P))
sigma <- interval(parm[Data$pos.sigma], 1e-100, Inf)
parm[Data$pos.sigma] <- sigma
### Log-Prior
alpha.prior <- sum(dnormv(alpha, 0, 1000, log=TRUE))
Gamma.prior <- sum(dbern(Gamma, 0.5, log=TRUE))
Phi.prior <- sum(dnorm(Phi, 0, Phi.Sigma, log=TRUE))
sigma.prior <- sum(dhalfcauchy(sigma, 25, log=TRUE))
### Log-Likelihood
mu <- matrix(alpha, Data$T, Data$J, byrow=TRUE)
for (p in 1:Data$P)
mu[(1+Data$L[p]):Data$T,] <- mu[(1+Data$L[p]):Data$T,] +
Data$Y[1:(Data$T-Data$L[p]),] %*% (Gamma[, , p]*Phi[, , p])
Sigma <- matrix(sigma, Data$T, Data$J, byrow=TRUE)
LL <- sum(dnorm(Data$Y[(1+Data$L[Data$P]):Data$T,],
		 mu[(1+Data$L[Data$P]):Data$T,],
		 Sigma[(1+Data$L[Data$P]):Data$T,], log=TRUE))
### Log-Posterior
LP <- LL + alpha.prior + Gamma.prior + Phi.prior + sigma.prior
Modelout <- list(LP=LP, Dev=-2*LL, Monitor=LP,
			yhat=rnorm(prod(dim(mu)), mu, Sigma), parm=parm)
return(Modelout)
}
#Initial Values
Initial.Values <- c(colMeans(Y), rep(1,J*J*P), runif(J*J*P,-1,1), rep(1,J))
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