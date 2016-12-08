#State-Space-Model-Linear-Regression: time!!!
library(LaplacesDemon)
#========================================================
#Data
data(demonfx)
y <- demonfx[1:50,1]
X <- cbind(1, as.matrix(demonfx[1:50,2:3]))
T <- nrow(X)
J <- ncol(X)
mon.names <- "LP"
parm.names <- as.parm.names(list(b0=rep(0,J), beta=matrix(0,T,J),
					      mu=rep(0,J), phi=rep(0,J), sigma=rep(0,J+1)))
pos.b0 <- grep("b0", parm.names)
pos.beta <- grep("beta", parm.names)
pos.mu <- grep("mu", parm.names)
pos.phi <- grep("phi", parm.names)
pos.sigma <- grep("sigma", parm.names)
PGF <- function(Data) {
b0 <- rnorm(Data$J)
beta <- c(rnorm(Data$T,mean(Data$y),1), rnorm(Data$T*(Data$J-1)))
mu <- rnorm(Data$J)
phi <- runif(Data$J, -1, 1)
sigma <- runif(Data$J+1)
return(c(beta, mu, phi, sigma))
}
MyData <- list(J=J, PGF=PGF, T=T, X=X, mon.names=mon.names,
		    parm.names=parm.names, pos.b0=pos.b0, pos.beta=pos.beta,
		    pos.mu=pos.mu, pos.phi=pos.phi, pos.sigma=pos.sigma, y=y)
#Model
Model <- function(parm, Data)
{
### Parameters
b0 <- parm[Data$pos.b0]
beta <- matrix(parm[Data$pos.beta], Data$T, Data$J)
mu <- parm[Data$pos.mu]
parm[Data$pos.phi] <- phi <- interval(parm[Data$pos.phi], -1, 1)
sigma <- interval(parm[Data$pos.sigma], 1e-100, Inf)
parm[Data$pos.sigma] <- sigma
### Log-Prior
b0.prior <- sum(dnormv(b0, 0, 1000, log=TRUE))
beta.prior <- sum(dnorm(beta, matrix(mu, Data$T, Data$J, byrow=TRUE) +
			      matrix(phi, Data$T, Data$J, byrow=TRUE) *
			      (rbind(b0, beta[-Data$T,]) -
							 matrix(mu, Data$T, Data$J, byrow=TRUE)),
			      matrix(sigma[1:Data$J], Data$T, Data$J, byrow=TRUE), log=TRUE))
mu.prior <- sum(dnormv(mu, 0, 1000, log=TRUE))
phi.prior <- sum(dbeta((phi+1)/2, 20, 1.5, log=TRUE))
sigma.prior <- sum(dhalfcauchy(sigma, 25, log=TRUE))
### Log-Likelihood
mu <- rowSums(beta*Data$X)
LL <- sum(dnorm(Data$y, mu, sigma[Data$J+1], log=TRUE))
yhat <- rnorm(length(mu), mu, sigma[Data$J+1]) #Fitted
#yhat <- rnorm(length(mu), rowSums(matrix(rnorm(Data$T*Data$J,
						 # rbind(b0, beta[-Data$T,]), matrix(sigma[-Data$J], Data$T, Data$J,
												     # byrow=TRUE)), Data$T, Data$J) * Data$X), sigma[Data$J+1]) #One-step
#ahead
### Log-Posterior
LP <- LL + b0.prior + beta.prior + mu.prior + phi.prior + sigma.prior
Modelout <- list(LP=LP, Dev=-2*LL, Monitor=LP, yhat=yhat, parm=parm)
return(Modelout)
}
#Initial Values
Initial.Values <- c(rep(0,J), rep(mean(y),T), 
                    rep(0,T*(J-1)), rep(0,J),
			              rep(0,J), rep(1,J+1))
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
Initial.Values <- as.initial.values(Fit)
Fit <- LaplacesDemon(Model, Data=MyData, Initial.Values,
                     Covar=Fit$Covar, Iterations=30000, 
                     Status=46, Thinning=30,
                     Algorithm="AFSS", 
                     Specs=list(A=500, B=NULL, 
                                m=Fit$Specs$m,
                                n=1000, 
                  w=Fit$CovarDHis[nrow(Fit$CovarDHis),]))
#-------------------------------------------------------
Initial.Values <- as.initial.values(Fit)
Fit <- LaplacesDemon(Model, Data=MyData, Initial.Values,
                     Covar=Fit$Covar, Iterations=720000, 
                     Status=38, Thinning=720,
                     Algorithm="AFSS", 
                     Specs=list(A=500, B=NULL, 
                                m=Fit$Specs$m,
                                n=31000, 
                  w=Fit$CovarDHis[nrow(Fit$CovarDHis),]))

#=======================================================
#It will be wait for FPGA to continue. 720000 iterations 
#is large.