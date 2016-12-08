#VAR(p)-GARCH(1,1)-M: find errors
#The Minnesota prior is applied to the VAR parameters, 
#and the multivariate GARCH component is estimated with 
#asymmetric BEKK. Compared to VAR(p) or VARMA(p,q), this
#is computationally intensive. However, it also tends to 
#result in a substantial improvement when time for 
#computation is feasible. This model also performs well 
#when SSVS is applied to all parameters except C, though 
#it is even more computationally intensive, and is not 
#shown here.
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
L <- c(1,5,20) #Autoregressive lags
P <- length(L) #Autoregressive order
Phi.mu <- array(0, dim=c(J,J,P))
Phi.mu[, , 1] <- diag(J)
C <- matrix(NA, J, J)
C[lower.tri(C, diag=TRUE)] <- 0
mon.names <- "LP"
parm.names <- as.parm.names(list(alpha=rep(0,J), delta=matrix(0,J,J),
						 Phi=array(0, dim=c(J,J,P)), C=C, A=matrix(0,J,J), B=matrix(0,J,J),
						 D=matrix(0,J,J)))
pos.alpha <- grep("alpha", parm.names)
pos.delta <- grep("delta", parm.names)
pos.Phi <- grep("Phi", parm.names)
pos.C <- grep("C", parm.names)
pos.A <- grep("A", parm.names)
pos.B <- grep("B", parm.names)
pos.D <- grep("D", parm.names)
PGF <- function(Data) {
alpha <- rnorm(Data$J)
delta <- rnorm(Data$J*Data$J)
Phi <- runif(Data$J*Data$J*Data$P, -1, 1)
C <- runif(Data$J*(Data$J+1)/2)
A <- as.vector(diag(Data$J)) + runif(Data$J*Data$J, -0.1, 0.1)
B <- as.vector(diag(Data$J)) + runif(Data$J*Data$J, -0.1, 0.1)
D <- as.vector(diag(Data$J)) + runif(Data$J*Data$J, -0.1, 0.1)
return(c(alpha, delta, Phi, C, A, B, D))
}
MyData <- list(J=J, L=L, P=P, PGF=PGF, Phi.mu=Phi.mu, T=T, Y=Y,
		    mon.names=mon.names, parm.names=parm.names, pos.alpha=pos.alpha,
		    pos.delta=pos.delta, pos.Phi=pos.Phi, pos.C=pos.C, pos.A=pos.A,
		    pos.B=pos.B, pos.D=pos.D)
#Model
#======================================================
Model <- function(parm, Data)
{
### Parameters
alpha <- parm[Data$pos.alpha]
delta <- matrix(parm[Data$pos.delta], Data$J, Data$J)
Phi <- array(parm[Data$pos.Phi], dim=c(Data$J, Data$J, Data$P))
C <- matrix(0, Data$J, Data$J)
C[lower.tri(C, diag=TRUE)] <- parm[Data$pos.C]
diag(C) <- abs(diag(C))
parm[Data$pos.C] <- C[lower.tri(C, diag=TRUE)]
Omega <- C %*% t(C)
A <- matrix(parm[Data$pos.A], Data$J, Data$J)
A[1,1] <- abs(A[1,1])
parm[Data$pos.A] <- as.vector(A)
B <- matrix(parm[Data$pos.B], Data$J, Data$J)
B[1,1] <- abs(B[1,1])
parm[Data$pos.B] <- as.vector(B)
D <- matrix(parm[Data$pos.D], Data$J, Data$J)
D[1,1] <- abs(D[1,1])
parm[Data$pos.D] <- as.vector(D)
### Log-Prior
alpha.prior <- sum(dnormv(alpha, 0, 1000, log=TRUE))
delta.prior <- sum(dnormv(delta, 0, 1000, log=TRUE))
Sigma <- MinnesotaPrior(Data$J, lags=Data$L, lambda=1,
				theta=0.5, sqrt(diag(Omega)))
Phi.prior <- sum(dnormv(Phi, Data$Phi.mu, Sigma, log=TRUE))
C.prior <- sum(dnormv(C[lower.tri(C, diag=TRUE)], 0, 100, log=TRUE))
A.prior <- sum(dnormv(A, 0, 100, log=TRUE))
B.prior <- sum(dnormv(B, 0, 100, log=TRUE))
D.prior <- sum(dnormv(D, 0, 100, log=TRUE))
### Log-Likelihood
mu <- matrix(alpha, Data$T, Data$J, byrow=TRUE)
for (p in 1:Data$P)
mu[(1+Data$L[p]):Data$T,] <- mu[(1+Data$L[p]):Data$T,] +
Data$Y[1:(Data$T-Data$L[p]),] %*% Phi[, , p]
LL <- 0
Yhat <- Data$Y
H <- array(Omega, dim=c(Data$J, Data$J, Data$T))
for (t in 2:Data$T) {
eps <- Data$Y - mu
zeta <- matrix(interval(eps, -Inf, 0, reflect=FALSE), Data$T,
						      Data$J)
part1 <- t(A) %*% eps[t-1,] %*% t(eps[t-1,]) %*% A
part2 <- t(B) %*% H[, , t-1] %*% B
part3 <- t(D) %*% zeta[t-1,] %*% t(zeta[t-1,]) %*% D
H0 <- Omega + part1 + part2 + part3
H0[upper.tri(H0, diag=TRUE)] <- t(H0)[upper.tri(H0, diag=TRUE)]
H[, , t] <- H0
mu[t-1,] <- mu[t-1,] + colMeans(H[, , t-1]*delta)
Sigma <- MinnesotaPrior(Data$J, lags=Data$L, lambda=1,
				theta=0.5, sqrt(diag(H[, , t])))
Phi.prior <- Phi.prior + sum(dnormv(Phi, Data$Phi.mu, Sigma,
					 log=TRUE))
LL <- LL + dmvn(Y[t,], mu[t,], H[, , t], log=TRUE)
Yhat[t,] <- rmvn(1, mu[t,], H[, , t])
}
Phi.prior <- Phi.prior / Data$T
### Log-Posterior
LP <- LL + alpha.prior + delta.prior + Phi.prior + C.prior +
A.prior + B.prior + D.prior
Modelout <- list(LP=LP, Dev=-2*LL, Monitor=LP, yhat=Yhat, parm=parm)
return(Modelout)
}
#Initial Values
#=======================================================
Initial.Values <- c(colMeans(Y), rnorm(J*J), runif(J*J*P,-1,1),
				 runif(J*(J+1)/2), as.vector(diag(J)), as.vector(diag(J)),
				 as.vector(diag(J)))
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