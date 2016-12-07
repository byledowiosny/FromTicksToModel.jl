library(LaplacesDemonCpp)
####################################################
data(action)
str(action)
y <- as.numeric(action[,1])
X <- cbind(1, as.matrix(action[,2:3]))
for (j in 2:ncol(X)) X[,j] <- CenterScale(X[,j])
N <- length(y)
J <- length(unique(y)) #Number of categories in y
K <- ncol(X) #Number of predictors (including the intercept)
S <- diag(J-1)
mon.names <- "LP"
parm.names <- as.parm.names(list(beta=array(0, dim=c(J-1,K,N)),
				  zeta.mu=matrix(0,J-1,K), zeta.sigma=matrix(0,J-1,K)))
pos.beta <- grep("beta", parm.names)
pos.zeta.mu <- grep("zeta.mu", parm.names)
pos.zeta.sigma <- grep("zeta.sigma", parm.names)
PGF <- function(Data) {
zeta.mu <- matrix(rnorm((Data$J-1)*Data$K), Data$J-1, Data$K)
zeta.sigma <- matrix(runif((Data$J-1)*Data$K), Data$J-1, Data$K)
beta <- array(rnorm((Data$J-1)*Data$K*Data$N),
	       dim=c( Data$J-1, Data$K, Data$N))
return(c(beta, as.vector(zeta.mu), as.vector(zeta.sigma)))
}
MyData <- list(J=J, K=K, N=N, PGF=PGF, S=S, X=X, mon.names=mon.names,
		    parm.names=parm.names, pos.beta=pos.beta, pos.zeta.mu=pos.zeta.mu,
		    pos.zeta.sigma=pos.zeta.sigma, y=y)
######################
Model <- function(parm, Data)
{
### Parameters
beta <- array(parm[Data$pos.beta], dim=c(Data$J-1, Data$K, Data$N))
zeta.mu <- matrix(parm[Data$pos.zeta.mu], Data$J-1, Data$K)zeta.sigma <- matrix(interval(parm[Data$pos.zeta.sigma], 1e-100, Inf),
										 Data$J-1, Data$K)
parm[Data$pos.zeta.sigma] <- as.vector(zeta.sigma)
### Log-Hyperprior
zeta.mu.prior <- sum(dnormv(zeta.mu, 0, 1000, log=TRUE))
zeta.sigma.prior <- sum(dhalfcauchy(zeta.sigma, 25, log=TRUE))
### Log-Prior
beta.prior <- sum(dnorm(beta, zeta.mu, zeta.sigma, log=TRUE))
### Log-Likelihood
mu <- matrix(0, Data$N, Data$J)
for (j in 1:(Data$J-1)) mu[,j] <- rowSums(Data$X * t(beta[j, , ]))
mu <- interval(mu, -700, 700, reflect=FALSE)
phi <- exp(mu)
p <- phi / rowSums(phi)
LL <- sum(dcat(Data$y, p, log=TRUE))
### Log-Posterior
LP <- LL + beta.prior + zeta.mu.prior + zeta.sigma.prior
Modelout <- list(LP=LP, Dev=-2*LL, Monitor=LP, yhat=rcat(nrow(p), p),
			parm=parm)
return(Modelout)
}
######################
Initial.Values <- c(rep(0,(J-1)*K*N), rep(0,(J-1)*K), rep(1,(J-1)*K))

