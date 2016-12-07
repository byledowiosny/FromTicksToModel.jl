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
mon.names <- "LP"
parm.names <- as.parm.names(list(beta=matrix(0,J-1,K)))
PGF <- function(Data) {
beta <- rnorm((Data$J-1)*Data$K)
return(beta)
}
MyData <- list(J=J, K=K, N=N, PGF=PGF, X=X, mon.names=mon.names,
		    parm.names=parm.names, y=y)
##################
Model <- function(parm, Data)
{
### Parameters
beta <- matrix(parm, Data$J-1, Data$K)
### Log-Prior
beta.prior <- sum(dnormv(beta, 0, 1000, log=TRUE))
### Log-Likelihood
mu <- matrix(0, Data$N, Data$J)
mu[,-Data$J] <- tcrossprod(Data$X, beta)mu <- interval(mu, -700, 700, reflect=FALSE)
phi <- exp(mu)
p <- phi / rowSums(phi)
LL <- sum(dcat(Data$y, p, log=TRUE))
### Log-Posterior
LP <- LL + beta.prior
Modelout <- list(LP=LP, Dev=-2*LL, Monitor=LP, yhat=rcat(nrow(p), p),
			parm=parm)
return(Modelout)
}
################
Initial.Values <- c(rep(0,(J-1)*K))

