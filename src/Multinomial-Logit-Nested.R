library(LaplacesDemon)
####################################################
data(action)
str(action)
y <- as.numeric(action[,1])
X <- cbind(1, as.matrix(action[,2:3]))
for (j in 2:ncol(X)) X[,j] <- CenterScale(X[,j])
N <- length(y)
J <- length(unique(y)) #Number of categories in y
K <- ncol(X) #Number of predictors (including the intercept)
mon.names <- c("LP", as.parm.names(list(iota=rep(0,K))))
parm.names <- as.parm.names(list(alpha=0, beta=matrix(0,J-1,K)))
pos.alpha <- grep("alpha", parm.names)
pos.beta <- grep("beta", parm.names)
PGF <- function(Data) {
alpha <- rtrunc(1, "exp", a=0, b=2, rate=1)
beta <- rnorm((Data$J-1)*Data$K)
return(c(alpha, beta))
}
MyData <- list(J=J, K=K, N=N, PGF=PGF, X=X, mon.names=mon.names,
		    parm.names=parm.names, pos.alpha=pos.alpha, pos.beta=pos.beta, y=y)
################
Model <- function(parm, Data)
{
### Hyperparameters
alpha.rate <- 1
### Parameters
parm[Data$pos.alpha] <- alpha <- interval(parm[Data$pos.alpha],0,2)
beta <- matrix(parm[Data$pos.beta], Data$J-1, Data$K)
### Log-Prior
alpha.prior <- dtrunc(alpha, "exp", a=0, b=2, rate=alpha.rate,
			     log=TRUE)
beta.prior <- sum(dnormv(beta, 0, 1000, log=TRUE))
### Log-Likelihood
mu <- P <- matrix(0, Data$N, Data$J)
iota <- alpha * beta[1,]
mu[,1] <- tcrossprod(Data$X, t(iota))
mu[,2] <- tcrossprod(Data$X, t(beta[2,]))
mu <- interval(mu, -700, 700, reflect=FALSE)
R <- exp(mu[,1])
S <- exp(mu[,-1])
V <- rowSums(S)
I <- log(V)P[,1] <- R / (R + exp(alpha*I))
P[,2] <- (1 - P[,1]) * S[,1] / V
P[,3] <- (1 - P[,1]) * S[,2] / V
LL <- sum(dcat(Data$y, P, log=TRUE))
### Log-Posterior
LP <- LL + alpha.prior + beta.prior
Modelout <- list(LP=LP, Dev=-2*LL, Monitor=c(LP,iota),
			yhat=rcat(nrow(P), P), parm=parm)
return(Modelout)
}
###################"
Initial.Values <- c(0.5, rep(0.1,(J-1)*K))

