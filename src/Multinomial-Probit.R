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
S <- diag(J-1)
U <- matrix(NA,J-1,J-1)
U[upper.tri(U, diag=TRUE)] <- 0
mon.names <- "LP"
parm.names <- as.parm.names(list(beta=matrix(0,(J-1),K),U=U, W=matrix(0,N,J-1)))
parm.names <- parm.names[-which(parm.names == "U[1,1]")]
pos.beta <- grep("beta", parm.names)
pos.U <- grep("U", parm.names)
pos.W <- grep("W", parm.names)
PGF <- function(Data) {
beta <- rnorm((Data$J-1)*Data$K)
U <- rnorm((Data$J-2) + (factorial(Data$J-1) /
					     (factorial(Data$J-1-2)*factorial(2))))
W <- matrix(runif(Data$N*(Data$J-1),-10,0), Data$N, Data$J-1)
Y <- as.indicator.matrix(Data$y)
W <- ifelse(Y[,-Data$J] == 1, abs(W), W)
return(c(beta, U, as.vector(W)))}
MyData <- list(J=J, K=K, N=N, PGF=PGF, S=S, X=X, mon.names=mon.names,
		    parm.names=parm.names, pos.beta=pos.beta, pos.U=pos.U, pos.W=pos.W,
		    y=y)
#####################
Model <- function(parm, Data)
{
### Parameters
beta <- matrix(parm[Data$pos.beta], Data$J-1, Data$K)
u <- c(0, parm[Data$pos.U])
U <- diag(Data$J-1)
U[upper.tri(U, diag=TRUE)] <- u
diag(U) <- exp(diag(U))
Sigma <- t(U) %*% U
Sigma[1,] <- Sigma[,1] <- U[1,]
W <- matrix(parm[Data$pos.W], Data$N, Data$J-1)
Y <- as.indicator.matrix(Data$y)
temp <- which(Y[,-c(Data$J)] == 1)
W[temp] <- interval(W[temp], 0, 10)
temp <- which(Y[,-c(Data$J)] == 0)
W[temp] <- interval(W[temp], -10, 0)
parm[Data$pos.W] <- as.vector(W)
### Log-Prior
beta.prior <- sum(dnormv(beta, 0, 10, log=TRUE))
U.prior <- sum(dnorm(u[-1], 0, 1, log=TRUE))
### Log-Likelihood
mu <- tcrossprod(Data$X, beta)
#eta <- exp(cbind(mu,0))
#p <- eta / rowSums(eta)
LL <- sum(dmvn(W, mu, Sigma, log=TRUE))
### Log-Posterior
LP <- LL + beta.prior + U.priorModelout <- list(LP=LP, Dev=-2*LL, Monitor=LP,
						       yhat=max.col(cbind(rmvn(nrow(mu), mu, Sigma),0)), parm=parm)
return(Modelout)
}
######################
Initial.Values <- GIV(Model, MyData, PGF=TRUE)

