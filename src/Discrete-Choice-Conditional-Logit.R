library(LaplacesDemon)
####################################################
data(action)
str(action)
y <- as.numeric(action[,1])
X <- cbind(1, as.matrix(action[,2:3]))
Z <- as.matrix(action[,4:9])
for (j in 2:ncol(X)) X[,j] <- CenterScale(X[,j])
for (j in 1:ncol(Z)) Z[,j] <- CenterScale(Z[,j])
N <- length(y) #Number of records
J <- length(unique(y)) #Number of categories in y
K <- ncol(X) #Number of individual attributes 
             #(including the intercept)
C <- ncol(Z) #Number of choice-based attributes 
             #(intercept is not included)
mon.names <- "LP"
parm.names <- as.parm.names(list(beta=matrix(0, J-1, K), 
                                 gamma=rep(0, C)))
pos.beta <- grep("beta", parm.names)
pos.gamma <- grep("gamma", parm.names)
PGF <- function(Data) {
  beta <- rnorm((Data$J-1) * Data$K)
  gamma <- rnorm(Data$C)
  return(c(beta, gamma))
}
MyData <- list(C=C, J=J, K=K, N=N, PGF=PGF, X=X, Z=Z, 
               mon.names=mon.names,
		           parm.names=parm.names, 
		           pos.beta=pos.beta, 
		           pos.gamma=pos.gamma, 
		           y=y)
####################################################
Model <- function(parm, Data)
{
  ### Parameters
  beta <- matrix(parm[Data$pos.beta], Data$J-1, Data$K)
  gamma <- parm[Data$pos.gamma]
  ### Log-Prior
  beta.prior <- sum(dnormv(beta, 0, 1000, log=TRUE))
  gamma.prior <- sum(dnormv(gamma, 0, 1000, log=TRUE))
  ### Log-Likelihood
  mu <- matrix(tcrossprod(gamma, Data$Z), Data$N, Data$J)
  mu[,-Data$J] <- mu[,-Data$J] + tcrossprod(Data$X, beta)
  mu <- interval(mu, -700, 700, reflect=FALSE)
  phi <- exp(mu)
  p <- phi / rowSums(phi)
  LL <- sum(dcat(Data$y, p, log=TRUE))
  ### Log-Posterior
  LP <- LL + beta.prior + gamma.prior
  Modelout <- list(LP=LP, Dev=-2*LL, Monitor=LP, 
                   yhat=rcat(nrow(p), p), parm=parm)
  return(Modelout)
}
##########################
Initial.Values <- c(rep(0,(J-1)*K), rep(0,C))
##########################
set.seed(666)
Fit <- LaplacesDemon(Model, Data=MyData, Initial.Values,
                     Covar=NULL, Iterations=1000, 
                     Status=100, Thinning=1,
                     Algorithm="AFSS", 
                     Specs=list(A=500, B=NULL, m=100, 
                                n=0, w=1))
Concordance:  0.7748344
##########################
Consort(Fit)
Pred <- predict(Fit, Model, MyData, CPUs=1)
summary(Pred, Discrep="Chi-Square")

str(Fit)
str(Pred)
Fit$Acceptance.Rate

print(Fit)
print(Pred$y)
print(y)
Fit$Posterior2
plot(Fit, BurnIn=500, MyData, PDF=FALSE, Parms=NULL)
plot(BMK.Diagnostic(Fit$Posterior1[501:1000,]))
caterpillar.plot(Fit, Parms="beta")
plot(Pred, Style="Density", Rows=1:9)
plot(Pred$y, Style="Density", Rows=1:9)
plot(Pred$yhat, Style="Density", Rows=1:9)
plot(Pred, Style="Fitted")
plot(Pred$y, Style="Fitted")
plot(Pred$yhat, Style="Fitted")

