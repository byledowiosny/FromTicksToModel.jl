#Dynamic-Sparse-Factor-Model: find errors
library(LaplacesDemon)
#========================================================
#Data
#======================================================
data(demonfx)
Y.orig <- demonfx
Y <- log(as.matrix(Y.orig[1:20,1:3]))
Y.means <- colMeans(Y)
Y <- Y - matrix(Y.means, nrow(Y), ncol(Y), byrow=TRUE) 
#Center
Y.scales <- sqrt(.colVars(Y))
Y <- Y / matrix(Y.scales, nrow(Y), ncol(Y), byrow=TRUE) 
#Scale
T <- nrow(Y) #Number of time-periods
J <- ncol(Y) #Number of time-series
P <- 2       #Number of dynamic factors
mon.names <- "LP"
U1 <- matrix(NA,P,P); U2 <- matrix(NA,P,J)
U1[upper.tri(U1, diag=TRUE)] <- 0; U2[upper.tri(U2)] <- 0
Lambda <- array(NA, dim=c(P,J,T))
U <- array(NA, dim=c(P,P,T))
for (t in 1:T) {
  U[ , , t] <- U1
  Lambda[ , , t] <- U2
}
parm.names <- as.parm.names(list(alpha0=rep(0,J), 
                                 Alpha=matrix(0,T,J),
						                     alpha.mu=rep(0,J), 
						                     alpha.phi=rep(0,J), 
						                     alpha.sigma=rep(0,J),
						                     f0=rep(0,P), 
						                     F=matrix(0,T,P), 
						                     f.phi=rep(0,P), 
						                     f.u0=U1, f.U=U,
						                     f.u.mu=U1, 
						                     f.u.phi=U1, 
						                     f.u.sigma=U1,
						                     lambda0=U2, 
						                     Lambda=Lambda,
						                     lambda.d=U2, 
						                     lambda.mu=U2, 
						                     lambda.phi=U2, 
						                     lambda.sigma=U2,
						                     lsigma0=rep(0,J), 
						                     lSigma=matrix(0,T,J),
						                     lsigma.mu=rep(0,J), 
						                     lsigma.phi=rep(0,J), 
						                     lsigma.sigma=rep(0,J)))
pos.alpha0 <- grep("alpha0", parm.names)
pos.Alpha <- grep("Alpha", parm.names)
pos.alpha.mu <- grep("alpha.mu", parm.names)
pos.alpha.phi <- grep("alpha.phi", parm.names)
pos.alpha.sigma <- grep("alpha.sigma", parm.names)
pos.f0 <- grep("f0", parm.names)
pos.F <- grep("F", parm.names)
pos.f.phi <- grep("f.phi", parm.names)
pos.f.u0 <- grep("f.u0", parm.names)
pos.f.U <- grep("f.U", parm.names)
pos.f.u.mu <- grep("f.u.mu", parm.names)
pos.f.u.phi <- grep("f.u.phi", parm.names)
pos.f.u.sigma <- grep("f.u.sigma", parm.names)
pos.lambda0 <- grep("lambda0", parm.names)
pos.Lambda <- grep("Lambda", parm.names)
pos.lambda.d <- grep("lambda.d", parm.names)
pos.lambda.mu <- grep("lambda.mu", parm.names)
pos.lambda.phi <- grep("lambda.phi", parm.names)
pos.lambda.sigma <- grep("lambda.sigma", parm.names)
pos.lsigma0 <- grep("lsigma0", parm.names)
pos.lSigma <- grep("lSigma", parm.names)
pos.lsigma.mu <- grep("lsigma.mu", parm.names)
pos.lsigma.phi <- grep("lsigma.phi", parm.names)
pos.lsigma.sigma <- grep("lsigma.sigma", parm.names)
PGF <- function(Data) {
  alpha0 <- rnorm(Data$J)
  Alpha <- rnorm(Data$T*Data$J)
  alpha.mu <- rnorm(Data$J)
  alpha.phi <- rbeta(Data$J, 20, 1.5) * 2 - 1
  alpha.sigma <- runif(Data$J)
  f0 <- rnorm(Data$P)
  F <- rnorm(Data$T*Data$P)
  f.phi <- rbeta(Data$P, 1, 1) * 2 - 1
  f.u0 <- rnorm(length(Data$pos.f.u0))
  f.U <- rnorm(length(Data$pos.f.U))
  f.u.mu <- rnorm(length(Data$pos.f.u.mu))
  f.u.phi <- runif(length(Data$pos.f.u.phi))
  f.u.sigma <- runif(length(Data$pos.f.u.sigma))
  lambda0 <- rnorm(length(Data$pos.lambda0))
  Lambda <- rnorm(length(Data$pos.Lambda))
  lambda.mu <- rnorm(length(Data$pos.lambda.mu))
  lambda.phi <- rbeta(length(Data$pos.lambda.phi), 20, 1.5)
  lambda.sigma <- runif(length(Data$pos.lambda.sigma))
  lambda.d <- runif(length(Data$pos.lambda.d), 0, 
                    abs(lambda.mu) +
					          3*sqrt(lambda.sigma/(1-lambda.phi^2)))
  lsigma0 <- rnorm(Data$J)
  lSigma <- rnorm(Data$T*Data$J)
  lsigma.mu <- rnorm(Data$J)
  lsigma.phi <- rbeta(Data$J, 20, 1.5) * 2 - 1
  lsigma.sigma <- runif(Data$J)
  return(c(alpha0, Alpha, alpha.mu, alpha.phi, 
           alpha.sigma, f0, F, f.phi, f.u0, f.U, f.u.mu, 
           f.u.phi, f.u.sigma, lambda0, Lambda, lambda.d, 
           lambda.mu, lambda.phi, lambda.sigma, lsigma0, 
           lSigma, lsigma.mu, lsigma.phi, lsigma.sigma))
}
MyData <- list(J=J, P=P, PGF=PGF, T=T, Y=Y, 
               mon.names=mon.names,
		           parm.names=parm.names, 
		           pos.alpha0=pos.alpha0, 
		           pos.Alpha=pos.Alpha,
		           pos.alpha.mu=pos.alpha.mu, 
		           pos.alpha.phi=pos.alpha.phi,
		           pos.alpha.sigma=pos.alpha.sigma, 
		           pos.f0=pos.f0, pos.F=pos.F,
		           pos.f.phi=pos.f.phi, pos.f.u0=pos.f.u0, 
		           pos.f.U=pos.f.U,
		           pos.f.u.mu=pos.f.u.mu, 
		           pos.f.u.phi=pos.f.u.phi,
		           pos.f.u.sigma=pos.f.u.sigma, 
		           pos.lambda0=pos.lambda0,
		           pos.Lambda=pos.Lambda, 
		           pos.lambda.d=pos.lambda.d,
		           pos.lambda.mu=pos.lambda.mu, 
		           pos.lambda.phi=pos.lambda.phi,
		           pos.lambda.sigma=pos.lambda.sigma, 
		           pos.lsigma0=pos.lsigma0,
		           pos.lSigma=pos.lSigma, 
		           pos.lsigma.mu=pos.lsigma.mu,
		           pos.lsigma.phi=pos.lsigma.phi, 
		           pos.lsigma.sigma=pos.lsigma.sigma)
#Model
#=======================================================
Model <- function(parm, Data)
{
  ### Parameters
  alpha0 <- parm[Data$pos.alpha0]
  Alpha <- matrix(parm[Data$pos.Alpha], Data$T, Data$J)
  alpha.mu <- parm[Data$pos.alpha.mu]
  alpha.phi <- interval(parm[Data$pos.alpha.phi], -1, 1)
  parm[Data$pos.alpha.phi] <- alpha.phi
  alpha.sigma <- interval(parm[Data$pos.alpha.sigma], 
                          1e-100, Inf)
  parm[Data$pos.alpha.sigma] <- alpha.sigma
  f0 <- parm[Data$pos.f0]
  F <- matrix(parm[Data$pos.F], Data$T, Data$P)
  f.phi <- interval(parm[Data$pos.f.phi], -1, 1)
  parm[Data$pos.f.phi] <- f.phi
  f.u0 <- parm[Data$pos.f.u0]
  f.U <- parm[Data$pos.f.U]
  f.u.mu <- parm[Data$pos.f.u.mu]
  f.u.phi <- interval(parm[Data$pos.f.u.phi], -1, 1)
  parm[Data$pos.f.u.phi] <- f.u.phi
  f.u.sigma <- interval(parm[Data$pos.f.u.sigma], 
                        1e-100, Inf)
  parm[Data$pos.f.u.sigma] <- f.u.sigma
  lambda0 <- parm[Data$pos.lambda0]
  Lambda <- parm[Data$pos.Lambda]
  lambda.mu <- parm[Data$pos.lambda.mu]
  lambda.phi <- interval(parm[Data$pos.lambda.phi], -1, 1)
  parm[Data$pos.lambda.phi] <- lambda.phi
  lambda.sigma <- interval(parm[Data$pos.lambda.sigma], 
                           1e-100, Inf)
  parm[Data$pos.lambda.sigma] <- lambda.sigma
  lambda.d <- parm[Data$pos.lambda.d]
  for (i in 1:length(lambda.d))
    lambda.d[i] <- interval(lambda.d[i], 0, 
                            abs(lambda.mu[i]) +
				                    3*sqrt(lambda.sigma[i]/
				                             (1-lambda.phi[i]^2)))
  parm[Data$pos.lambda.d] <- lambda.d
  lsigma0 <- parm[Data$pos.lsigma0]
  lSigma <- matrix(parm[Data$pos.lSigma], Data$T, Data$J)
  lsigma.mu <- parm[Data$pos.lsigma.mu]
  lsigma.phi <- interval(parm[Data$pos.lsigma.phi], -1, 1)
  parm[Data$pos.lsigma.phi] <- lsigma.phi
  lsigma.sigma <- interval(parm[Data$pos.lsigma.sigma], 
                           1e-100, Inf)
  parm[Data$pos.lsigma.sigma] <- lsigma.sigma
  ### Log-Prior
  alpha0.prior <- sum(dnorm(alpha0, 0, 1, log=TRUE))
  Alpha.prior <- sum(dnorm(Alpha,
			  matrix(alpha.mu, Data$T, Data$J, byrow=TRUE) +
			  matrix(alpha.phi, Data$T, Data$J, byrow=TRUE) *
			  (rbind(alpha0, Alpha[-Data$T,]) -
				matrix(alpha.mu, Data$T, Data$J, byrow=TRUE)),
			  matrix(alpha.sigma, Data$T, Data$J, byrow=TRUE), 
				log=TRUE))
  alpha.mu.prior <- sum(dnorm(alpha.mu, 0, 1, log=TRUE))
  alpha.phi.prior <- sum(dbeta((alpha.phi + 1) / 2, 
                               20, 1.5, log=TRUE))
  alpha.sigma.prior <- sum(dhalfcauchy(alpha.sigma, 5, 
                                       log=TRUE))
  f0.prior <- sum(dnorm(f0, 0, 1, log=TRUE))
  f.phi.prior <- sum(dbeta((f.phi + 1) / 2, 1, 1, 
                           log=TRUE))
  f.u0.prior <- sum(dnorm(f.u0, 0, 1, log=TRUE))
  f.U.prior <- sum(dnorm(matrix(f.U, nrow=Data$T, 
                                byrow=TRUE),
			matrix(f.u.mu, Data$T, Data$P*(Data$P-1)/2+Data$P, 
			       byrow=TRUE) +
			matrix(f.u.phi, Data$T, Data$P*(Data$P-1)/2+Data$P, 
			       byrow=TRUE) *
			(rbind(f.u0, matrix(f.U, nrow=Data$T, 
			                    byrow=TRUE)[-Data$T,]) -
										     matrix(f.u.mu, Data$T, 
										            Data$P*(Data$P-1)/
										              2+Data$P, byrow=TRUE)),
			matrix(f.u.sigma, Data$T, Data$P*(Data$P-1)/
			         2+Data$P, byrow=TRUE),
			log=TRUE))
  f.u.mu.prior <- sum(dnorm(f.u.mu, 0, 1, log=TRUE))
  f.u.phi.prior <- sum(dbeta((f.u.phi + 1) / 2, 20, 1.5, 
                             log=TRUE))
  f.u.sigma.prior <- sum(dhalfcauchy(f.u.sigma, 1, 
                                     log=TRUE))
  lambda0.prior <- sum(dnorm(lambda0, 0, 1, log=TRUE))
  Lambda.prior <- sum(dnorm(matrix(Lambda, nrow=Data$T, byrow=TRUE),
			            matrix(lambda.mu, Data$T, length(lambda.mu), byrow=TRUE) +
			            (rbind(lambda0, matrix(Lambda, nrow=Data$T, byrow=TRUE))
			             [-(Data$T+1),] -
			            matrix(lambda.mu, Data$T, length(lambda.mu), byrow=TRUE)),
			            matrix(lambda.sigma, Data$T, length(lambda.sigma), byrow=TRUE),
			            log=TRUE))
  lambda.d.prior <- sum(dunif(lambda.d, 0, abs(lambda.mu) +
				      3*sqrt(lambda.sigma/(1-lambda.phi^2)), log=TRUE))
  lambda.mu.prior <- sum(dnorm(lambda.mu, 0, 1, log=TRUE))
  lambda.phi.prior <- sum(dbeta((lambda.phi + 1) / 2, 20, 1.5, log=TRUE))
  lambda.sigma.prior <- sum(dhalfcauchy(lambda.sigma, 1, log=TRUE))
  lsigma0.prior <- sum(dnorm(lsigma0, 0, 1, log=TRUE))
  lSigma.prior <- sum(dnorm(lSigma,
			   matrix(lsigma.mu, Data$T, Data$J, byrow=TRUE) +
			   matrix(lsigma.phi, Data$T, Data$J, byrow=TRUE) *
			   (rbind(lsigma0, lSigma[-Data$T,]) -
				 matrix(lsigma.mu, Data$T, Data$J, byrow=TRUE)),
			   matrix(lsigma.sigma, Data$T, Data$J, byrow=TRUE), log=TRUE))
  lsigma.mu.prior <- sum(dnorm(lsigma.mu, 0, 1, log=TRUE))
  lsigma.phi.prior <- sum(dbeta((lsigma.phi + 1) / 2, 20, 1.5, log=TRUE))
  lsigma.sigma.prior <- sum(dhalfcauchy(lsigma.sigma, 1, log=TRUE))
  ### Log-Likelihood
  LL <- 0; Yhat <- Data$Y; F.prior <- 0
  for (t in 1:Data$T) {
    f.U.temp <- matrix(0, Data$P, Data$P)
    f.U.temp[upper.tri(f.U.temp, diag=TRUE)] <- matrix(f.U, nrow=Data$T,
							byrow=TRUE)[t,]
    diag(f.U.temp) <- exp(diag(f.U.temp))
    f.Sigma <- as.symmetric.matrix(t(f.U.temp) %*% f.U.temp)
    F.prior <- F.prior + dmvn(F[t,], rbind(f0, F)[t,] %*% diag(f.phi),
				 f.Sigma, log=TRUE)
    Lambda.temp <- matrix(1, Data$P, Data$J)
    Lambda.temp[lower.tri(Lambda.temp)] <- 0
    Lambda.temp[upper.tri(Lambda.temp)] <- matrix(Lambda,
					       nrow=Data$T, byrow=TRUE)[t,]*(abs(matrix(Lambda,
											 nrow=Data$T, byrow=TRUE)[t,]) > lambda.d)
    mu <- Alpha[t,] + F[t,] %*% Lambda.temp
    LL <- LL + sum(dnorm(Data$Y[t,], mu, exp(lSigma[t,]), log=TRUE))
    Yhat[t,] <- rnorm(Data$J, mu, exp(lSigma[t,])) #Fitted
  }
  ### Log-Posterior
  LP <- LL + alpha0.prior + Alpha.prior + alpha.mu.prior +
  alpha.phi.prior + alpha.sigma.prior + f0.prior + F.prior +
  f.phi.prior + f.u0.prior + f.U.prior + f.u.mu.prior +
  f.u.phi.prior + f.u.sigma.prior + lambda0.prior +
  Lambda.prior + lambda.d.prior + lambda.mu.prior +
  lambda.phi.prior + lambda.sigma.prior + lsigma0.prior +
  lSigma.prior + lsigma.mu.prior + lsigma.phi.prior +
  lsigma.sigma.prior
  Modelout <- list(LP=LP, Dev=-2*LL, Monitor=LP, yhat=Yhat, parm=parm)
  return(Modelout)
}
#Initial Values
#======================================================
Initial.Values <- c(rnorm(J), rnorm(T*J), rnorm(J), runif(J), runif(J),
			      rnorm(P), rnorm(T*P), rbeta(P,1,1)*2-1, rnorm(P*(P-1)/2+P),
			      rnorm((P*(P-1)/2+P)*T), rnorm(P*(P-1)/2+P),
			      rbeta(P*(P-1)/2+P,1,1)*2-1, runif(P*(P-1)/2+P),
			      rnorm(P*J-P-P*(P-1)/2), rnorm((P*J-P-P*(P-1)/2)*T),
			      runif(P*J-P-P*(P-1)/2,0,1e-3), rnorm(P*J-P-P*(P-1)/2),
			      rbeta(P*J-P-P*(P-1)/2,20,1.5)*2-1, runif(P*J-P-P*(P-1)/2),
			      rnorm(J), rnorm(T*J), rnorm(J), rbeta(J,20,1.5)*2-1, runif(J))
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
Fit <-LaplacesDemon(Model, Data=MyData, Initial.Values,
                    Covar=Fit$Covar, Iterations=5000, 
                    Status=2702, Thinning=5,
                    Algorithm="AFSS", 
                    Specs=list(A=500, B=NULL, 
                               m=Fit$Specs$m,
                               n=1000, 
                  w=Fit$CovarDHis[nrow(Fit$CovarDHis),]))
#=======================================================
Initial.Values <- as.initial.values(Fit)
Fit <- LaplacesDemon(Model, Data=MyData, Initial.Values,
                     Covar=Fit$Covar, Iterations=8000, 
                     Status=1250, Thinning=8,
                     Algorithm="AFSS", 
                     Specs=list(A=500, B=NULL, 
                                m=Fit$Specs$m,
                                n=1000, 
                  w=Fit$CovarDHis[nrow(Fit$CovarDHis),]))
#=======================================================
Initial.Values <- as.initial.values(Fit)
Fit <- LaplacesDemon(Model, Data=MyData, Initial.Values,
                     Covar=Fit$Covar, Iterations=30000, 
                     Status=1111, Thinning=30,
                     Algorithm="AFSS", 
                     Specs=list(A=500, B=NULL, 
                                m=Fit$Specs$m,
                                n=1000, 
                  w=Fit$CovarDHis[nrow(Fit$CovarDHis),]))
#======================================================
Initial.Values <- as.initial.values(Fit)
Fit <- LaplacesDemon(Model, Data=MyData, Initial.Values,
                     Covar=Fit$Covar, Iterations=30000, 
                     Status=1369, Thinning=30,
                     Algorithm="AFSS", 
                     Specs=list(A=500, B=NULL, 
                                m=Fit$Specs$m,
                                n=1000, 
                  w=Fit$CovarDHis[nrow(Fit$CovarDHis),]))
#======================================================
Initial.Values <- as.initial.values(Fit)
Fit <- LaplacesDemon(Model, Data=MyData, Initial.Values,
                     Covar=Fit$Covar, Iterations=30000, 
                     Status=1176, Thinning=30,
                     Algorithm="AFSS", 
                     Specs=list(A=500, B=NULL, 
                                m=Fit$Specs$m,
                                n=1000, 
                  w=Fit$CovarDHis[nrow(Fit$CovarDHis),]))
#======================================================
Initial.Values <- as.initial.values(Fit)
Fit <- LaplacesDemon(Model, Data=MyData, Initial.Values,
                     Covar=Fit$Covar, Iterations=30000, 
                     Status=684, Thinning=30,
                     Algorithm="AFSS", 
                     Specs=list(A=500, B=NULL, 
                                m=Fit$Specs$m,
                                n=1000, 
                  w=Fit$CovarDHis[nrow(Fit$CovarDHis),]))
#======================================================
Initial.Values <- as.initial.values(Fit)
Fit <- LaplacesDemon(Model, Data=MyData, Initial.Values,
                     Covar=Fit$Covar, Iterations=30000, 
                     Status=520, Thinning=30,
                     Algorithm="AFSS", 
                     Specs=list(A=500, B=NULL, 
                                m=Fit$Specs$m,
                                n=1000, 
                  w=Fit$CovarDHis[nrow(Fit$CovarDHis),]))
#======================================================
Initial.Values <- as.initial.values(Fit)
Fit <- LaplacesDemon(Model, Data=MyData, Initial.Values,
                     Covar=Fit$Covar, Iterations=120000, 
                     Status=789, Thinning=120,
                     Algorithm="AFSS", 
                     Specs=list(A=500, B=NULL, 
                                m=Fit$Specs$m,
                                n=31000, 
                  w=Fit$CovarDHis[nrow(Fit$CovarDHis),]))
#=======================================================
#Laplace's Demon has been appeased, and suggests
#the marginal posterior samples should be plotted
#and subjected to any other MCMC diagnostic deemed
#fit before using these samples for inference.
#Concordance:  0.9846154 
#=======================================================
Initial.Values <- as.initial.values(Fit)
Fit <- LaplacesDemon(Model, Data=MyData, Initial.Values,
                     Covar=Fit$Covar, Iterations=30000, 
                     Status=1639, Thinning=30,
                     Algorithm="AFSS", 
                     Specs=list(A=500, B=NULL, 
                                m=Fit$Specs$m,
                                n=1000, 
                  w=Fit$CovarDHis[nrow(Fit$CovarDHis),]))
#=======================================================
Initial.Values <- as.initial.values(Fit)
Fit <- LaplacesDemon(Model, Data=MyData, Initial.Values,
                     Covar=Fit$Covar, Iterations=30000, 
                     Status=1492, Thinning=30,
                     Algorithm="AFSS", 
                     Specs=list(A=500, B=NULL, 
                                m=Fit$Specs$m,
                                n=1000, 
                  w=Fit$CovarDHis[nrow(Fit$CovarDHis),]))
#=======================================================
#errors