# 
# data(airquality)
# Wind = airquality$Wind # wind speed
# Temp = airquality$Temp # air temperature
# N = dim(airquality)[1] # number of data points
# 
# require(R2jags)
# ?jags.parallel
# 
# 
# system.time(
#   jagsfit <- jags(data=jags.data, inits=jags.inits, jags.params,n.chains = 4,
#                   n.iter=1000000, model.file=schoolsmodel))
# 
# system.time(jagsfit.p <- jags.parallel(data=jags.data, inits=jags.inits, jags.params,n.chains = 4,
#                                        n.iter=1000000, model.file=schoolsmodel))
# #############
# 
# ###############################################################
# # 1. LINEAR REGRESSION with no covariates
# # no covariates, so intercept only. The parameters are
# # mean 'mu' and precision/variance parameter 'tau.obs'
# ###############################################################
# model.loc="lm_intercept.txt" # name of the txt file
# jagsscript = cat("
#                  model {
#                  # priors on parameters
#                  mu ~ dnorm(0, 0.01); # mean = 0, sd = 1/sqrt(0.01)
#                  tau.obs ~ dgamma(0.001,0.001); # This is inverse gamma
#                  sd.obs <- 1/sqrt(tau.obs); # sd is treated as derived parameter
#                  for(i in 1:N) {
#                  Y[i] ~ dnorm(mu, tau.obs);
#                  }
#                  }
#                  ",file=model.loc)
# 
# jags.data = list("Y"=Wind,"N"=N) # named list of inputs
# jags.params=c("sd.obs","mu") # parameters to be monitored
# mod_lm_intercept = jags(jags.data, parameters.to.save=jags.params,
#                         model.file=model.loc, n.chains = 3, n.burnin=5000,
#                         n.thin=1, n.iter=10000, DIC=TRUE)
# createMcmcList = function(jagsmodel) {
#   McmcArray = as.array(jagsmodel$BUGSoutput$sims.array)
#   McmcList = vector("list",length=dim(McmcArray)[2])
#   for(i in 1:length(McmcList)) McmcList[[i]] = as.mcmc(McmcArray[,i,])
#   McmcList = mcmc.list(McmcList)
#   return(McmcList)
# }
# myList = createMcmcList(mod_lm_intercept)
# summary(myList[[1]])
# # Run the majority of the diagnostics that CODA() offers
# library(coda)
# gelmanDiags = gelman.diag(createMcmcList(mod_lm_intercept),multivariate=F)
# autocorDiags = autocorr.diag(createMcmcList(mod_lm_intercept))
# gewekeDiags = geweke.diag(createMcmcList(mod_lm_intercept))
# heidelDiags = heidel.diag(createMcmcList(mod_lm_intercept))
# 
# 
# 
# 
# ###############################################################
# # 2. MODIFY THE ERRORS TO BE AUTOCORRELATED
# # no covariates, so intercept only.
# ###############################################################
# model.loc=("lmcor_intercept.txt")
# jagsscript = cat("
# model {
# # priors on parameters
# mu ~ dnorm(0, 0.01);
# tau.obs ~ dgamma(0.001,0.001);
# sd.obs <- 1/sqrt(tau.obs);
# phi ~ dunif(-1,1);
# tau.cor <- tau.obs * (1-phi*phi); # Var = sigma2 * (1-rho^2)
# epsilon[1] <- Y[1] - mu;
# predY[1] <- mu; # initial valu
# for(i in 2:N) {
# predY[i] <- mu + phi * epsilon[i-1];
#                  Y[i] ~ dnorm(predY[i], tau.cor);
#                  epsilon[i] <- (Y[i] - mu) - phi*epsilon[i-1];
#                  }
#                  }
#                  ",file=model.loc)
# jags.data = list("Y"=Wind,"N"=N)
# jags.params=c("sd.obs","predY","mu","phi")
# mod_lmcor_intercept = jags(jags.data, parameters.to.save=jags.params,
#                            model.file=model.loc, n.chains = 3, n.burnin=5000,
#                            n.thin=1, n.iter=10000, DIC=TRUE)
# plotModelOutput = function(jagsmodel, Y) {
#   # attach the model
#   attach.jags(jagsmodel)
#   x = seq(1,length(Y))
#   summaryPredictions = cbind(apply(predY,2,quantile,0.025), apply(predY,2,mean),
#                              apply(predY,2,quantile,0.975))
#   plot(Y, col="white",ylim=c(min(c(Y,summaryPredictions)),max(c(Y,summaryPredictions))),
#        xlab="",ylab="95% CIs of predictions and data",main=paste("JAGS results:",
#                                                                  jagsmodel$model.file))
#   polygon(c(x,rev(x)), c(summaryPredictions[,1], rev(summaryPredictions[,3])),
#           col="grey70",border=NA)
#   lines(summaryPredictions[,2])
#   points(Y)
# }
# plotModelOutput(mod_lmcor_intercept, Wind)
# 
# 
# 
# ################################################################
# # 3. AR(1) MODEL WITH NO ESTIMATED AR COEFFICIENT = RANDOM WALK
# # no covariates. The model is y[t] ~ Normal(y[n-1], sigma) for
# # we'll call the precision tau.pro
# # Note too that we have to define predY[1]
# ################################################################
# model.loc=("rw_intercept.txt")
# jagsscript = cat("
#                  model {
#                  mu ~ dnorm(0, 0.01);
#                  tau.pro ~ dgamma(0.001,0.001);
#                  sd.pro <- 1/sqrt(tau.pro);
#                  predY[1] <- mu; # initial value
#                  for(i in 2:N) {
#                  predY[i] <- Y[i-1];
#                  Y[i] ~ dnorm(predY[i], tau.pro);
#                  }
#                  }
#                  ",file=model.loc)
# jags.data = list("Y"=Wind,"N"=N)
# jags.data = list("Y"=photons,"N"=500)
# jags.params=c("sd.pro","mu","predY")
# jags.params=c("sd.pro","mu")
# mod_rw_intercept = jags(jags.data, parameters.to.save=jags.params, model.file=model.loc,
#                         n.chains = 3, n.burnin=5000, n.thin=1, n.iter=10000, DIC=TRUE)
# mod_rw_intercept
# 5*1.5
# plotModelOutput(mod_rw_intercept, photons)
# 
# #20
# 2154,221,27.925, 5.9
# 5000, 500, 50, 5
# 
# #40
# 2786, 289, 36
# 5000, 500, 50
# 
# #100
# 3994, 409
# 5000, 500
# 
# #il semble que sd.pro soit égal à Vpsf/V0 * N * CR
# ################################################################
# # 4. AR(1) MODEL WITH AND ESTIMATED AR COEFFICIENT
# # We're introducting a new AR coefficient 'phi', so the model is
# # y[t] ~ N(mu + phi*y[n-1], sigma^2)
# ################################################################
# model.loc=("ar1_intercept.txt")
# jagsscript = cat("
#                  model {
#                  mu ~ dnorm(0, 0.01);
#                  tau.pro ~ dgamma(0.001,0.001);
#                  sd.pro <- 1/sqrt(tau.pro);
#                  phi ~ dnorm(0, 1);
#                  predY[1] <- Y[1];
#                  for(i in 2:N) {
#                  predY[i] <- mu + phi * Y[i-1];
#                  Y[i] ~ dnorm(predY[i], tau.pro);
#                  }
#                  }
#                  ",file=model.loc)
# jags.data = list("Y"=Wind,"N"=N)
# jags.data = list("Y"=photons,"N"=500)
# jags.params=c("sd.pro","mu","phi","predY")
# mod_ar1_intercept = jags(jags.data, parameters.to.save=jags.params,
#                          model.file=model.loc, n.chains = 3, n.burnin=5000, n.thin=1,
#                          n.iter=10000, DIC=TRUE)
# mod_ar1_intercept
# 
# plotModelOutput(mod_ar1_intercept, photons)
# 
# 
# 
# 
# 
# ###############################################################
# # 5. MAKE THE SS MODEL a univariate random walk
# # no covariates.
# ###############################################################
# model.loc=("ss_model.txt")
# jagsscript = cat("
#                  model {
#                  # priors on parameters
#                  mu ~ dnorm(0, 0.01);
#                  tau.pro ~ dgamma(0.001,0.001);
#                  sd.q <- 1/sqrt(tau.pro);
#                  tau.obs ~ dgamma(0.001,0.001);
#                  sd.r <- 1/sqrt(tau.obs);
#                  phi ~ dnorm(0,1);
#                  X[1] <- mu;
#                  predY[1] <- X[1];
#                  Y[1] ~ dnorm(X[1], tau.obs);
#                  for(i in 2:N) {
#                  predX[i] <- phi*X[i-1];
#                  X[i] ~ dnorm(predX[i],tau.pro); # Process variation
#                  predY[i] <- X[i];
#                  Y[i] ~ dnorm(X[i], tau.obs); # Observation variation
#                  }
#                  }
#                  ",file=model.loc)
# jags.data = list("Y"=Wind,"N"=N)
# jags.data = list("Y"=flux,"N"=20000)
# jags.params=c("sd.q","sd.r","predY","mu","phi")
# jags.params=c("sd.q","sd.r","mu","phi")
# 
# mod_ss = jags(jags.data, parameters.to.save=jags.params, model.file=model.loc, n.chains = 1,
#               n.burnin=8000, n.thin=2, n.iter=10000, DIC=TRUE)
# mod_ss$BUGSoutput$mean$mu
# mod_ss$BUGSoutput$mean$sd.q #process variation
# mod_ss$BUGSoutput$mean$sd.r #obs variation
# 
# jags.data = list("Y"=c(photons,NA,NA,NA,NA,NA),"N"=503)
# plotModelOutput(mod_ss, fluxPhotons[5000:6000])
