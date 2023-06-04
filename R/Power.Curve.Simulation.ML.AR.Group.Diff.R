###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################

# Simulation-based power analysis

## This function uses Monte Carlo simulations for computing standard errors and statistical power. 

Power.Simulation.ML.7 = function(data,T.obs,Ylag.center,
                                 b00,b01.Z,b10,b11.Z,
                                 sigma,
                                 sigma.v0,sigma.v1,rho.v,
                                 alpha,
                                 side.test,
                                 Opt.Method){
  
  # If Ylag.center is TRUE Mean centered lag varying variable per-individual
  if (Ylag.center==TRUE){  
    N.subject = unique(data$subjno)
    for (i in N.subject){
      data$Ylag[which(data$subjno==i)] = data$Ylag[which(data$subjno==i)] - mean(data$Y[which(data$subjno==i)])
    }}
  
  # Fit linear mixed-effects models
  
  if (Opt.Method == 'ML'){
    # Maximum Likelihood
    fit.lme = try(lme(Y ~ Z + Ylag + Z*Ylag, random = ~ 1 + Ylag|subjno,data=data,na.action=na.omit,method='ML',
                      control=lmeControl(opt='optim')),silent = FALSE)
  }
  
  if (Opt.Method == 'REML'){
    fit.lme = try(lme(Y ~ Z + Ylag + Z*Ylag, random = ~ 1 + Ylag|subjno,data=data,na.action=na.omit,method='REML',
                      control=lmeControl(opt='optim')),silent = FALSE)
  }
  
  if (length(fit.lme)>1){
    
    # Obtain the estimated coefficients of the model
    beta.hat.lme = coef(summary(fit.lme))[,'Value']
    
    # Obtain the estimated variance components of the model
    sigma.hat = as.numeric(VarCorr(fit.lme)[3,2])
    sigma.v0.hat = as.numeric(VarCorr(fit.lme)[1,2])
    sigma.v1.hat = as.numeric(VarCorr(fit.lme)[2,2])
    rho.v.hat = as.numeric(VarCorr(fit.lme)[2,3])

    var.hat.lme = c(sigma.hat=sigma.hat, sigma.v0.hat=sigma.v0.hat, sigma.v1.hat=sigma.v1.hat, rho.v.hat=rho.v.hat)
    
    # Obtain the standard errors
    StdError.beta.lme = coef(summary(fit.lme))[,'Std.Error']
    
    # Compute power and standard error from lme  
    
    if (side.test == 1){ # One-side test: positive
      p.value = pt(coef(summary(fit.lme))[,4], coef(summary(fit.lme))[,3], lower = FALSE)
    }
    
    if (side.test == 2){ # One-side test: negative
      p.value = pt(coef(summary(fit.lme))[,4], coef(summary(fit.lme))[,3], lower = TRUE)
      
    }
    
    if (side.test == 3){ # Two-tailed test
      p.value = 2*pt(-abs(coef(summary(fit.lme))[,4]), coef(summary(fit.lme))[,3])
    }
    
    power.hat.lme = p.value < alpha
    
    return(list(beta.hat.lme=beta.hat.lme,
                var.hat.lme=var.hat.lme,
                power.hat.lme=power.hat.lme,
                StdError.beta.lme=StdError.beta.lme,
                p.value=p.value))}
  
  if (length(fit.lme)==1){
    return(list(fit.lme))
  }}


# Function to conduct Monte Carlo simulation for conducting power analysis

## This function conduct Monte Carlo simulation for conducting power analysis using analytic derivation and estimation-based methods.

Power.Curve.Simulation.ML.AR.Group.Diff = function(N.0,N.1,T.obs,Ylag.center,
                                      b00,b01.Z,b10,b11.Z,
                                      sigma,
                                      sigma.v0,sigma.v1,rho.v,
                                      alpha,
                                      side.test,
                                      Opt.Method,R){
  
  tic()
  # Simulate data from the linear mixed-effects model  
  data = lapply(1:R, function(r) 
    Sim.AR.Model.Group.Diff(N.0,N.1,T.obs,
                      Ylag.center,
                      b00, b01.Z, b10, b11.Z, 
                      sigma, 
                      sigma.v0, sigma.v1, rho.v)) 
  
  # Simulation-based power analysis using Monte Carlo simulation
  fit.list.sim = lapply(1:R, function(r) Power.Simulation.ML.7(data[[r]],T.obs,Ylag.center,
                                                               b00,b01.Z,b10,b11.Z,
                                                               sigma,
                                                               sigma.v0,sigma.v1,rho.v,
                                                               alpha,
                                                               side.test,
                                                               Opt.Method))
  toc(log = TRUE, quiet = TRUE)
  log.txt =  tic.log(format = TRUE)
  log.lst = tic.log(format = FALSE)
  tic.clearlog()
  timings.sim.power.simulation = unlist(lapply(log.lst, function(x) x$toc - x$tic))
  
  # Get a vector with the iterations that converge
  errors = rep(0,R)
  for (r in 1:R){errors[r] = length(fit.list.sim[[r]])}
  
  R.converge = which(errors>1)
  
  # Number of replicates that converge
  n.R = length(R.converge)
  
  # Estimates the fixed effects
  beta.hat.lme.list = matrix(unlist(lapply(R.converge, function(r) fit.list.sim[[r]]$beta.hat.lme)), 
                             ncol=4, byrow=TRUE)
  colnames(beta.hat.lme.list) = c('b00','b01.Z','b10','b11.Z')
  
  beta.hat.lme = colMeans(beta.hat.lme.list)
  beta.hat.lme.se = apply(beta.hat.lme.list,2,sd)/sqrt(n.R)
  
  # Estimates the variance components
  var.hat.lme.list = matrix(unlist(lapply(R.converge, function(r) fit.list.sim[[r]]$var.hat.lme)), 
                            ncol=4, byrow=TRUE)
  colnames(var.hat.lme.list) = c('sigma.hat','sigma.v0.hat','sigma.v1.hat','rho.v.hat')
  
  var.hat.lme = colMeans(var.hat.lme.list)
  var.hat.lme.se = apply(var.hat.lme.list,2,sd)/sqrt(n.R)
  
  # Power
  power.hat.lme.list = matrix(unlist(lapply(R.converge, function(r) 
    fit.list.sim[[r]]$power.hat.lme)), 
    ncol=4, byrow=TRUE)
  colnames(power.hat.lme.list) = c('b00','b01.Z','b10','b11.Z')
  
  power.hat.lme = colMeans(power.hat.lme.list)
  power.hat.lme.se = sqrt(power.hat.lme*(1-power.hat.lme))/sqrt(n.R)
  
  # Standard errors
  StdError.beta.hat.lme.list = matrix(unlist(lapply(R.converge, function(r) 
    fit.list.sim[[r]]$StdError.beta.lme)), 
    ncol=4, byrow=TRUE)
  colnames(StdError.beta.hat.lme.list) = c('b00','b01.Z','b10','b11.Z')
  
  StdError.beta.hat.lme = colMeans(StdError.beta.hat.lme.list)
  StdError.beta.hat.lme.se = apply(StdError.beta.hat.lme.list,2,sd)/sqrt(n.R)

  # P-value
  p.value.beta.hat.lme.list = matrix(unlist(lapply(R.converge, function(r) 
    fit.list.sim[[r]]$p.value)), 
    ncol=4, byrow=TRUE)
  colnames(StdError.beta.hat.lme.list) = c('b00','b01.Z','b10','b11.Z')
  
  return(list(beta.hat.lme.list=beta.hat.lme.list,
              beta.hat.lme=beta.hat.lme,
              beta.hat.lme.se=beta.hat.lme.se,
              var.hat.lme.list=var.hat.lme.list,
              var.hat.lme=var.hat.lme,
              var.hat.lme.se=var.hat.lme.se,
              power.hat.lme.list=power.hat.lme.list,
              power.hat.lme=power.hat.lme,
              power.hat.lme.se=power.hat.lme.se,
              StdError.beta.hat.lme=StdError.beta.hat.lme,
              StdError.beta.hat.lme.se=StdError.beta.hat.lme.se,
              p.value.beta.hat.lme.list=p.value.beta.hat.lme.list,
              timings.sim.power.simulation=timings.sim.power.simulation,
              n.R=n.R))}
