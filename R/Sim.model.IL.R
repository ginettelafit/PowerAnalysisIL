###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################

# Function to compute power for IL studies as a function of the number of participants

Sim.model.IL = function(Model,N,N.0,N.1,T, 
b00,b01.Z,b01.W,b10,b11.Z,b11.W, 
sigma,rho,sigma.v0,sigma.v1,rho.v, 
mu.W,sigma.W,mu.X,mu.X0,mu.X1,sigma.X, 
sigma.X0,sigma.X1,
is.rho.zero,isW.center,isX.center,Ylag.center, 
alpha,R,Opt.Method){

###############################################################
###############################################################
###############################################################

message('Initializing...')

# Check parameters & and output messages

if (length(alpha) == 0) {stop('The Type I error must be between 0 and 1')}
if (alpha > 1) {stop('The Type I error must be between 0 and 1')}
if (alpha == 0) {stop('The Type I error must be between 0 and 1')}
if (length(T) == 0) {stop('Set the number of time points')}
if (length(sigma) == 0) {stop('Set the value of the standard error of the level-1 residual')}
if (sigma <= 0) {stop('The variance of the level-1 standard error must be positive')}

if (Model==1){
N.1 = as.numeric(unlist(strsplit(N.1,",")))
N.0 = as.numeric(unlist(strsplit(N.0,",")))
if (length(N.0) == 1) {stop('The length of the vector with the number of participants in Group 0 must be larger than one')}
if (length(N.1) == 1) {stop('The length of the vector with the number of participants in Group 1 must be larger than one')}
if (abs(length(N.0) - length(N.1)) > 0) {stop('The vector with the number of participants in Group 0 must have the same length to the vector with the number of participants in Group 1')}
if (length(rho) == 0) {stop('Set the value of the autocorrelation of the level-1 residuals')}
if (abs(rho)>1) {stop('The absolute value of the autocorrelation of the level-1 residuals must be included in the interval [-1,1]')}
if (length(N.0) == 0) {stop('The number of participants in Group 0 must be positive')}
if (length(N.1) == 0) {stop('The number of participants in Group 1 must be positive')}
if (length(b00) == 0) {stop('Set the value of the fixed intercept')}
if (length(b01.Z) == 0) {stop('Set the value of the effect of the level-2 dummy variable on the intercept')}
if (length(sigma.v0) == 0) {stop('Set the value of the standard deviation of the random intercept')}
if (sigma.v0 < 0) {stop('The standard deviation of the random intercept must be positive')}
}

if (Model==2){
N = as.numeric(unlist(strsplit(N,",")))
if (length(N) == 1) {stop('The length of the vector with the number of participants must be larger than one')}
if (length(rho) == 0) {stop('Set the value of the autocorrelation of the level-1 residuals')}
if (abs(rho)>1) {stop('The absolute value of the autocorrelation of the level-1 residuals must be included in the interval [-1,1]')}
if (length(N) == 0) {stop('The number of participants must be positive')}
if (length(b00) == 0) {stop('Set the value of the fixed intercept')}
if (length(b01.W) == 0) {stop('Set the value of the effect of the level-2 continuous variable on the intercept')}
if (length(sigma.v0) == 0) {stop('Set the value of the standard deviation of the random intercept')}
if (sigma.v0 < 0) {stop('The standard deviation of the random intercept must be positive')}  
}

if (Model==3){
N = as.numeric(unlist(strsplit(N,",")))
if (length(N) == 1) {stop('The length of the vector with the number of participants must be larger than one')}
if (length(rho) == 0) {stop('Set the value of the autocorrelation of the level-1 residuals')}
if (abs(rho)>1) {stop('The absolute value of the autocorrelation of the level-1 residuals must be included in the interval [-1,1]')}
if (length(N) == 0) {stop('The number of participants must be positive')}
if (length(b00) == 0) {stop('Set the value of the fixed intercept')}
if (length(b10) == 0) {stop('Set the value of the fixed slope')}
if (length(sigma.v0) == 0) {stop('Set the value of the standard deviation of the random intercept')}
if (length(sigma.v1) == 0) {stop('Set the value of the standard deviation of the random slope')}
if (length(rho.v) == 0) {stop('Set the value of the correlation between the random intercept and the random intercept')}
if (length(mu.X) == 0) {stop('Set the value of the mean of the level-1 predictor')}
if (length(sigma.X) == 0) {stop('Set the value of the standard deviation of the level-1 predictor')}
if (sigma.v0 < 0) {stop('The standard deviation of the random intercept must be positive')}
if (sigma.v1 < 0) {stop('The standard deviation of the random slope must be positive')}
  
}

if (Model==4){
N = as.numeric(unlist(strsplit(N,",")))
if (length(N) == 1) {stop('The length of the vector with the number of participants must be larger than one')}
if (length(rho) == 0) {stop('Set the value of the autocorrelation of the level-1 residuals')}
if (abs(rho)>1) {stop('The absolute value of the autocorrelation of the level-1 residuals must be included in the interval [-1,1]')}
if (length(N) == 0) {stop('The number of participants must be positive')}
if (length(b00) == 0) {stop('Set the value of the fixed intercept')}
if (length(b10) == 0) {stop('Set the value of the fixed slope')}
if (length(sigma.v0) == 0) {stop('Set the value of the standard deviation of the random intercept')}
if (length(mu.X) == 0) {stop('Set the value of the mean of the level-1 predictor')}
if (length(sigma.X) == 0) {stop('Set the value of the standard deviation of the level-1 predictor')}
if (sigma.v0 < 0) {stop('The standard deviation of the random intercept must be positive')}
}

if (Model==5){
N.1 = as.numeric(unlist(strsplit(N.1,",")))
N.0 = as.numeric(unlist(strsplit(N.0,",")))
if (length(N.0) == 1) {stop('The length of the vector with the number of participants in Group 0 must be larger than one')}
if (length(N.1) == 1) {stop('The length of the vector with the number of participants in Group 1 must be larger than one')}
if (abs(length(N.0) - length(N.1)) > 0) {stop('The vector with the number of participants in Group 0 must have the same length to the vector with the number of participants in Group 1')}
if (length(rho) == 0) {stop('Set the value of the autocorrelation of the level-1 residuals')}
if (abs(rho)>1) {stop('The absolute value of the autocorrelation of the level-1 residuals must be included in the interval [-1,1]')}
if (length(N.0) == 0) {stop('The number of participants in Group 0 must be positive')}
if (length(N.1) == 0) {stop('The number of participants in Group 1 must be positive')}
if (length(b00) == 0) {stop('Set the value of the fixed intercept')}
if (length(b01.Z) == 0) {stop('Set the value of the effect of the level-2 dummy variable on the intercept')}
if (length(b10) == 0) {stop('Set the value of the fixed slope')}
if (length(b11.Z) == 0) {stop('Set the value of the effect of the level-2 dummy variable on the level-1 slope')}
if (length(sigma.v0) == 0) {stop('Set the value of the standard deviation of the random intercept')}
if (length(sigma.v1) == 0) {stop('Set the value of the standard deviation of the random slope')}
if (length(rho.v) == 0) {stop('Set the value of the correlation between the random intercept and the random intercept')}
if (length(mu.X0) == 0) {stop('Set the value of the mean of the time-varying predictor in Group 0')}
if (length(sigma.X0) == 0) {stop('Set the value of the standard deviation of the time-varying predictor in Group 0')}
if (length(mu.X1) == 0) {stop('Set the value of the mean of the level-1 predictor in Group 1')}
if (length(sigma.X1) == 0) {stop('Set the value of the standard deviation of the level-1 predictor in Group 1')}
if (sigma.v0 < 0) {stop('The standard deviation of the random intercept must be positive')}
if (sigma.v1 < 0) {stop('The standard deviation of the random slope must be positive')}  
}

if (Model==6){
N.1 = as.numeric(unlist(strsplit(N.1,",")))
N.0 = as.numeric(unlist(strsplit(N.0,",")))
if (length(N.0) == 1) {stop('The length of the vector with the number of participants in Group 0 must be larger than one')}
if (length(N.1) == 1) {stop('The length of the vector with the number of participants in Group 1 must be larger than one')}
if (abs(length(N.0) - length(N.1)) > 0) {stop('The vector with the number of participants in Group 0 must have the same length to the vector with the number of participants in Group 1')}
if (length(rho) == 0) {stop('Set the value of the autocorrelation of the level-1 residuals')}
if (abs(rho)>1) {stop('The absolute value of the autocorrelation of the level-1 residuals must be included in the interval [-1,1]')}
if (length(N.0) == 0) {stop('The number of participants in Group 0 must be positive')}
if (length(N.1) == 0) {stop('The number of participants in Group 1 must be positive')}
if (length(b00) == 0) {stop('Set the value of the fixed intercept')}
if (length(b01.Z) == 0) {stop('Set the value of the effect of the level-2 dummy variable on the intercept')}
if (length(b10) == 0) {stop('Set the value of the fixed slope')}
if (length(b11.Z) == 0) {stop('Set the value of the effect of the level-2 dummy variable on the slope')}
if (length(sigma.v0) == 0) {stop('Set the value of the standard deviation of the random intercept')}
if (length(mu.X0) == 0) {stop('Set the value of the mean of the level-1 predictor in Group 0')}
if (length(sigma.X0) == 0) {stop('Set the value of the standard deviation of the level-1 predictor in Group 0')}
if (length(mu.X1) == 0) {stop('Set the value of the mean of the level-1 predictor in Group 1')}
if (length(sigma.X1) == 0) {stop('Set the value of the standard deviation of the level-1 predictor in Group 1')}
if (sigma.v0 < 0) {stop('The standard deviation of the random intercept must be positive')}
}

if (Model==7){
N = as.numeric(unlist(strsplit(N,",")))
if (length(N) == 1) {stop('The length of the vector with the number of participants must be larger than one')}
if (length(rho) == 0) {stop('Set the value of the autocorrelation of the level-1 residuals')}
if (abs(rho)>1) {stop('The absolute value of the autocorrelation of the level-1 residuals must be included in the interval [-1,1]')}
if (length(N) == 0) {stop('The number of participants must be positive')}
if (length(b00) == 0) {stop('Set the value of the fixed intercept')}
if (length(b01.W) == 0) {stop('Set the value of the effect of the level-2 continuous variable on the intercept')}
if (length(b10) == 0) {stop('Set the value of fixed slope')}
if (length(b11.W) == 0) {stop('Set the value of the effect of the level-2 continuous variable on the slope')}
if (length(sigma.v0) == 0) {stop('Set the value of the standard deviation of the random intercept')}
if (length(sigma.v1) == 0) {stop('Set the value of the standard deviation of the random slope')}
if (length(rho.v) == 0) {stop('Set the value of the correlation between the random intercept and the random intercept')}
if (length(mu.X) == 0) {stop('Set the value of the mean of the level-1 predictor')}
if (length(sigma.X) == 0) {stop('Set the value of the standard deviation of the level-1 predictor')}
if (length(mu.W) == 0) {stop('Set the value of the mean of the level-2 continuous predictor')}
if (length(sigma.W) == 0) {stop('Set the value of the standard deviation of the level-2 continuous predictor')}
if (sigma.v0 < 0) {stop('The standard deviation of the random intercept must be positive')}
if (sigma.v1 < 0) {stop('The standard deviation of the random slope must be positive')}    
}

if (Model==8){
N = as.numeric(unlist(strsplit(N,",")))
if (length(N) == 1) {stop('The length of the vector with the number of participants must be larger than one')}
if (length(rho) == 0) {stop('Set the value of the autocorrelation of the level-1 residuals')}
if (abs(rho)>1) {stop('The absolute value of the autocorrelation of the level-1 residuals must be included in the interval [-1,1]')}
if (length(N) == 0) {stop('The number of participants must be positive')}
if (length(b00) == 0) {stop('Set the value of the fixed intercept')}
if (length(b01.W) == 0) {stop('Set the value of the effect of the level-2 continuous variable on the intercept')}
if (length(b10) == 0) {stop('Set the value of fixed slope')}
if (length(b11.W) == 0) {stop('Set the value of the effect of the level-2 continuous variable on the slope')}
if (length(sigma.v0) == 0) {stop('Set the value of the standard deviation of the random intercept')}
if (length(mu.X) == 0) {stop('Set the value of the mean of the level-1 predictor')}
if (length(sigma.X) == 0) {stop('Set the value of the standard deviation of the level-1 predictor')}
if (length(mu.W) == 0) {stop('Set the value of the mean of the level-2 continuous predictor')}
if (length(sigma.W) == 0) {stop('Set the value of the standard deviation of the level-2 continuous predictor')}
if (sigma.v0 < 0) {stop('The standard deviation of the random intercept must be positive')} 
}

if (Model==9){
N = as.numeric(unlist(strsplit(N,",")))
if (length(N) == 1) {stop('The length of the vector with the number of participants must be larger than one')}
if (abs(b10) > 1){stop('The absolute value of the autoregressive effect must be included in the interval [-1,1]')}
if (length(N) == 0) {stop('The number of participants must be positive')}
if (length(b00) == 0) {stop('Set the value of the fixed intercept')}
if (length(b10) == 0) {stop('Set the value of fixed slope')}
if (length(sigma.v0) == 0) {stop('Set the value of the standard deviation of the random intercept')}
if (length(sigma.v1) == 0) {stop('Set the value of the standard deviation of the random slope')}
if (length(rho.v) == 0) {stop('Set the value of the correlation between the random intercept and the random intercept')}
if (sigma.v0 < 0) {stop('The standard deviation of the random intercept must be positive')}
if (sigma.v1 < 0) {stop('The standard deviation of the random slope must be positive')}  
}

if (Model==10){
N.1 = as.numeric(unlist(strsplit(N.1,",")))
N.0 = as.numeric(unlist(strsplit(N.0,",")))
if (length(N.0) == 1) {stop('The length of the vector with the number of participants in Group 0 must be larger than one')}
if (length(N.1) == 1) {stop('The length of the vector with the number of participants in Group 1 must be larger than one')}
if (abs(length(N.0) - length(N.1)) > 0) {stop('The vector with the number of participants in Group 0 must have the same length to the vector with the number of participants in Group 1')}
if (length(N.0) == 0) {stop('The number of participants in Group 0 must be positive')}
if (length(N.1) == 0) {stop('The number of participants in Group 1 must be positive')}
if (length(b00) == 0) {stop('Set the value of the fixed intercept')}
if (length(b01.Z) == 0) {stop('Set the value of the effect of the level-2 dummy variable on the intercept')}
if (length(b10) == 0) {stop('Set the value of the fixed slope')}
if (length(b11.Z) == 0) {stop('Set the value of the effect of the level-2 dummy variable on the slope')}
if (length(sigma.v0) == 0) {stop('Set the value of the standard deviation of the random intercept')}
if (length(sigma.v1) == 0) {stop('Set the value of the standard deviation of the random slope')}
if (length(rho.v) == 0) {stop('Set the value of the correlation between the random intercept and the random intercept')}
if (sigma.v0 < 0) {stop('The standard deviation of the random intercept must be positive')}
if (sigma.v1 < 0) {stop('The standard deviation of the random slope must be positive')}  
}

if (Model==11){
N = as.numeric(unlist(strsplit(N,",")))
if (length(N) == 1) {stop('The length of the vector with the number of participants must be larger than one')}
if (length(N) == 0) {stop('The number of participants must be positive')}
if (length(b00) == 0) {stop('Set the value of the fixed intercept')}
if (length(b01.W) == 0) {stop('Set the value of the effect of the level-2 continuous variable on the intercept')}
if (length(b10) == 0) {stop('Set the value of fixed slope')}
if (length(b11.W) == 0) {stop('Set the value of the effect of the level-2 continuous variable on the slope')}
if (length(sigma.v0) == 0) {stop('Set the value of the standard deviation of the random intercept')}
if (length(sigma.v1) == 0) {stop('Set the value of the standard deviation of the random slope')}
if (length(rho.v) == 0) {stop('Set the value of the correlation between the random intercept and the random intercept')}
if (length(mu.W) == 0) {stop('Set the value of the mean of the level-2 continuous predictor')}
if (length(sigma.W) == 0) {stop('Set the value of the standard deviation of the level-2 continuous predictor')}
if (sigma.v0 < 0) {stop('The standard deviation of the random intercept must be positive')}
if (sigma.v1 < 0) {stop('The standard deviation of the random slope must be positive')}  
}

cmp.Summary.model.IL = cmpfun(Summary.model.IL)

########################################################################################
########################################################################################
########################################################################################

# Simulate data from Model 1: Y ~ b00 + b01*Z

if (Model == 1){

# Monte Carlo replicates

plan(multisession)
fit = future_lapply(1:length(N.0), function(i){
message(paste('Estimating Power for N.0 =',N.0[i],'and N.1 =',N.1[i]))
cmp.Summary.model.IL(Model, N, N.0[i], N.1[i], T, 
isX.center, Ylag.center, isW.center, alpha, 
b00, b01.Z, b01.W, b10, b11.Z, b11.W, sigma, rho, sigma.v0, sigma.v1,rho.v, 
mu.W, sigma.W, mu.X, mu.X0, mu.X1, sigma.X, sigma.X0, sigma.X1,R,Opt.Method,is.rho.zero)},future.seed = 0xBEEF)

# Distribution of the parameters: fixed effects 

b00.mean = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[1]][[2]]))
b00.se = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[1]][[3]]))
b00.bias = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[1]][[4]]))
CI.b00 = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[1]][[5]]))
power.b00 = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[1]][[6]]))

b01.mean = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[2]][[2]]))
b01.se = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[2]][[3]]))
b01.bias = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[2]][[4]]))
CI.b01 = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[2]][[5]]))
power.b01 = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[2]][[6]]))

# Table fixed fredictors 

coef.names.b00 = rep(0,length(N.0))
coef.names.b01 = rep(0,length(N.0))

for (i in 1:length(N.0)){
coef.names.b00[i] = rep(paste('Fixed intercept','N(Group=1)',N.0[i],'N(Group=0)',N.1[i]))
coef.names.b01[i] = rep(paste('Effect of the level-2 dummy variable on the intercept','N(Group=1)',N.0[i],'N(Group=0)',N.1[i]))
}

coef.sim = data.frame(round(rbind(cbind(rep(b00,length(N.0)),b00.mean,b00.se,b00.bias,CI.b00,power.b00),
cbind(rep(b01.Z,length(N.0)),b01.mean,b01.se,b01.bias,CI.b01,power.b01)),digits=4))
rownames(coef.sim) = c(coef.names.b00,coef.names.b01)
colnames(coef.sim) = c('True value','Mean','Std.error','Bias','(1-alpha)% Coverage','Power')

# Distribution of the parameters: random effects

if (is.rho.zero==TRUE){
sigma.mean = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[1]][[2]]))
sigma.se = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[1]][[3]]))
sigma.bias = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[1]][[4]]))

rho.mean = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[2]][[2]]))
rho.se = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[2]][[3]]))
rho.bias = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[2]][[4]]))

sigma.v0.mean = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[3]][[2]]))
sigma.v0.se = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[3]][[3]]))
sigma.v0.bias = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[3]][[4]]))

# Table random effects 

coef.names.sigma = rep(0,length(N.0))
coef.names.rho = rep(0,length(N.0))
coef.names.sigma.v0 = rep(0,length(N.0))

for (i in 1:length(N.0)){
coef.names.sigma[i] = rep(paste('Standard deviation of the level-1 error','N(Group=1)',N.0[i],'N(Group=0)',N.1[i]))
coef.names.rho[i] = rep(paste('Autocorrelation of the level-1 error','N(Group=1)',N.0[i],'N(Group=0)',N.1[i]))
coef.names.sigma.v0[i] = rep(paste('Standard deviation of the random intercept','N(Group=1)',N.0[i],'N(Group=0)',N.1[i]))
}

cov.sim = data.frame(round(rbind(cbind(rep(sigma,length(N.0)),sigma.mean,sigma.se,sigma.bias),
cbind(rep(rho,length(N.0)),rho.mean,rho.se,rho.bias),
cbind(rep(sigma.v0,length(N.0)),sigma.v0.mean,sigma.v0.se,sigma.v0.bias)),digits=4))
rownames(cov.sim) = c(coef.names.sigma,coef.names.rho,coef.names.sigma.v0)
colnames(cov.sim) = c('True value','Mean','Std.error','Bias')
}

if (is.rho.zero==FALSE){
sigma.mean = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[1]][[2]]))
sigma.se = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[1]][[3]]))
sigma.bias = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[1]][[4]]))

sigma.v0.mean = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[2]][[2]]))
sigma.v0.se = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[2]][[2]]))
sigma.v0.bias = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[2]][[2]]))

# Table random effects 

coef.names.sigma = rep(0,length(N.0))
coef.names.sigma.v0 = rep(0,length(N.0))

for (i in 1:length(N.0)){
coef.names.sigma[i] = rep(paste('Standard deviation of the level-1 error','N(Group=1)',N.0[i],'N(Group=0)',N.1[i]))
coef.names.sigma.v0[i] = rep(paste('Standard deviation of the random intercept','N(Group=1)',N.0[i],'N(Group=0)',N.1[i]))
}

cov.sim = data.frame(round(rbind(cbind(rep(sigma,length(N.0)),sigma.mean,sigma.se,sigma.bias),
cbind(rep(sigma.v0,length(N.0)),sigma.v0.mean,sigma.v0.se,sigma.v0.bias)),digits=4))
rownames(cov.sim) = c(coef.names.sigma,coef.names.sigma.v0)
colnames(cov.sim) = c('True value','Mean','Std.error','Bias')
}

# Power (for the power curve!)

power.curve = cbind(N.0,N.1,power.b00,power.b01)

# Parameters distribution across the R Monte Carlo replicates for maximum N

N = cbind(N.0,N.1)
N.0.max = max(N.0)
N.1.max = max(N.1)
N.max = which.max(N[,which.max(c(N.0.max,N.1.max))])

if (is.rho.zero==TRUE){
b00.sim = fit[[N.max]][[1]][[1]][[1]]
b01.sim = fit[[N.max]][[1]][[2]][[1]]
sigma.sim = fit[[N.max]][[2]][[1]][[1]]
rho.sim = fit[[N.max]][[2]][[2]][[1]]
sigma.v0.sim = fit[[N.max]][[2]][[3]][[1]]

b.sim = data.frame(cbind(b00.sim,b01.sim,rho.sim,sigma.sim,sigma.v0.sim))
colnames(b.sim) = c('b00.hat','b01.hat','rho.hat','sigma.hat','sigma.v0.hat')
}

if (is.rho.zero==FALSE){
b00.sim = fit[[N.max]][[1]][[1]][[1]]
b01.sim = fit[[N.max]][[1]][[2]][[1]]
sigma.sim = fit[[N.max]][[2]][[1]][[1]]
sigma.v0.sim = fit[[N.max]][[2]][[2]][[1]]

b.sim = data.frame(cbind(b00.sim,b01.sim,sigma.sim,sigma.v0.sim))
colnames(b.sim) = c('b00.hat','b01.hat','sigma.hat','sigma.v0.hat')
}

# Trajectories of Y for one replicate

data = Sim.Data.IL(Model,N,N.0[N.max], N.1[N.max], T,isX.center,Ylag.center,isW.center, 
b00, b01.Z, b01.W, b10, b11.Z, b11.W, sigma, rho, sigma.v0, sigma.v1, 
rho.v, mu.W, sigma.W, mu.X, mu.X0, mu.X1, sigma.X, sigma.X0, sigma.X1)

Ymean = aggregate(data, by=list(data$Z,data$subjno),FUN=mean, na.rm=TRUE)

Y0mean = Ymean[which(Ymean$Z==0),]

Q0.05 = quantile(Y0mean$Y[which(Y0mean$Z==0)],probs=0.05)
N0.Q05 = Y0mean$subjno[which.max(Y0mean$Y[which(Y0mean$Z==0)]<=Q0.05)]

Q0.5 = quantile(Y0mean$Y[which(Y0mean$Z==0)],probs=0.5)
N0.Q5 = Y0mean$subjno[which.max(Y0mean$Y[which(Y0mean$Z==0)]<=Q0.5)]

Q0.95 = quantile(Y0mean$Y[which(Y0mean$Z==0)],probs=0.95)
N0.Q95 = Y0mean$subjno[which.max(Y0mean$Y[which(Y0mean$Z==0)]>=Q0.95)]

Y1mean = Ymean[which(Ymean$Z==1),]

Q1.05 = quantile(Y1mean$Y[which(Y1mean$Z==1)],probs=0.05)
N1.Q05 = Y1mean$subjno[which.max(Y1mean$Y[which(Y1mean$Z==1)]<=Q1.05)]

Q1.5 = quantile(Y1mean$Y[which(Y1mean$Z==1)],probs=0.5)
N1.Q5 = Y1mean$subjno[which.max(Y1mean$Y[which(Y1mean$Z==1)]<=Q1.5)]

Q1.95 = quantile(Y1mean$Y[which(Y1mean$Z==1)],probs=0.95)
N1.Q95 = Y1mean$subjno[which.max(Y1mean$Y[which(Y1mean$Z==1)]>=Q1.95)]

T.list = rep(1:T,2)
data$Z = as.factor(data$Z)
Ni.05 = c(which(data$subjno==N0.Q05),which(data$subjno==N1.Q05))
Ni.5 = c(which(data$subjno==N0.Q5),which(data$subjno==N1.Q5))
Ni.95 = c(which(data$subjno==N0.Q95),which(data$subjno==N1.Q95))

data.IL.Q05 = data.frame(cbind(data[Ni.05,],Time=T.list))
data.IL.Q5 = data.frame(cbind(data[Ni.5,],Time=T.list))
data.IL.Q95 = data.frame(cbind(data[Ni.95,],Time=T.list))

Y.pool = rbind(data.IL.Q05,data.IL.Q5,data.IL.Q95)$Y
Y.IL.min = min(Y.pool)
Y.IL.max = max(Y.pool)
}

########################################################################################

# Simulate data from Model 2: Y ~ b00 + b01*W

if (Model == 2){

# Monte Carlo replicates

plan(multisession)
fit = future_lapply(1:length(N), function(i){
message(paste('Estimating Power for N =',N[i]))
cmp.Summary.model.IL(Model, N[i], N.0, N.1, T, 
isX.center, Ylag.center, isW.center, alpha, 
b00, b01.Z, b01.W, b10, b11.Z, b11.W, sigma, rho, sigma.v0, sigma.v1,rho.v, 
mu.W, sigma.W, mu.X, mu.X0, mu.X1, sigma.X, sigma.X0, sigma.X1,R,Opt.Method,is.rho.zero)},future.seed = 0xBEEF)

# Distribution of the parameters: fixed effects 

b00.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[2]]))
b00.se = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[3]]))
b00.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[4]]))
CI.b00 = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[5]]))
power.b00 = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[6]]))

b01.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[2]][[2]]))
b01.se = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[2]][[3]]))
b01.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[2]][[4]]))
CI.b01 = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[2]][[5]]))
power.b01 = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[2]][[6]]))

# Table fixed fredictors

coef.names.b00 = rep(0,length(N))
coef.names.b01 = rep(0,length(N))

for (i in 1:length(N)){
coef.names.b00[i] = rep(paste('Fixed intercept','N',N[i]))
coef.names.b01[i] = rep(paste('Effect of the level-2 continuous variable on the intercept','N',N[i]))
}

coef.sim = data.frame(round(rbind(cbind(rep(b00,length(N)),b00.mean,b00.se,b00.bias,CI.b00,power.b00),
cbind(rep(b01.W,length(N)),b01.mean,b01.se,b01.bias,CI.b01,power.b01)),digits=4))
rownames(coef.sim) = c(coef.names.b00,coef.names.b01)
colnames(coef.sim) = c('True value','Mean','Std.error','Bias','(1-alpha)% Coverage','Power')

# Distribution of the parameters: random effects

if (is.rho.zero==TRUE){
sigma.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[1]][[2]]))
sigma.se = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[3]]))
sigma.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[4]]))

rho.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[2]][[2]]))
rho.se = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[2]][[3]]))
rho.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[2]][[4]]))

sigma.v0.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[3]][[2]]))
sigma.v0.se = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[3]][[3]]))
sigma.v0.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[3]][[4]]))

# Table random effects 

coef.names.sigma = rep(0,length(N))
coef.names.rho = rep(0,length(N))
coef.names.sigma.v0 = rep(0,length(N))

for (i in 1:length(N)){
coef.names.sigma[i] = rep(paste('Standard deviation of the level-1 error','N',N[i]))
coef.names.rho[i] = rep(paste('Autocorrelation of the level-1 error','N',N[i]))
coef.names.sigma.v0[i] = rep(paste('Standard deviation of the random intercept','N',N[i]))
}

cov.sim = data.frame(round(rbind(cbind(rep(sigma,length(N)),sigma.mean,sigma.se,sigma.bias),
cbind(rep(rho,length(N)),rho.mean,rho.se,rho.bias),
cbind(rep(sigma.v0,length(N)),sigma.v0.mean,sigma.v0.se,sigma.v0.bias)),digits=4))
rownames(cov.sim) = c(coef.names.sigma,coef.names.rho,coef.names.sigma.v0)
colnames(cov.sim) = c('True value','Mean','Std.error','Bias')
}

if (is.rho.zero==FALSE){
sigma.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[1]][[2]]))
sigma.se = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[3]]))
sigma.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[4]]))

sigma.v0.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[2]][[2]]))
sigma.v0.se = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[2]][[3]]))
sigma.v0.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[2]][[4]]))

# Table random effects 

coef.names.sigma = rep(0,length(N))
coef.names.sigma.v0 = rep(0,length(N))

for (i in 1:length(N)){
coef.names.sigma[i] = rep(paste('Standard deviation of the level-1 error','N',N[i]))
coef.names.sigma.v0[i] = rep(paste('Standard deviation of the random intercept','N',N[i]))
}

cov.sim = data.frame(round(rbind(cbind(rep(sigma,length(N)),sigma.mean,sigma.se,sigma.bias),
cbind(rep(sigma.v0,length(N)),sigma.v0.mean,sigma.v0.se,sigma.v0.bias)),digits=4))
rownames(cov.sim) = c(coef.names.sigma,coef.names.sigma.v0)
colnames(cov.sim) = c('True value','Mean','Std.error','Bias')
}

# Power (for the power curve!)

power.curve = cbind(N,power.b00,power.b01)

# Parameters distribution across the R Monte Carlo replicates for maximum N

N.max = which.max(N)

if (is.rho.zero==TRUE){
b00.sim = fit[[N.max]][[1]][[1]][[1]]
b01.sim = fit[[N.max]][[1]][[2]][[1]]
sigma.sim = fit[[N.max]][[2]][[1]][[1]]
rho.sim = fit[[N.max]][[2]][[2]][[1]]
sigma.v0.sim = fit[[N.max]][[2]][[3]][[1]]

b.sim = data.frame(cbind(b00.sim,b01.sim,rho.sim,sigma.sim,sigma.v0.sim))
colnames(b.sim) = c('b00.hat','b01.hat','rho.hat','sigma.hat','sigma.v0.hat')
}

if (is.rho.zero==FALSE){
b00.sim = fit[[N.max]][[1]][[1]][[1]]
b01.sim = fit[[N.max]][[1]][[2]][[1]]
sigma.sim = fit[[N.max]][[2]][[1]][[1]]
sigma.v0.sim = fit[[N.max]][[2]][[2]][[1]]

b.sim = data.frame(cbind(b00.sim,b01.sim,sigma.sim,sigma.v0.sim))
colnames(b.sim) = c('b00.hat','b01.hat','sigma.hat','sigma.v0.hat')
}

# Trajectories of Y for one replicate

data = Sim.Data.IL(Model,N[N.max],N.0, N.1, T,isX.center,Ylag.center,isW.center, 
b00, b01.Z, b01.W, b10, b11.Z, b11.W, sigma, rho, sigma.v0, sigma.v1, 
rho.v, mu.W, sigma.W, mu.X, mu.X0, mu.X1, sigma.X, sigma.X0, sigma.X1)

Ymean = aggregate(data, by=list(data$subjno),FUN=mean, na.rm=TRUE)

Q.05 = quantile(Ymean$Y,probs=0.05)
N.Q05 = Ymean$subjno[which.max(Ymean$Y<=Q.05)]

Q.5 = quantile(Ymean$Y,probs=0.5)
N.Q5 = Ymean$subjno[which.max(Ymean$Y<=Q.5)]

Q.95 = quantile(Ymean$Y,probs=0.95)
N.Q95 = Ymean$subjno[which.max(Ymean$Y>=Q.95)]

T.list = rep(1:T,1)
Ni.05 = which(data$subjno==N.Q05)
Ni.5 = which(data$subjno==N.Q5)
Ni.95 = which(data$subjno==N.Q95)

data.IL.Q05 = data.frame(cbind(data[Ni.05,],Time=T.list))
data.IL.Q5 = data.frame(cbind(data[Ni.5,],Time=T.list))
data.IL.Q95 = data.frame(cbind(data[Ni.95,],Time=T.list))

Y.pool = rbind(data.IL.Q05,data.IL.Q5,data.IL.Q95)$Y
Y.IL.min = min(Y.pool)
Y.IL.max = max(Y.pool)
}

########################################################################################

# Simulate data from Model 3: Y ~ b00 + b10*X

if (Model == 3){

# Monte Carlo replicates

plan(multisession)
fit = future_lapply(1:length(N), function(i){
message(paste('Estimating Power for N =',N[i]))
cmp.Summary.model.IL(Model, N[i], N.0, N.1, T, 
isX.center, Ylag.center, isW.center, alpha, 
b00, b01.Z, b01.W, b10, b11.Z, b11.W, sigma, rho, sigma.v0, sigma.v1,rho.v, 
mu.W, sigma.W, mu.X, mu.X0, mu.X1, sigma.X, sigma.X0, sigma.X1,R,Opt.Method,is.rho.zero)},future.seed = 0xBEEF)

# Distribution of the parameters: random effects 

b00.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[2]]))
b00.se = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[3]]))
b00.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[4]]))
CI.b00 = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[5]]))
power.b00 = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[6]]))

b10.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[2]][[2]]))
b10.se = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[2]][[3]]))
b10.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[2]][[4]]))
CI.b10 = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[2]][[5]]))
power.b10 = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[2]][[6]]))

# Table fixed effects

coef.names.b00 = rep(0,length(N))
coef.names.b10 = rep(0,length(N))

for (i in 1:length(N)){
coef.names.b00[i] = rep(paste('Fixed intercept','N',N[i]))
coef.names.b10[i] = rep(paste('Fixed slope','N',N[i]))
}

coef.sim = data.frame(round(rbind(cbind(rep(b00,length(N)),b00.mean,b00.se,b00.bias,CI.b00,power.b00),
cbind(rep(b10,length(N)),b10.mean,b10.se,b10.bias,CI.b10,power.b10)),digits=4))
rownames(coef.sim) = c(coef.names.b00,coef.names.b10)
colnames(coef.sim) = c('True value','Mean','Std.error','Bias','(1-alpha)% Coverage','Power')

# Distribution of the parameters: random effects

if (is.rho.zero==TRUE){
sigma.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[1]][[2]]))
sigma.se = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[3]]))
sigma.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[4]]))

rho.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[2]][[2]]))
rho.se = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[2]][[3]]))
rho.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[2]][[4]]))

sigma.v0.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[3]][[2]]))
sigma.v0.se = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[3]][[3]]))
sigma.v0.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[3]][[4]]))

sigma.v1.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[4]][[2]]))
sigma.v1.se = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[4]][[3]]))
sigma.v1.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[4]][[4]]))

rho.v.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[5]][[2]]))
rho.v.se = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[5]][[3]]))
rho.v.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[5]][[4]]))

# Table random effects 

coef.names.sigma = rep(0,length(N))
coef.names.rho = rep(0,length(N))
coef.names.sigma.v0 = rep(0,length(N))
coef.names.sigma.v1 = rep(0,length(N))
coef.names.rho.v = rep(0,length(N))

for (i in 1:length(N)){
coef.names.sigma[i] = rep(paste('Standard deviation of the level-1 error','N',N[i]))
coef.names.rho[i] = rep(paste('Autocorrelation of the level-1 error','N',N[i]))
coef.names.sigma.v0[i] = rep(paste('Standard deviation of the random intercept','N',N[i]))
coef.names.sigma.v1[i] = rep(paste('Standard deviation of the random slope','N',N[i]))
coef.names.rho.v[i] = rep(paste('Correlation between the random intercept and the random slope','N',N[i]))
}

cov.sim = data.frame(round(rbind(cbind(rep(sigma,length(N)),sigma.mean,sigma.se,sigma.bias),
cbind(rep(rho,length(N)),rho.mean,rho.se,rho.bias),
cbind(rep(sigma.v0,length(N)),sigma.v0.mean,sigma.v0.se,sigma.v0.bias),
cbind(rep(sigma.v1,length(N)),sigma.v1.mean,sigma.v1.se,sigma.v1.bias),
cbind(rep(rho.v,length(N)),rho.v.mean,rho.v.se,rho.v.bias)),digits=4))
rownames(cov.sim) = c(coef.names.sigma,coef.names.rho,coef.names.sigma.v0,coef.names.sigma.v1,coef.names.rho.v)
colnames(cov.sim) = c('True value','Mean','Std.error','Bias')
}

if (is.rho.zero==FALSE){
sigma.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[1]][[2]]))
sigma.se = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[3]]))
sigma.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[4]]))

sigma.v0.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[2]][[2]]))
sigma.v0.se = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[2]][[3]]))
sigma.v0.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[2]][[4]]))

sigma.v1.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[3]][[2]]))
sigma.v1.se = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[3]][[3]]))
sigma.v1.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[3]][[4]]))

rho.v.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[4]][[2]]))
rho.v.se = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[4]][[3]]))
rho.v.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[4]][[4]]))

# Table random effects 

coef.names.sigma = rep(0,length(N))
coef.names.sigma.v0 = rep(0,length(N))
coef.names.sigma.v1 = rep(0,length(N))
coef.names.rho.v = rep(0,length(N))

for (i in 1:length(N)){
coef.names.sigma[i] = rep(paste('Standard deviation of the level-1 error','N',N[i]))
coef.names.sigma.v0[i] = rep(paste('Standard deviation of the random intercept','N',N[i]))
coef.names.sigma.v1[i] = rep(paste('Standard deviation of the random slope','N',N[i]))
coef.names.rho.v[i] = rep(paste('Correlation between the random intercept and the random slope','N',N[i]))
}

cov.sim = data.frame(round(rbind(cbind(rep(sigma,length(N)),sigma.mean,sigma.se,sigma.bias),
cbind(rep(sigma.v0,length(N)),sigma.v0.mean,sigma.v0.se,sigma.v0.bias),
cbind(rep(sigma.v1,length(N)),sigma.v1.mean,sigma.v1.se,sigma.v1.bias),
cbind(rep(rho.v,length(N)),rho.v.mean,rho.v.se,rho.v.bias)),digits=4))
rownames(cov.sim) = c(coef.names.sigma,coef.names.sigma.v0,coef.names.sigma.v1,coef.names.rho.v)
colnames(cov.sim) = c('True value','Mean','Std.error','Bias')
}

# Power (for the power curve!)

power.curve = cbind(N,power.b00,power.b10)

# Parameters distribution across the R Monte Carlo replicates for maximum N

N.max = which.max(N)

if (is.rho.zero==TRUE){
b00.sim = fit[[N.max]][[1]][[1]][[1]]
b10.sim = fit[[N.max]][[1]][[2]][[1]]
sigma.sim = fit[[N.max]][[2]][[1]][[1]]
rho.sim = fit[[N.max]][[2]][[2]][[1]]
sigma.v0.sim = fit[[N.max]][[2]][[3]][[1]]
sigma.v1.sim = fit[[N.max]][[2]][[4]][[1]]
rho.v.sim = fit[[N.max]][[2]][[5]][[1]]

b.sim = data.frame(cbind(b00.sim,b10.sim,rho.sim,sigma.sim,sigma.v0.sim,
sigma.v1.sim,rho.v.sim))
colnames(b.sim) = c('b00.hat','b10.hat','rho.hat','sigma.hat','sigma.v0.hat','sigma.v1.hat','rho.v.hat')
}

if (is.rho.zero==FALSE){
b00.sim = fit[[N.max]][[1]][[1]][[1]]
b10.sim = fit[[N.max]][[1]][[2]][[1]]
sigma.sim = fit[[N.max]][[2]][[1]][[1]]
sigma.v0.sim = fit[[N.max]][[2]][[2]][[1]]
sigma.v1.sim = fit[[N.max]][[2]][[3]][[1]]
rho.v.sim = fit[[N.max]][[2]][[4]][[1]]

b.sim = data.frame(cbind(b00.sim,b10.sim,sigma.sim,sigma.v0.sim,sigma.v1.sim,rho.v.sim))
colnames(b.sim) = c('b00.hat','b10.hat','sigma.hat','sigma.v0.hat','sigma.v1.hat','rho.v.hat')
}

# Trajectories of Y for one replicate

data = Sim.Data.IL(Model,N[N.max],N.0, N.1, T,isX.center,Ylag.center,isW.center, 
b00, b01.Z, b01.W, b10, b11.Z, b11.W, sigma, rho, sigma.v0, sigma.v1, 
rho.v, mu.W, sigma.W, mu.X, mu.X0, mu.X1, sigma.X, sigma.X0, sigma.X1)

Ymean = aggregate(data, by=list(data$subjno),FUN=mean, na.rm=TRUE)

Q.05 = quantile(Ymean$Y,probs=0.05)
N.Q05 = Ymean$subjno[which.max(Ymean$Y<=Q.05)]

Q.5 = quantile(Ymean$Y,probs=0.5)
N.Q5 = Ymean$subjno[which.max(Ymean$Y<=Q.5)]

Q.95 = quantile(Ymean$Y,probs=0.95)
N.Q95 = Ymean$subjno[which.max(Ymean$Y>=Q.95)]

T.list = rep(1:T,1)
Ni.05 = which(data$subjno==N.Q05)
Ni.5 = which(data$subjno==N.Q5)
Ni.95 = which(data$subjno==N.Q95)

data.IL.Q05 = data.frame(cbind(data[Ni.05,],Time=T.list))
data.IL.Q5 = data.frame(cbind(data[Ni.5,],Time=T.list))
data.IL.Q95 = data.frame(cbind(data[Ni.95,],Time=T.list))

Y.pool = rbind(data.IL.Q05,data.IL.Q5,data.IL.Q95)$Y
Y.IL.min = min(Y.pool)
Y.IL.max = max(Y.pool)
}

########################################################################################

# Simulate data from Model 4: Y ~ b00 + b10*X fixed slope

if (Model == 4){

# Monte Carlo replicates

plan(multisession)
fit = future_lapply(1:length(N), function(i){
message(paste('Estimating Power for N =',N[i]))
cmp.Summary.model.IL(Model, N[i], N.0, N.1, T, 
isX.center, Ylag.center, isW.center, alpha, 
b00, b01.Z, b01.W, b10, b11.Z, b11.W, sigma, rho, sigma.v0, sigma.v1,rho.v, 
mu.W, sigma.W, mu.X, mu.X0, mu.X1, sigma.X, sigma.X0, sigma.X1,R,Opt.Method,is.rho.zero)},future.seed = 0xBEEF)

# Distribution of the parameters: random effects 

b00.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[2]]))
b00.se = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[3]]))
b00.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[4]]))
CI.b00 = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[5]]))
power.b00 = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[6]]))

b10.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[2]][[2]]))
b10.se = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[2]][[3]]))
b10.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[2]][[4]]))
CI.b10 = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[2]][[5]]))
power.b10 = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[2]][[6]]))

# Table fixed effects

coef.names.b00 = rep(0,length(N))
coef.names.b10 = rep(0,length(N))

for (i in 1:length(N)){
coef.names.b00[i] = rep(paste('Fixed intercept','N',N[i]))
coef.names.b10[i] = rep(paste('Fixed slope','N',N[i]))
}

coef.sim = data.frame(round(rbind(cbind(rep(b00,length(N)),b00.mean,b00.se,b00.bias,CI.b00,power.b00),
cbind(rep(b10,length(N)),b10.mean,b10.se,b10.bias,CI.b10,power.b10)),digits=4))
rownames(coef.sim) = c(coef.names.b00,coef.names.b10)
colnames(coef.sim) = c('True value','Mean','Std.error','Bias','(1-alpha)% Coverage','Power')

# Distribution of the parameters: random effects

if (is.rho.zero==TRUE){
sigma.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[1]][[2]]))
sigma.se = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[3]]))
sigma.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[4]]))

rho.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[2]][[2]]))
rho.se = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[2]][[3]]))
rho.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[2]][[4]]))

sigma.v0.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[3]][[2]]))
sigma.v0.se = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[3]][[3]]))
sigma.v0.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[3]][[4]]))

# Table random effects 

coef.names.sigma = rep(0,length(N))
coef.names.rho = rep(0,length(N))
coef.names.sigma.v0 = rep(0,length(N))

for (i in 1:length(N)){
coef.names.sigma[i] = rep(paste('Standard deviation of the level-1 error','N',N[i]))
coef.names.rho[i] = rep(paste('Autocorrelation of the level-1 error','N',N[i]))
coef.names.sigma.v0[i] = rep(paste('Standard deviation of the random intercept','N',N[i]))
}

cov.sim = data.frame(round(rbind(cbind(rep(sigma,length(N)),sigma.mean,sigma.se,sigma.bias),
cbind(rep(rho,length(N)),rho.mean,rho.se,rho.bias),
cbind(rep(sigma.v0,length(N)),sigma.v0.mean,sigma.v0.se,sigma.v0.bias)),digits=4))
rownames(cov.sim) = c(coef.names.sigma,coef.names.rho,coef.names.sigma.v0)
colnames(cov.sim) = c('True value','Mean','Std.error','Bias')
}

if (is.rho.zero==FALSE){
sigma.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[1]][[2]]))
sigma.se = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[3]]))
sigma.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[4]]))

sigma.v0.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[2]][[2]]))
sigma.v0.se = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[2]][[3]]))
sigma.v0.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[2]][[4]]))

# Table random effects 

coef.names.sigma = rep(0,length(N))
coef.names.rho = rep(0,length(N))
coef.names.sigma.v0 = rep(0,length(N))
coef.names.sigma.v1 = rep(0,length(N))

for (i in 1:length(N)){
coef.names.sigma[i] = rep(paste('Standard deviation of the level-1 error','N',N[i]))
coef.names.sigma.v0[i] = rep(paste('Standard deviation of the random intercept','N',N[i]))
}

cov.sim = data.frame(round(rbind(cbind(rep(sigma,length(N)),sigma.mean,sigma.se,sigma.bias),
cbind(rep(sigma.v0,length(N)),sigma.v0.mean,sigma.v0.se,sigma.v0.bias)),digits=4))
rownames(cov.sim) = c(coef.names.sigma,coef.names.sigma.v0)
colnames(cov.sim) = c('True value','Mean','Std.error','Bias')
}

# Power (for the power curve!)

power.curve = cbind(N,power.b00,power.b10)

# Parameters distribution across the R Monte Carlo replicates for maximum N

N.max = which.max(N)

if (is.rho.zero==TRUE){
b00.sim = fit[[N.max]][[1]][[1]][[1]]
b10.sim = fit[[N.max]][[1]][[2]][[1]]
sigma.sim = fit[[N.max]][[2]][[1]][[1]]
rho.sim = fit[[N.max]][[2]][[2]][[1]]
sigma.v0.sim = fit[[N.max]][[2]][[3]][[1]]

b.sim = data.frame(cbind(b00.sim,b10.sim,rho.sim,sigma.sim,sigma.v0.sim))
colnames(b.sim) = c('b00.hat','b10.hat','rho.hat','sigma.hat','sigma.v0.hat')
}

if (is.rho.zero==FALSE){
b00.sim = fit[[N.max]][[1]][[1]][[1]]
b10.sim = fit[[N.max]][[1]][[2]][[1]]
sigma.sim = fit[[N.max]][[2]][[1]][[1]]
sigma.v0.sim = fit[[N.max]][[2]][[2]][[1]]

b.sim = data.frame(cbind(b00.sim,b10.sim,sigma.sim,sigma.v0.sim))
colnames(b.sim) = c('b00.hat','b10.hat','sigma.hat','sigma.v0.hat')
}

# Trajectories of Y for one replicate

data = Sim.Data.IL(Model,N[N.max],N.0, N.1, T,isX.center,Ylag.center,isW.center, 
b00, b01.Z, b01.W, b10, b11.Z, b11.W, sigma, rho, sigma.v0, sigma.v1, 
rho.v, mu.W, sigma.W, mu.X, mu.X0, mu.X1, sigma.X, sigma.X0, sigma.X1)

Ymean = aggregate(data, by=list(data$subjno),FUN=mean, na.rm=TRUE)

Q.05 = quantile(Ymean$Y,probs=0.05)
N.Q05 = Ymean$subjno[which.max(Ymean$Y<=Q.05)]

Q.5 = quantile(Ymean$Y,probs=0.5)
N.Q5 = Ymean$subjno[which.max(Ymean$Y<=Q.5)]

Q.95 = quantile(Ymean$Y,probs=0.95)
N.Q95 = Ymean$subjno[which.max(Ymean$Y>=Q.95)]

T.list = rep(1:T,1)
Ni.05 = which(data$subjno==N.Q05)
Ni.5 = which(data$subjno==N.Q5)
Ni.95 = which(data$subjno==N.Q95)

data.IL.Q05 = data.frame(cbind(data[Ni.05,],Time=T.list))
data.IL.Q5 = data.frame(cbind(data[Ni.5,],Time=T.list))
data.IL.Q95 = data.frame(cbind(data[Ni.95,],Time=T.list))

Y.pool = rbind(data.IL.Q05,data.IL.Q5,data.IL.Q95)$Y
Y.IL.min = min(Y.pool)
Y.IL.max = max(Y.pool)
}

########################################################################################

# Simulate data from Model 5: Y ~ b00 + b01*Z + b10*X + b11*Z*X

if (Model == 5){

# Monte Carlo replicates

plan(multisession)
fit = future_lapply(1:length(N.0), function(i){
message(paste('Estimating Power for N.0 =',N.0[i],'and N.1 =',N.1[i]))
cmp.Summary.model.IL(Model, N, N.0[i], N.1[i], T, 
isX.center, Ylag.center, isW.center, alpha, 
b00, b01.Z, b01.W, b10, b11.Z, b11.W, sigma, rho, sigma.v0, sigma.v1,rho.v, 
mu.W, sigma.W, mu.X, mu.X0, mu.X1, sigma.X, sigma.X0, sigma.X1,R,Opt.Method,is.rho.zero)},future.seed = 0xBEEF)

# Distribution of the parameters: fixed effects 

b00.mean = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[1]][[2]]))
b00.se = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[1]][[3]]))
b00.bias = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[1]][[4]]))
CI.b00 = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[1]][[5]]))
power.b00 = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[1]][[6]]))

b01.mean = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[2]][[2]]))
b01.se = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[2]][[3]]))
b01.bias = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[2]][[4]]))
CI.b01 = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[2]][[5]]))
power.b01 = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[2]][[6]]))

b10.mean = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[3]][[2]]))
b10.se = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[3]][[3]]))
b10.bias = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[3]][[4]]))
CI.b10 = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[3]][[5]]))
power.b10 = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[3]][[6]]))

b11.mean = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[4]][[2]]))
b11.se = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[4]][[3]]))
b11.bias = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[4]][[4]]))
CI.b11 = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[4]][[5]]))
power.b11 = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[4]][[6]]))

# Table fixed fredictors 

coef.names.b00 = rep(0,length(N.0))
coef.names.b01 = rep(0,length(N.0))
coef.names.b10 = rep(0,length(N.0))
coef.names.b11 = rep(0,length(N.0))

for (i in 1:length(N.0)){
coef.names.b00[i] = rep(paste('Fixed intercept','N(Group=1)',N.0[i],'N(Group=0)',N.1[i]))
coef.names.b01[i] = rep(paste('Effect of the level-2 dummy variable on the intercept','N(Group=1)',N.0[i],'N(Group=0)',N.1[i]))
coef.names.b10[i] = rep(paste('Fixed slope','N(Group=1)',N.0[i],'N(Group=0)',N.1[i]))
coef.names.b11[i] = rep(paste('Effect of the level-2 dummy variable on the slope','N(Group=1)',N.0[i],'N(Group=0)',N.1[i]))
}

coef.sim = data.frame(round(rbind(cbind(rep(b00,length(N.0)),b00.mean,b00.se,b00.bias,CI.b00,power.b00),
cbind(rep(b01.Z,length(N.0)),b01.mean,b01.se,b01.bias,CI.b01,power.b01),
cbind(rep(b10,length(N.0)),b10.mean,b10.se,b10.bias,CI.b10,power.b10),
cbind(rep(b11.Z,length(N.0)),b11.mean,b11.se,b11.bias,CI.b11,power.b11)),digits=4))
rownames(coef.sim) = c(coef.names.b00,coef.names.b01,coef.names.b10,coef.names.b11)
colnames(coef.sim) = c('True value','Mean','Std.error','Bias','(1-alpha)% Coverage','Power')

# Distribution of the parameters: random effects

if (is.rho.zero==TRUE){
sigma.mean = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[1]][[2]]))
sigma.se = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[1]][[3]]))
sigma.bias = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[1]][[4]]))

rho.mean = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[2]][[2]]))
rho.se = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[2]][[3]]))
rho.bias = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[2]][[4]]))

sigma.v0.mean = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[3]][[2]]))
sigma.v0.se = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[3]][[3]]))
sigma.v0.bias = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[3]][[4]]))

sigma.v1.mean = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[4]][[2]]))
sigma.v1.se = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[4]][[3]]))
sigma.v1.bias = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[4]][[4]]))

rho.v.mean = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[5]][[2]]))
rho.v.se = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[5]][[3]]))
rho.v.bias = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[5]][[4]]))

# Table random effects 

coef.names.sigma = rep(0,length(N.0))
coef.names.rho = rep(0,length(N.0))
coef.names.sigma.v0 = rep(0,length(N.0))
coef.names.sigma.v1 = rep(0,length(N.0))
coef.names.rho.v = rep(0,length(N.0))

for (i in 1:length(N.0)){
coef.names.sigma[i] = rep(paste('Standard deviation of the level-1 error','N(Group=1)',N.0[i],'N(Group=0)',N.1[i]))
coef.names.rho[i] = rep(paste('Autocorrelation of the level-1 error','N(Group=1)',N.0[i],'N(Group=0)',N.1[i]))
coef.names.sigma.v0[i] = rep(paste('Standard deviation of the random intercept','N(Group=1)',N.0[i],'N(Group=0)',N.1[i]))
coef.names.sigma.v1[i] = rep(paste('Standard deviation of the random slope','N(Group=1)',N.0[i],'N(Group=0)',N.1[i]))
coef.names.rho.v[i] = rep(paste('Correlation between the random intercept and the random slope','N(Group=1)',N.0[i],'N(Group=0)',N.1[i]))
}

cov.sim = data.frame(round(rbind(cbind(rep(sigma,length(N.0)),sigma.mean,sigma.se,sigma.bias),
cbind(rep(rho,length(N.0)),rho.mean,rho.se,rho.bias),
cbind(rep(sigma.v0,length(N.0)),sigma.v0.mean,sigma.v0.se,sigma.v0.bias),
cbind(rep(sigma.v1,length(N.0)),sigma.v1.mean,sigma.v1.se,sigma.v1.bias),
cbind(rep(rho.v,length(N.0)),rho.v.mean,rho.v.se,rho.v.bias)),digits=4))
rownames(cov.sim) = c(coef.names.sigma,coef.names.rho,coef.names.sigma.v0,coef.names.sigma.v1,coef.names.rho.v)
colnames(cov.sim) = c('True value','Mean','Std.error','Bias')
}

if (is.rho.zero==FALSE){
sigma.mean = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[1]][[2]]))
sigma.se = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[1]][[3]]))
sigma.bias = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[1]][[4]]))

sigma.v0.mean = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[2]][[2]]))
sigma.v0.se = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[2]][[3]]))
sigma.v0.bias = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[2]][[4]]))

sigma.v1.mean = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[3]][[2]]))
sigma.v1.se = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[3]][[3]]))
sigma.v1.bias = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[3]][[4]]))

rho.v.mean = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[4]][[2]]))
rho.v.se = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[4]][[3]]))
rho.v.bias = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[4]][[4]]))

# Table random effects 

coef.names.sigma = rep(0,length(N.0))
coef.names.sigma.v0 = rep(0,length(N.0))
coef.names.sigma.v1 = rep(0,length(N.0))
coef.names.rho.v = rep(0,length(N.0))

for (i in 1:length(N.0)){
coef.names.sigma[i] = rep(paste('Standard deviation of the level-1 error','N(Group=1)',N.0[i],'N(Group=0)',N.1[i]))
coef.names.sigma.v0[i] = rep(paste('Standard deviation of the random intercept','N(Group=1)',N.0[i],'N(Group=0)',N.1[i]))
coef.names.sigma.v1[i] = rep(paste('Standard deviation of the random slope','N(Group=1)',N.0[i],'N(Group=0)',N.1[i]))
coef.names.rho.v[i] = rep(paste('Correlation between the random intercept and the random slope','N(Group=1)',N.0[i],'N(Group=0)',N.1[i]))
}

cov.sim = data.frame(round(rbind(cbind(rep(sigma,length(N.0)),sigma.mean,sigma.se,sigma.bias),
cbind(rep(sigma.v0,length(N.0)),sigma.v0.mean,sigma.v0.se,sigma.v0.bias),
cbind(rep(sigma.v1,length(N.0)),sigma.v1.mean,sigma.v1.se,sigma.v1.bias),
cbind(rep(rho.v,length(N.0)),rho.v.mean,rho.v.se,rho.v.bias)),digits=4))
rownames(cov.sim) = c(coef.names.sigma,coef.names.sigma.v0,coef.names.sigma.v1,coef.names.rho.v)
colnames(cov.sim) = c('True value','Mean','Std.error','Bias')
}

# Power (for the power curve!)

power.curve = cbind(N.0,N.1,power.b00,power.b01,power.b10,power.b11)

# Parameters distribution across the R Monte Carlo replicates for maximum N

N = cbind(N.0,N.1)
N.0.max = max(N.0)
N.1.max = max(N.1)
N.max = which.max(N[,which.max(c(N.0.max,N.1.max))])

if (is.rho.zero==TRUE){
b00.sim = fit[[N.max]][[1]][[1]][[1]]
b01.sim = fit[[N.max]][[1]][[2]][[1]]
b10.sim = fit[[N.max]][[1]][[3]][[1]]
b11.sim = fit[[N.max]][[1]][[4]][[1]]
sigma.sim = fit[[N.max]][[2]][[1]][[1]]
rho.sim = fit[[N.max]][[2]][[2]][[1]]
sigma.v0.sim = fit[[N.max]][[2]][[3]][[1]]
sigma.v1.sim = fit[[N.max]][[2]][[3]][[1]]
rho.v.sim = fit[[N.max]][[2]][[4]][[1]]

b.sim = data.frame(cbind(b00.sim,b01.sim,b10.sim,b11.sim,rho.sim,sigma.sim,sigma.v0.sim,sigma.v1.sim,rho.v.sim))
colnames(b.sim) = c('b00.hat','b01.hat','b10.hat','b11.hat','rho.hat','sigma.hat','sigma.v0.hat','sigma.v1.hat','rho.v.hat')
}

if (is.rho.zero==FALSE){
b00.sim = fit[[N.max]][[1]][[1]][[1]]
b01.sim = fit[[N.max]][[1]][[2]][[1]]
b10.sim = fit[[N.max]][[1]][[3]][[1]]
b11.sim = fit[[N.max]][[1]][[4]][[1]]
sigma.sim = fit[[N.max]][[2]][[1]][[1]]
sigma.v0.sim = fit[[N.max]][[2]][[2]][[1]]
sigma.v1.sim = fit[[N.max]][[2]][[3]][[1]]
rho.v.sim = fit[[N.max]][[2]][[4]][[1]]

b.sim = data.frame(cbind(b00.sim,b01.sim,b10.sim,b11.sim,sigma.sim,sigma.v0.sim,sigma.v1.sim,rho.v.sim))
colnames(b.sim) = c('b00.hat','b01.hat','b10.hat','b11.hat','sigma.hat','sigma.v0.hat','sigma.v1.hat','rho.v.hat')
}

# Trajectories of Y for one replicate

data = Sim.Data.IL(Model,N,N.0[N.max], N.1[N.max], T,isX.center,Ylag.center,isW.center, 
b00, b01.Z, b01.W, b10, b11.Z, b11.W, sigma, rho, sigma.v0, sigma.v1, 
rho.v, mu.W, sigma.W, mu.X, mu.X0, mu.X1, sigma.X, sigma.X0, sigma.X1)

Ymean = aggregate(data, by=list(data$Z,data$subjno),FUN=mean, na.rm=TRUE)

Y0mean = Ymean[which(Ymean$Z==0),]

Q0.05 = quantile(Y0mean$Y[which(Y0mean$Z==0)],probs=0.05)
N0.Q05 = Y0mean$subjno[which.max(Y0mean$Y[which(Y0mean$Z==0)]<=Q0.05)]

Q0.5 = quantile(Y0mean$Y[which(Y0mean$Z==0)],probs=0.5)
N0.Q5 = Y0mean$subjno[which.max(Y0mean$Y[which(Y0mean$Z==0)]<=Q0.5)]

Q0.95 = quantile(Y0mean$Y[which(Y0mean$Z==0)],probs=0.95)
N0.Q95 = Y0mean$subjno[which.max(Y0mean$Y[which(Y0mean$Z==0)]>=Q0.95)]

Y1mean = Ymean[which(Ymean$Z==1),]

Q1.05 = quantile(Y1mean$Y[which(Y1mean$Z==1)],probs=0.05)
N1.Q05 = Y1mean$subjno[which.max(Y1mean$Y[which(Y1mean$Z==1)]<=Q1.05)]

Q1.5 = quantile(Y1mean$Y[which(Y1mean$Z==1)],probs=0.5)
N1.Q5 = Y1mean$subjno[which.max(Y1mean$Y[which(Y1mean$Z==1)]<=Q1.5)]

Q1.95 = quantile(Y1mean$Y[which(Y1mean$Z==1)],probs=0.95)
N1.Q95 = Y1mean$subjno[which.max(Y1mean$Y[which(Y1mean$Z==1)]>=Q1.95)]

T.list = rep(1:T,2)
data$Z = as.factor(data$Z)
Ni.05 = c(which(data$subjno==N0.Q05),which(data$subjno==N1.Q05))
Ni.5 = c(which(data$subjno==N0.Q5),which(data$subjno==N1.Q5))
Ni.95 = c(which(data$subjno==N0.Q95),which(data$subjno==N1.Q95))

data.IL.Q05 = data.frame(cbind(data[Ni.05,],Time=T.list))
data.IL.Q5 = data.frame(cbind(data[Ni.5,],Time=T.list))
data.IL.Q95 = data.frame(cbind(data[Ni.95,],Time=T.list))

Y.pool = rbind(data.IL.Q05,data.IL.Q5,data.IL.Q95)$Y
Y.IL.min = min(Y.pool)
Y.IL.max = max(Y.pool)
}

########################################################################################

# Simulate data from Model 6: Y ~ b00 + b01*Z + b10*X + b11*Z*X fixed slope

if (Model == 6){

# Monte Carlo replicates

plan(multisession)
fit = future_lapply(1:length(N.0), function(i){
message(paste('Estimating Power for N.0 =',N.0[i],'and N.1 =',N.1[i]))
cmp.Summary.model.IL(Model, N, N.0[i], N.1[i], T, 
isX.center, Ylag.center, isW.center, alpha, 
b00, b01.Z, b01.W, b10, b11.Z, b11.W, sigma, rho, sigma.v0, sigma.v1,rho.v, 
mu.W, sigma.W, mu.X, mu.X0, mu.X1, sigma.X, sigma.X0, sigma.X1,R,Opt.Method,is.rho.zero)},future.seed = 0xBEEF)

# Distribution of the parameters: fixed effects 

b00.mean = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[1]][[2]]))
b00.se = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[1]][[3]]))
b00.bias = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[1]][[4]]))
CI.b00 = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[1]][[5]]))
power.b00 = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[1]][[6]]))

b01.mean = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[2]][[2]]))
b01.se = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[2]][[3]]))
b01.bias = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[2]][[4]]))
CI.b01 = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[2]][[5]]))
power.b01 = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[2]][[6]]))

b10.mean = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[3]][[2]]))
b10.se = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[3]][[3]]))
b10.bias = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[3]][[4]]))
CI.b10 = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[3]][[5]]))
power.b10 = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[3]][[6]]))

b11.mean = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[4]][[2]]))
b11.se = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[4]][[3]]))
b11.bias = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[4]][[4]]))
CI.b11 = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[4]][[5]]))
power.b11 = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[4]][[6]]))

# Table fixed fredictors 

coef.names.b00 = rep(0,length(N.0))
coef.names.b01 = rep(0,length(N.0))
coef.names.b10 = rep(0,length(N.0))
coef.names.b11 = rep(0,length(N.0))

for (i in 1:length(N.0)){
coef.names.b00[i] = rep(paste('Fixed intercept','N(Group=1)',N.0[i],'N(Group=0)',N.1[i]))
coef.names.b01[i] = rep(paste('Effect of the level-2 dummy variable on the intercept','N(Group=1)',N.0[i],'N(Group=0)',N.1[i]))
coef.names.b10[i] = rep(paste('Fixed slope','N(Group=1)',N.0[i],'N(Group=0)',N.1[i]))
coef.names.b11[i] = rep(paste('Effect of the level-2 dummy variable on the slope','N(Group=1)',N.0[i],'N(Group=0)',N.1[i]))
}

coef.sim = data.frame(round(rbind(cbind(rep(b00,length(N.0)),b00.mean,b00.se,b00.bias,CI.b00,power.b00),
cbind(rep(b01.Z,length(N.0)),b01.mean,b01.se,b01.bias,CI.b01,power.b01),
cbind(rep(b10,length(N.0)),b10.mean,b10.se,b10.bias,CI.b10,power.b10),
cbind(rep(b11.Z,length(N.0)),b11.mean,b11.se,b11.bias,CI.b11,power.b11)),digits=4))
rownames(coef.sim) = c(coef.names.b00,coef.names.b01,coef.names.b10,coef.names.b11)
colnames(coef.sim) = c('True value','Mean','Std.error','Bias','(1-alpha)% Coverage','Power')

# Distribution of the parameters: random effects

if (is.rho.zero==TRUE){
sigma.mean = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[1]][[2]]))
sigma.se = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[1]][[3]]))
sigma.bias = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[1]][[4]]))

rho.mean = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[2]][[2]]))
rho.se = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[2]][[3]]))
rho.bias = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[2]][[4]]))

sigma.v0.mean = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[3]][[2]]))
sigma.v0.se = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[3]][[3]]))
sigma.v0.bias = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[3]][[4]]))

# Table random effects 

coef.names.sigma = rep(0,length(N.0))
coef.names.rho = rep(0,length(N.0))
coef.names.sigma.v0 = rep(0,length(N.0))

for (i in 1:length(N.0)){
coef.names.sigma[i] = rep(paste('Standard deviation of the level-1 error','N(Group=1)',N.0[i],'N(Group=0)',N.1[i]))
coef.names.rho[i] = rep(paste('Autocorrelation of the level-1 error','N(Group=1)',N.0[i],'N(Group=0)',N.1[i]))
coef.names.sigma.v0[i] = rep(paste('Standard deviation of the random intercept','N(Group=1)',N.0[i],'N(Group=0)',N.1[i]))
}

cov.sim = data.frame(round(rbind(cbind(rep(sigma,length(N.0)),sigma.mean,sigma.se,sigma.bias),
cbind(rep(rho,length(N.0)),rho.mean,rho.se,rho.bias),
cbind(rep(sigma.v0,length(N.0)),sigma.v0.mean,sigma.v0.se,sigma.v0.bias)),digits=4))
rownames(cov.sim) = c(coef.names.sigma,coef.names.rho,coef.names.sigma.v0)
colnames(cov.sim) = c('True value','Mean','Std.error','Bias')
}

if (is.rho.zero==FALSE){
sigma.mean = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[1]][[2]]))
sigma.se = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[1]][[3]]))
sigma.bias = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[1]][[4]]))

sigma.v0.mean = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[2]][[2]]))
sigma.v0.se = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[2]][[3]]))
sigma.v0.bias = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[2]][[4]]))

# Table random effects 

coef.names.sigma = rep(0,length(N.0))
coef.names.sigma.v0 = rep(0,length(N.0))

for (i in 1:length(N.0)){
coef.names.sigma[i] = rep(paste('Standard deviation of the level-1 error','N(Group=1)',N.0[i],'N(Group=0)',N.1[i]))
coef.names.sigma.v0[i] = rep(paste('Standard deviation of the random intercept','N(Group=1)',N.0[i],'N(Group=0)',N.1[i]))
}

cov.sim = data.frame(round(rbind(cbind(rep(sigma,length(N.0)),sigma.mean,sigma.se,sigma.bias),
cbind(rep(sigma.v0,length(N.0)),sigma.v0.mean,sigma.v0.se,sigma.v0.bias)),digits=4))
rownames(cov.sim) = c(coef.names.sigma,coef.names.sigma.v0)
colnames(cov.sim) = c('True value','Mean','Std.error','Bias')
}

# Power (for the power curve!)

power.curve = cbind(N.0,N.1,power.b00,power.b01,power.b10,power.b11)

# Parameters distribution across the R Monte Carlo replicates for maximum N

N = cbind(N.0,N.1)
N.0.max = max(N.0)
N.1.max = max(N.1)
N.max = which.max(N[,which.max(c(N.0.max,N.1.max))])

if (is.rho.zero==TRUE){
b00.sim = fit[[N.max]][[1]][[1]][[1]]
b01.sim = fit[[N.max]][[1]][[2]][[1]]
b10.sim = fit[[N.max]][[1]][[3]][[1]]
b11.sim = fit[[N.max]][[1]][[4]][[1]]
sigma.sim = fit[[N.max]][[2]][[1]][[1]]
rho.sim = fit[[N.max]][[2]][[2]][[1]]
sigma.v0.sim = fit[[N.max]][[2]][[3]][[1]]

b.sim = data.frame(cbind(b00.sim,b01.sim,b10.sim,b11.sim,rho.sim,sigma.sim,sigma.v0.sim))
colnames(b.sim) = c('b00.hat','b01.hat','b10.hat','b11.hat','rho.hat','sigma.hat','sigma.v0.hat')
}

if (is.rho.zero==FALSE){
b00.sim = fit[[N.max]][[1]][[1]][[1]]
b01.sim = fit[[N.max]][[1]][[2]][[1]]
b10.sim = fit[[N.max]][[1]][[3]][[1]]
b11.sim = fit[[N.max]][[1]][[4]][[1]]
sigma.sim = fit[[N.max]][[2]][[1]][[1]]
sigma.v0.sim = fit[[N.max]][[2]][[2]][[1]]

b.sim = data.frame(cbind(b00.sim,b01.sim,b10.sim,b11.sim,sigma.sim,sigma.v0.sim))
colnames(b.sim) = c('b00.hat','b01.hat','b10.hat','b11.hat','sigma.hat','sigma.v0.hat')
}

# Trajectories of Y for one replicate

data = Sim.Data.IL(Model,N,N.0[N.max], N.1[N.max], T,isX.center,Ylag.center,isW.center, 
b00, b01.Z, b01.W, b10, b11.Z, b11.W, sigma, rho, sigma.v0, sigma.v1, 
rho.v, mu.W, sigma.W, mu.X, mu.X0, mu.X1, sigma.X, sigma.X0, sigma.X1)

Ymean = aggregate(data, by=list(data$Z,data$subjno),FUN=mean, na.rm=TRUE)

Y0mean = Ymean[which(Ymean$Z==0),]

Q0.05 = quantile(Y0mean$Y[which(Y0mean$Z==0)],probs=0.05)
N0.Q05 = Y0mean$subjno[which.max(Y0mean$Y[which(Y0mean$Z==0)]<=Q0.05)]

Q0.5 = quantile(Y0mean$Y[which(Y0mean$Z==0)],probs=0.5)
N0.Q5 = Y0mean$subjno[which.max(Y0mean$Y[which(Y0mean$Z==0)]<=Q0.5)]

Q0.95 = quantile(Y0mean$Y[which(Y0mean$Z==0)],probs=0.95)
N0.Q95 = Y0mean$subjno[which.max(Y0mean$Y[which(Y0mean$Z==0)]>=Q0.95)]

Y1mean = Ymean[which(Ymean$Z==1),]

Q1.05 = quantile(Y1mean$Y[which(Y1mean$Z==1)],probs=0.05)
N1.Q05 = Y1mean$subjno[which.max(Y1mean$Y[which(Y1mean$Z==1)]<=Q1.05)]

Q1.5 = quantile(Y1mean$Y[which(Y1mean$Z==1)],probs=0.5)
N1.Q5 = Y1mean$subjno[which.max(Y1mean$Y[which(Y1mean$Z==1)]<=Q1.5)]

Q1.95 = quantile(Y1mean$Y[which(Y1mean$Z==1)],probs=0.95)
N1.Q95 = Y1mean$subjno[which.max(Y1mean$Y[which(Y1mean$Z==1)]>=Q1.95)]

T.list = rep(1:T,2)
data$Z = as.factor(data$Z)
Ni.05 = c(which(data$subjno==N0.Q05),which(data$subjno==N1.Q05))
Ni.5 = c(which(data$subjno==N0.Q5),which(data$subjno==N1.Q5))
Ni.95 = c(which(data$subjno==N0.Q95),which(data$subjno==N1.Q95))

data.IL.Q05 = data.frame(cbind(data[Ni.05,],Time=T.list))
data.IL.Q5 = data.frame(cbind(data[Ni.5,],Time=T.list))
data.IL.Q95 = data.frame(cbind(data[Ni.95,],Time=T.list))

Y.pool = rbind(data.IL.Q05,data.IL.Q5,data.IL.Q95)$Y
Y.IL.min = min(Y.pool)
Y.IL.max = max(Y.pool)
}

########################################################################################

# Simulate data from Model 7: Y ~ b00 + b01*W + b10*X + b11*W*X

if (Model == 7){

# Monte Carlo replicates

plan(multisession)
fit = future_lapply(1:length(N), function(i){
message(paste('Estimating Power for N =',N[i]))
cmp.Summary.model.IL(Model, N[i], N.0, N.1, T, 
isX.center, Ylag.center, isW.center, alpha, 
b00, b01.Z, b01.W, b10, b11.Z, b11.W, sigma, rho, sigma.v0, sigma.v1,rho.v, 
mu.W, sigma.W, mu.X, mu.X0, mu.X1, sigma.X, sigma.X0, sigma.X1,R,Opt.Method,is.rho.zero)},future.seed = 0xBEEF)

# Distribution of the parameters: fixed effects 

b00.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[2]]))
b00.se = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[3]]))
b00.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[4]]))
CI.b00 = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[5]]))
power.b00 = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[6]]))

b01.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[2]][[2]]))
b01.se = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[2]][[3]]))
b01.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[2]][[4]]))
CI.b01 = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[2]][[5]]))
power.b01 = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[2]][[6]]))

b10.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[3]][[2]]))
b10.se = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[3]][[3]]))
b10.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[3]][[4]]))
CI.b10 = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[3]][[5]]))
power.b10 = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[3]][[6]]))

b11.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[4]][[2]]))
b11.se = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[4]][[3]]))
b11.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[4]][[4]]))
CI.b11 = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[4]][[5]]))
power.b11 = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[4]][[6]]))

# Table fixed fredictors 

coef.names.b00 = rep(0,length(N))
coef.names.b01 = rep(0,length(N))
coef.names.b10 = rep(0,length(N))
coef.names.b11 = rep(0,length(N))

for (i in 1:length(N)){
coef.names.b00[i] = rep(paste('Fixed intercept','N',N[i]))
coef.names.b01[i] = rep(paste('Effect of the level-2 continuous variable on the intercept','N',N[i]))
coef.names.b10[i] = rep(paste('Fixed slope','N',N[i]))
coef.names.b11[i] = rep(paste('Effect of the level-2 continuous variable on the slope','N',N[i]))
}

coef.sim = data.frame(round(rbind(cbind(rep(b00,length(N)),b00.mean,b00.se,b00.bias,CI.b00,power.b00),
cbind(rep(b01.W,length(N)),b01.mean,b01.se,b01.bias,CI.b01,power.b01),
cbind(rep(b10,length(N)),b10.mean,b10.se,b10.bias,CI.b10,power.b10),
cbind(rep(b11.W,length(N)),b11.mean,b11.se,b11.bias,CI.b11,power.b11)),digits=4))
rownames(coef.sim) = c(coef.names.b00,coef.names.b01,coef.names.b10,coef.names.b11)
colnames(coef.sim) = c('True value','Mean','Std.error','Bias','(1-alpha)% Coverage','Power')

# Distribution of the parameters: random effects

if (is.rho.zero==TRUE){
sigma.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[1]][[2]]))
sigma.se = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[3]]))
sigma.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[4]]))

rho.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[2]][[2]]))
rho.se = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[2]][[3]]))
rho.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[2]][[4]]))

sigma.v0.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[3]][[2]]))
sigma.v0.se = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[3]][[3]]))
sigma.v0.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[3]][[4]]))

sigma.v1.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[4]][[2]]))
sigma.v1.se = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[4]][[3]]))
sigma.v1.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[4]][[4]]))

rho.v.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[5]][[2]]))
rho.v.se = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[5]][[3]]))
rho.v.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[5]][[4]]))

# Table random effects 

coef.names.sigma = rep(0,length(N))
coef.names.rho = rep(0,length(N))
coef.names.sigma.v0 = rep(0,length(N))
coef.names.sigma.v1 = rep(0,length(N))
coef.names.rho.v = rep(0,length(N))

for (i in 1:length(N)){
coef.names.sigma[i] = rep(paste('Standard deviation of the level-1 error','N',N[i]))
coef.names.rho[i] = rep(paste('Autocorrelation of the level-1 error','N',N[i]))
coef.names.sigma.v0[i] = rep(paste('Standard deviation of the random intercept','N',N[i]))
coef.names.sigma.v1[i] = rep(paste('Standard deviation of the random slope','N',N[i]))
coef.names.rho.v[i] = rep(paste('Correlation between the random intercept and the random slope','N',N[i]))
}

cov.sim = data.frame(round(rbind(cbind(rep(sigma,length(N)),sigma.mean,sigma.se,sigma.bias),
cbind(rep(rho,length(N)),rho.mean,rho.se,rho.bias),
cbind(rep(sigma.v0,length(N)),sigma.v0.mean,sigma.v0.se,sigma.v0.bias),
cbind(rep(sigma.v1,length(N)),sigma.v1.mean,sigma.v1.se,sigma.v1.bias),
cbind(rep(rho.v,length(N)),rho.v.mean,rho.v.se,rho.v.bias)),digits=4))
rownames(cov.sim) = c(coef.names.sigma,coef.names.rho,coef.names.sigma.v0,coef.names.sigma.v1,coef.names.rho.v)
colnames(cov.sim) = c('True value','Mean','Std.error','Bias')
}

if (is.rho.zero==FALSE){
sigma.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[1]][[2]]))
sigma.se = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[3]]))
sigma.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[4]]))

sigma.v0.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[2]][[2]]))
sigma.v0.se = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[2]][[3]]))
sigma.v0.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[2]][[4]]))

sigma.v1.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[3]][[2]]))
sigma.v1.se = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[3]][[3]]))
sigma.v1.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[3]][[4]]))

rho.v.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[4]][[2]]))
rho.v.se = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[4]][[3]]))
rho.v.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[4]][[4]]))

# Table random effects 

coef.names.sigma = rep(0,length(N))
coef.names.sigma.v0 = rep(0,length(N))
coef.names.sigma.v1 = rep(0,length(N))
coef.names.rho.v = rep(0,length(N))

for (i in 1:length(N)){
coef.names.sigma[i] = rep(paste('Standard deviation of the level-1 error','N',N[i]))
coef.names.sigma.v0[i] = rep(paste('Standard deviation of the random intercept','N',N[i]))
coef.names.sigma.v1[i] = rep(paste('Standard deviation of the random slope','N',N[i]))
coef.names.rho.v[i] = rep(paste('Correlation between the random intercept and the random slope','N',N[i]))
}

cov.sim = data.frame(round(rbind(cbind(rep(sigma,length(N)),sigma.mean,sigma.se,sigma.bias),
cbind(rep(sigma.v0,length(N)),sigma.v0.mean,sigma.v0.se,sigma.v0.bias),
cbind(rep(sigma.v1,length(N)),sigma.v1.mean,sigma.v1.se,sigma.v1.bias),
cbind(rep(rho.v,length(N)),rho.v.mean,rho.v.se,rho.v.bias)),digits=4))
rownames(cov.sim) = c(coef.names.sigma,coef.names.sigma.v0,coef.names.sigma.v1,coef.names.rho.v)
colnames(cov.sim) = c('True value','Mean','Std.error','Bias')
}

# Power (for the power curve!)

power.curve = cbind(N,power.b00,power.b01,power.b10,power.b11)

# Parameters distribution across the R Monte Carlo replicates for maximum N

N.max = which.max(N)

if (is.rho.zero==TRUE){
b00.sim = fit[[N.max]][[1]][[1]][[1]]
b01.sim = fit[[N.max]][[1]][[2]][[1]]
b10.sim = fit[[N.max]][[1]][[3]][[1]]
b11.sim = fit[[N.max]][[1]][[4]][[1]]
sigma.sim = fit[[N.max]][[2]][[1]][[1]]
rho.sim = fit[[N.max]][[2]][[2]][[1]]
sigma.v0.sim = fit[[N.max]][[2]][[3]][[1]]
sigma.v1.sim = fit[[N.max]][[2]][[3]][[1]]
rho.v.sim = fit[[N.max]][[2]][[4]][[1]]

b.sim = data.frame(cbind(b00.sim,b01.sim,b10.sim,b11.sim,rho.sim,sigma.sim,sigma.v0.sim,sigma.v1.sim,rho.v.sim))
colnames(b.sim) = c('b00.hat','b01.hat','b10.hat','b11.hat','rho.hat','sigma.hat','sigma.v0.hat','sigma.v1.hat','rho.v.hat')
}

if (is.rho.zero==FALSE){
b00.sim = fit[[N.max]][[1]][[1]][[1]]
b01.sim = fit[[N.max]][[1]][[2]][[1]]
b10.sim = fit[[N.max]][[1]][[3]][[1]]
b11.sim = fit[[N.max]][[1]][[4]][[1]]
sigma.sim = fit[[N.max]][[2]][[1]][[1]]
sigma.v0.sim = fit[[N.max]][[2]][[2]][[1]]
sigma.v1.sim = fit[[N.max]][[2]][[3]][[1]]
rho.v.sim = fit[[N.max]][[2]][[4]][[1]]

b.sim = data.frame(cbind(b00.sim,b01.sim,b10.sim,b11.sim,sigma.sim,sigma.v0.sim,sigma.v1.sim,rho.v.sim))
colnames(b.sim) = c('b00.hat','b01.hat','b10.hat','b11.hat','sigma.hat','sigma.v0.hat','sigma.v1.hat','rho.v.hat')
}

# Trajectories of Y for one replicate

data = Sim.Data.IL(Model,N[N.max],N.0, N.1, T,isX.center,Ylag.center,isW.center, 
b00, b01.Z, b01.W, b10, b11.Z, b11.W, sigma, rho, sigma.v0, sigma.v1, 
rho.v, mu.W, sigma.W, mu.X, mu.X0, mu.X1, sigma.X, sigma.X0, sigma.X1)

Ymean = aggregate(data, by=list(data$subjno),FUN=mean, na.rm=TRUE)

Q.05 = quantile(Ymean$Y,probs=0.05)
N.Q05 = Ymean$subjno[which.max(Ymean$Y<=Q.05)]

Q.5 = quantile(Ymean$Y,probs=0.5)
N.Q5 = Ymean$subjno[which.max(Ymean$Y<=Q.5)]

Q.95 = quantile(Ymean$Y,probs=0.95)
N.Q95 = Ymean$subjno[which.max(Ymean$Y>=Q.95)]

T.list = rep(1:T,1)
Ni.05 = which(data$subjno==N.Q05)
Ni.5 = which(data$subjno==N.Q5)
Ni.95 = which(data$subjno==N.Q95)

data.IL.Q05 = data.frame(cbind(data[Ni.05,],Time=T.list))
data.IL.Q5 = data.frame(cbind(data[Ni.5,],Time=T.list))
data.IL.Q95 = data.frame(cbind(data[Ni.95,],Time=T.list))

Y.pool = rbind(data.IL.Q05,data.IL.Q5,data.IL.Q95)$Y
Y.IL.min = min(Y.pool)
Y.IL.max = max(Y.pool)
}

########################################################################################

# Simulate data from Model 8: Y ~ b00 + b01*W + b10*X + b11*W*X with fixed slope

if (Model == 8){

# Monte Carlo replicates

plan(multisession)
fit = future_lapply(1:length(N), function(i){
message(paste('Estimating Power for N =',N[i]))
cmp.Summary.model.IL(Model, N[i], N.0, N.1, T, 
isX.center, Ylag.center, isW.center, alpha, 
b00, b01.Z, b01.W, b10, b11.Z, b11.W, sigma, rho, sigma.v0, sigma.v1,rho.v, 
mu.W, sigma.W, mu.X, mu.X0, mu.X1, sigma.X, sigma.X0, sigma.X1,R,Opt.Method,is.rho.zero)},future.seed = 0xBEEF)

# Distribution of the parameters: fixed effects 

b00.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[2]]))
b00.se = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[3]]))
b00.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[4]]))
CI.b00 = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[5]]))
power.b00 = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[6]]))

b01.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[2]][[2]]))
b01.se = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[2]][[3]]))
b01.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[2]][[4]]))
CI.b01 = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[2]][[5]]))
power.b01 = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[2]][[6]]))

b10.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[3]][[2]]))
b10.se = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[3]][[3]]))
b10.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[3]][[4]]))
CI.b10 = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[3]][[5]]))
power.b10 = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[3]][[6]]))

b11.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[4]][[2]]))
b11.se = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[4]][[3]]))
b11.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[4]][[4]]))
CI.b11 = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[4]][[5]]))
power.b11 = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[4]][[6]]))

# Table fixed fredictors 

coef.names.b00 = rep(0,length(N))
coef.names.b01 = rep(0,length(N))
coef.names.b10 = rep(0,length(N))
coef.names.b11 = rep(0,length(N))

for (i in 1:length(N)){
coef.names.b00[i] = rep(paste('Fixed intercept','N',N[i]))
coef.names.b01[i] = rep(paste('Effect of the level-2 continuous variable on the intercept','N',N[i]))
coef.names.b10[i] = rep(paste('Fixed slope','N',N[i]))
coef.names.b11[i] = rep(paste('Effect of the level-2 continuous variable on the slope','N',N[i]))
}

coef.sim = data.frame(round(rbind(cbind(rep(b00,length(N)),b00.mean,b00.se,b00.bias,CI.b00,power.b00),
cbind(rep(b01.W,length(N)),b01.mean,b01.se,b01.bias,CI.b01,power.b01),
cbind(rep(b10,length(N)),b10.mean,b10.se,b10.bias,CI.b10,power.b10),
cbind(rep(b11.W,length(N)),b11.mean,b11.se,b11.bias,CI.b11,power.b11)),digits=4))
rownames(coef.sim) = c(coef.names.b00,coef.names.b01,coef.names.b10,coef.names.b11)
colnames(coef.sim) = c('True value','Mean','Std.error','Bias','(1-alpha)% Coverage','Power')

# Distribution of the parameters: random effects

if (is.rho.zero==TRUE){
sigma.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[1]][[2]]))
sigma.se = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[3]]))
sigma.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[4]]))

rho.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[2]][[2]]))
rho.se = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[2]][[3]]))
rho.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[2]][[4]]))

sigma.v0.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[3]][[2]]))
sigma.v0.se = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[3]][[3]]))
sigma.v0.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[3]][[4]]))

# Table random effects 

coef.names.sigma = rep(0,length(N))
coef.names.rho = rep(0,length(N))
coef.names.sigma.v0 = rep(0,length(N))

for (i in 1:length(N)){
coef.names.sigma[i] = rep(paste('Standard deviation of the level-1 error','N',N[i]))
coef.names.rho[i] = rep(paste('Autocorrelation of the level-1 error','N',N[i]))
coef.names.sigma.v0[i] = rep(paste('Standard deviation of the random intercept','N',N[i]))
}

cov.sim = data.frame(round(rbind(cbind(rep(sigma,length(N)),sigma.mean,sigma.se,sigma.bias),
cbind(rep(rho,length(N)),rho.mean,rho.se,rho.bias),
cbind(rep(sigma.v0,length(N)),sigma.v0.mean,sigma.v0.se,sigma.v0.bias)),digits=4))
rownames(cov.sim) = c(coef.names.sigma,coef.names.rho,coef.names.sigma.v0)
colnames(cov.sim) = c('True value','Mean','Std.error','Bias')
}

if (is.rho.zero==FALSE){
sigma.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[1]][[2]]))
sigma.se = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[3]]))
sigma.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[4]]))

sigma.v0.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[2]][[2]]))
sigma.v0.se = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[2]][[3]]))
sigma.v0.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[2]][[4]]))

# Table random effects 

coef.names.sigma = rep(0,length(N))
coef.names.sigma.v0 = rep(0,length(N))

for (i in 1:length(N)){
coef.names.sigma[i] = rep(paste('Standard deviation of the level-1 error','N',N[i]))
coef.names.sigma.v0[i] = rep(paste('Standard deviation of the random intercept','N',N[i]))
}

cov.sim = data.frame(round(rbind(cbind(rep(sigma,length(N)),sigma.mean,sigma.se,sigma.bias),
cbind(rep(sigma.v0,length(N)),sigma.v0.mean,sigma.v0.se,sigma.v0.bias)),digits=4))
rownames(cov.sim) = c(coef.names.sigma,coef.names.sigma.v0)
colnames(cov.sim) = c('True value','Mean','Std.error','Bias')
}

# Power (for the power curve!)

power.curve = cbind(N,power.b00,power.b01,power.b10,power.b11)

# Parameters distribution across the R Monte Carlo replicates for maximum N

N.max = which.max(N)

if (is.rho.zero==TRUE){
b00.sim = fit[[N.max]][[1]][[1]][[1]]
b01.sim = fit[[N.max]][[1]][[2]][[1]]
b10.sim = fit[[N.max]][[1]][[3]][[1]]
b11.sim = fit[[N.max]][[1]][[4]][[1]]
sigma.sim = fit[[N.max]][[2]][[1]][[1]]
rho.sim = fit[[N.max]][[2]][[2]][[1]]
sigma.v0.sim = fit[[N.max]][[2]][[3]][[1]]

b.sim = data.frame(cbind(b00.sim,b01.sim,b10.sim,b11.sim,rho.sim,sigma.sim,sigma.v0.sim))
colnames(b.sim) = c('b00.hat','b01.hat','b10.hat','b11.hat','rho.hat','sigma.hat','sigma.v0.hat')
}

if (is.rho.zero==FALSE){
b00.sim = fit[[N.max]][[1]][[1]][[1]]
b01.sim = fit[[N.max]][[1]][[2]][[1]]
b10.sim = fit[[N.max]][[1]][[3]][[1]]
b11.sim = fit[[N.max]][[1]][[4]][[1]]
sigma.sim = fit[[N.max]][[2]][[1]][[1]]
sigma.v0.sim = fit[[N.max]][[2]][[2]][[1]]

b.sim = data.frame(cbind(b00.sim,b01.sim,b10.sim,b11.sim,sigma.sim,sigma.v0.sim))
colnames(b.sim) = c('b00.hat','b01.hat','b10.hat','b11.hat','sigma.hat','sigma.v0.hat')
}

# Trajectories of Y for one replicate

data = Sim.Data.IL(Model,N[N.max],N.0, N.1, T,isX.center,Ylag.center,isW.center, 
b00, b01.Z, b01.W, b10, b11.Z, b11.W, sigma, rho, sigma.v0, sigma.v1, 
rho.v, mu.W, sigma.W, mu.X, mu.X0, mu.X1, sigma.X, sigma.X0, sigma.X1)

Ymean = aggregate(data, by=list(data$subjno),FUN=mean, na.rm=TRUE)

Q.05 = quantile(Ymean$Y,probs=0.05)
N.Q05 = Ymean$subjno[which.max(Ymean$Y<=Q.05)]

Q.5 = quantile(Ymean$Y,probs=0.5)
N.Q5 = Ymean$subjno[which.max(Ymean$Y<=Q.5)]

Q.95 = quantile(Ymean$Y,probs=0.95)
N.Q95 = Ymean$subjno[which.max(Ymean$Y>=Q.95)]

T.list = rep(1:T,1)
Ni.05 = which(data$subjno==N.Q05)
Ni.5 = which(data$subjno==N.Q5)
Ni.95 = which(data$subjno==N.Q95)

data.IL.Q05 = data.frame(cbind(data[Ni.05,],Time=T.list))
data.IL.Q5 = data.frame(cbind(data[Ni.5,],Time=T.list))
data.IL.Q95 = data.frame(cbind(data[Ni.95,],Time=T.list))

Y.pool = rbind(data.IL.Q05,data.IL.Q5,data.IL.Q95)$Y
Y.IL.min = min(Y.pool)
Y.IL.max = max(Y.pool)
}

########################################################################################

# Simulate data from Model 9: Y ~ b00 + b10*lag(Y)

if (Model == 9){

# Monte Carlo replicates

plan(multisession)
fit = future_lapply(1:length(N), function(i){
message(paste('Estimating Power for N =',N[i]))
cmp.Summary.model.IL(Model, N[i], N.0, N.1, T, 
isX.center, Ylag.center, isW.center, alpha, 
b00, b01.Z, b01.W, b10, b11.Z, b11.W, sigma, rho, sigma.v0, sigma.v1,rho.v, 
mu.W, sigma.W, mu.X, mu.X0, mu.X1, sigma.X, sigma.X0, sigma.X1,R,Opt.Method,is.rho.zero)},future.seed = 0xBEEF)

# Distribution of the parameters: random effects 

b00.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[2]]))
b00.se = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[3]]))
b00.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[4]]))
CI.b00 = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[5]]))
power.b00 = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[6]]))

b10.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[2]][[2]]))
b10.se = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[2]][[3]]))
b10.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[2]][[4]]))
CI.b10 = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[2]][[5]]))
power.b10 = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[2]][[6]]))

# Table fixed effects

coef.names.b00 = rep(0,length(N))
coef.names.b10 = rep(0,length(N))

for (i in 1:length(N)){
coef.names.b00[i] = rep(paste('Fixed intercept','N',N[i]))
coef.names.b10[i] = rep(paste('Fixed slope','N',N[i]))
}

coef.sim = data.frame(round(rbind(cbind(rep(b00,length(N)),b00.mean,b00.se,b00.bias,CI.b00,power.b00),
cbind(rep(b10,length(N)),b10.mean,b10.se,b10.bias,CI.b10,power.b10)),digits=4))
rownames(coef.sim) = c(coef.names.b00,coef.names.b10)
colnames(coef.sim) = c('True value','Mean','Std.error','Bias','(1-alpha)% Coverage','Power')

# Distribution of the parameters: random effects

sigma.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[1]][[2]]))
sigma.se = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[3]]))
sigma.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[4]]))

sigma.v0.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[2]][[2]]))
sigma.v0.se = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[2]][[3]]))
sigma.v0.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[2]][[4]]))

sigma.v1.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[3]][[2]]))
sigma.v1.se = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[3]][[3]]))
sigma.v1.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[3]][[4]]))

rho.v.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[4]][[2]]))
rho.v.se = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[4]][[3]]))
rho.v.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[4]][[4]]))

# Table random effects 

coef.names.sigma = rep(0,length(N))
coef.names.sigma.v0 = rep(0,length(N))
coef.names.sigma.v1 = rep(0,length(N))
coef.names.rho.v = rep(0,length(N))

for (i in 1:length(N)){
coef.names.sigma[i] = rep(paste('Standard deviation of the level-1 error','N',N[i]))
coef.names.sigma.v0[i] = rep(paste('Standard deviation of the random intercept','N',N[i]))
coef.names.sigma.v1[i] = rep(paste('Standard deviation of the random slope','N',N[i]))
coef.names.rho.v[i] = rep(paste('Correlation between the random intercept and the random slope','N',N[i]))
}

cov.sim = data.frame(round(rbind(cbind(rep(sigma,length(N)),sigma.mean,sigma.se,sigma.bias),
cbind(rep(sigma.v0,length(N)),sigma.v0.mean,sigma.v0.se,sigma.v0.bias),
cbind(rep(sigma.v1,length(N)),sigma.v1.mean,sigma.v1.se,sigma.v1.bias),
cbind(rep(rho.v,length(N)),rho.v.mean,rho.v.se,rho.v.bias)),digits=4))
rownames(cov.sim) = c(coef.names.sigma,coef.names.sigma.v0,coef.names.sigma.v1,coef.names.rho.v)
colnames(cov.sim) = c('True value','Mean','Std.error','Bias')


# Power (for the power curve!)

power.curve = cbind(N,power.b00,power.b10)

# Parameters distribution across the R Monte Carlo replicates for maximum N

N.max = which.max(N)

b00.sim = fit[[N.max]][[1]][[1]][[1]]
b10.sim = fit[[N.max]][[1]][[2]][[1]]
sigma.sim = fit[[N.max]][[2]][[1]][[1]]
sigma.v0.sim = fit[[N.max]][[2]][[2]][[1]]
sigma.v1.sim = fit[[N.max]][[2]][[3]][[1]]
rho.v.sim = fit[[N.max]][[2]][[4]][[1]]

b.sim = data.frame(cbind(b00.sim,b10.sim,sigma.sim,sigma.v0.sim,sigma.v1.sim,rho.v.sim))
colnames(b.sim) = c('b00.hat','b10.hat','sigma.hat','sigma.v0.hat','sigma.v1.hat','rho.v.hat')

# Trajectories of Y for one replicate

data = Sim.Data.IL(Model,N[N.max],N.0, N.1, T,isX.center,Ylag.center,isW.center, 
b00, b01.Z, b01.W, b10, b11.Z, b11.W, sigma, rho, sigma.v0, sigma.v1, 
rho.v, mu.W, sigma.W, mu.X, mu.X0, mu.X1, sigma.X, sigma.X0, sigma.X1)

Ymean = aggregate(data, by=list(data$subjno),FUN=mean, na.rm=TRUE)

Q.05 = quantile(Ymean$Y,probs=0.05)
N.Q05 = Ymean$subjno[which.max(Ymean$Y<=Q.05)]

Q.5 = quantile(Ymean$Y,probs=0.5)
N.Q5 = Ymean$subjno[which.max(Ymean$Y<=Q.5)]

Q.95 = quantile(Ymean$Y,probs=0.95)
N.Q95 = Ymean$subjno[which.max(Ymean$Y>=Q.95)]

T.list = rep(1:T,1)
Ni.05 = which(data$subjno==N.Q05)
Ni.5 = which(data$subjno==N.Q5)
Ni.95 = which(data$subjno==N.Q95)

data.IL.Q05 = data.frame(cbind(data[Ni.05,],Time=T.list))
data.IL.Q5 = data.frame(cbind(data[Ni.5,],Time=T.list))
data.IL.Q95 = data.frame(cbind(data[Ni.95,],Time=T.list))

Y.pool = rbind(data.IL.Q05,data.IL.Q5,data.IL.Q95)$Y
Y.IL.min = min(Y.pool)
Y.IL.max = max(Y.pool)
}

########################################################################################

# Simulate data from Model 10: Y ~ b00 + b01*Z + + b10*lag(Y) + + b11*Z*lag(Y)

if (Model == 10){

# Monte Carlo replicates

plan(multisession)
fit = future_lapply(1:length(N.0), function(i){
message(paste('Estimating Power for N.0 =',N.0[i],'and N.1 =',N.1[i]))
cmp.Summary.model.IL(Model, N, N.0[i], N.1[i], T, 
isX.center, Ylag.center, isW.center, alpha, 
b00, b01.Z, b01.W, b10, b11.Z, b11.W, sigma, rho, sigma.v0, sigma.v1,rho.v, 
mu.W, sigma.W, mu.X, mu.X0, mu.X1, sigma.X, sigma.X0, sigma.X1,R,Opt.Method,is.rho.zero)},future.seed = 0xBEEF)

# Distribution of the parameters: fixed effects 

b00.mean = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[1]][[2]]))
b00.se = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[1]][[3]]))
b00.bias = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[1]][[4]]))
CI.b00 = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[1]][[5]]))
power.b00 = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[1]][[6]]))

b01.mean = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[2]][[2]]))
b01.se = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[2]][[3]]))
b01.bias = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[2]][[4]]))
CI.b01 = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[2]][[5]]))
power.b01 = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[2]][[6]]))

b10.mean = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[3]][[2]]))
b10.se = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[3]][[3]]))
b10.bias = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[3]][[4]]))
CI.b10 = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[3]][[5]]))
power.b10 = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[3]][[6]]))

b11.mean = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[4]][[2]]))
b11.se = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[4]][[3]]))
b11.bias = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[4]][[4]]))
CI.b11 = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[4]][[5]]))
power.b11 = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[4]][[6]]))

# Table fixed fredictors 

coef.names.b00 = rep(0,length(N.0))
coef.names.b01 = rep(0,length(N.0))
coef.names.b10 = rep(0,length(N.0))
coef.names.b11 = rep(0,length(N.0))

for (i in 1:length(N.0)){
coef.names.b00[i] = rep(paste('Fixed intercept','N(Group=1)',N.0[i],'N(Group=0)',N.1[i]))
coef.names.b01[i] = rep(paste('Effect of the level-2 dummy variable on the intercept','N(Group=1)',N.0[i],'N(Group=0)',N.1[i]))
coef.names.b10[i] = rep(paste('Fixed slope','N(Group=1)',N.0[i],'N(Group=0)',N.1[i]))
coef.names.b11[i] = rep(paste('Effect of the level-2 dummy variable on the slope','N(Group=1)',N.0[i],'N(Group=0)',N.1[i]))
}

coef.sim = data.frame(round(rbind(cbind(rep(b00,length(N.0)),b00.mean,b00.se,b00.bias,CI.b00,power.b00),
cbind(rep(b01.Z,length(N.0)),b01.mean,b01.se,b01.bias,CI.b01,power.b01),
cbind(rep(b10,length(N.0)),b10.mean,b10.se,b10.bias,CI.b10,power.b10),
cbind(rep(b11.Z,length(N.0)),b11.mean,b11.se,b11.bias,CI.b11,power.b11)),digits=4))
rownames(coef.sim) = c(coef.names.b00,coef.names.b01,coef.names.b10,coef.names.b11)
colnames(coef.sim) = c('True value','Mean','Std.error','Bias','(1-alpha)% Coverage','Power')

# Distribution of the parameters: random effects

sigma.mean = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[1]][[2]]))
sigma.se = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[1]][[3]]))
sigma.bias = unlist(lapply(1:length(N.0), function(i) fit[[i]][[1]][[1]][[4]]))

sigma.v0.mean = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[2]][[2]]))
sigma.v0.se = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[2]][[3]]))
sigma.v0.bias = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[2]][[4]]))

sigma.v1.mean = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[3]][[2]]))
sigma.v1.se = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[3]][[3]]))
sigma.v1.bias = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[3]][[4]]))

rho.v.mean = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[4]][[2]]))
rho.v.se = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[4]][[3]]))
rho.v.bias = unlist(lapply(1:length(N.0), function(i) fit[[i]][[2]][[4]][[4]]))

# Table random effects 

coef.names.sigma = rep(0,length(N.0))
coef.names.sigma.v0 = rep(0,length(N.0))
coef.names.sigma.v1 = rep(0,length(N.0))
coef.names.rho.v = rep(0,length(N.0))

for (i in 1:length(N.0)){
coef.names.sigma[i] = rep(paste('Standard deviation of the level-1 error','N(Group=1)',N.0[i],'N(Group=0)',N.1[i]))
coef.names.sigma.v0[i] = rep(paste('Standard deviation of the random intercept','N(Group=1)',N.0[i],'N(Group=0)',N.1[i]))
coef.names.sigma.v1[i] = rep(paste('Standard deviation of the random slope','N(Group=1)',N.0[i],'N(Group=0)',N.1[i]))
coef.names.rho.v[i] = rep(paste('Correlation between the random intercept and the random slope','N(Group=1)',N.0[i],'N(Group=0)',N.1[i]))
}

cov.sim = data.frame(round(rbind(cbind(rep(sigma,length(N.0)),sigma.mean,sigma.se,sigma.bias),
cbind(rep(sigma.v0,length(N.0)),sigma.v0.mean,sigma.v0.se,sigma.v0.bias),
cbind(rep(sigma.v1,length(N.0)),sigma.v1.mean,sigma.v1.se,sigma.v1.bias),
cbind(rep(rho.v,length(N.0)),rho.v.mean,rho.v.se,rho.v.bias)),digits=4))
rownames(cov.sim) = c(coef.names.sigma,coef.names.sigma.v0,coef.names.sigma.v1,coef.names.rho.v)
colnames(cov.sim) = c('True value','Mean','Std.error','Bias')

# Power (for the power curve!)

power.curve = cbind(N.0,N.1,power.b00,power.b01,power.b10,power.b11)

# Parameters distribution across the R Monte Carlo replicates for maximum N

N = cbind(N.0,N.1)
N.0.max = max(N.0)
N.1.max = max(N.1)
N.max = which.max(N[,which.max(c(N.0.max,N.1.max))])

b00.sim = fit[[N.max]][[1]][[1]][[1]]
b01.sim = fit[[N.max]][[1]][[2]][[1]]
b10.sim = fit[[N.max]][[1]][[3]][[1]]
b11.sim = fit[[N.max]][[1]][[4]][[1]]
sigma.sim = fit[[N.max]][[2]][[1]][[1]]
sigma.v0.sim = fit[[N.max]][[2]][[2]][[1]]
sigma.v1.sim = fit[[N.max]][[2]][[3]][[1]]
rho.v.sim = fit[[N.max]][[2]][[4]][[1]]

b.sim = data.frame(cbind(b00.sim,b01.sim,b10.sim,b11.sim,sigma.sim,sigma.v0.sim,sigma.v1.sim,rho.v.sim))
colnames(b.sim) = c('b00.hat','b01.hat','b10.hat','b11.hat','sigma.hat','sigma.v0.hat','sigma.v1.hat','rho.v.hat')


# Trajectories of Y for one replicate

data = Sim.Data.IL(Model,N,N.0[N.max], N.1[N.max], T,isX.center,Ylag.center,isW.center, 
b00, b01.Z, b01.W, b10, b11.Z, b11.W, sigma, rho, sigma.v0, sigma.v1, 
rho.v, mu.W, sigma.W, mu.X, mu.X0, mu.X1, sigma.X, sigma.X0, sigma.X1)

Ymean = aggregate(data, by=list(data$Z,data$subjno),FUN=mean, na.rm=TRUE)

Y0mean = Ymean[which(Ymean$Z==0),]

Q0.05 = quantile(Y0mean$Y[which(Y0mean$Z==0)],probs=0.05)
N0.Q05 = Y0mean$subjno[which.max(Y0mean$Y[which(Y0mean$Z==0)]<=Q0.05)]

Q0.5 = quantile(Y0mean$Y[which(Y0mean$Z==0)],probs=0.5)
N0.Q5 = Y0mean$subjno[which.max(Y0mean$Y[which(Y0mean$Z==0)]<=Q0.5)]

Q0.95 = quantile(Y0mean$Y[which(Y0mean$Z==0)],probs=0.95)
N0.Q95 = Y0mean$subjno[which.max(Y0mean$Y[which(Y0mean$Z==0)]>=Q0.95)]

Y1mean = Ymean[which(Ymean$Z==1),]

Q1.05 = quantile(Y1mean$Y[which(Y1mean$Z==1)],probs=0.05)
N1.Q05 = Y1mean$subjno[which.max(Y1mean$Y[which(Y1mean$Z==1)]<=Q1.05)]

Q1.5 = quantile(Y1mean$Y[which(Y1mean$Z==1)],probs=0.5)
N1.Q5 = Y1mean$subjno[which.max(Y1mean$Y[which(Y1mean$Z==1)]<=Q1.5)]

Q1.95 = quantile(Y1mean$Y[which(Y1mean$Z==1)],probs=0.95)
N1.Q95 = Y1mean$subjno[which.max(Y1mean$Y[which(Y1mean$Z==1)]>=Q1.95)]

T.list = rep(1:T,2)
data$Z = as.factor(data$Z)
Ni.05 = c(which(data$subjno==N0.Q05),which(data$subjno==N1.Q05))
Ni.5 = c(which(data$subjno==N0.Q5),which(data$subjno==N1.Q5))
Ni.95 = c(which(data$subjno==N0.Q95),which(data$subjno==N1.Q95))

data.IL.Q05 = data.frame(cbind(data[Ni.05,],Time=T.list))
data.IL.Q5 = data.frame(cbind(data[Ni.5,],Time=T.list))
data.IL.Q95 = data.frame(cbind(data[Ni.95,],Time=T.list))

Y.pool = rbind(data.IL.Q05,data.IL.Q5,data.IL.Q95)$Y
Y.IL.min = min(Y.pool)
Y.IL.max = max(Y.pool)
}

########################################################################################

# Simulate data from Model 11: Y ~ b00 + b01*W+ + b10*lag(Y) + + b11*W*lag(Y)

if (Model == 11){

# Monte Carlo replicates

plan(multisession)
fit = future_lapply(1:length(N), function(i){
message(paste('Estimating Power for N =',N[i]))
cmp.Summary.model.IL(Model, N[i], N.0, N.1, T, 
isX.center, Ylag.center, isW.center, alpha, 
b00, b01.Z, b01.W, b10, b11.Z, b11.W, sigma, rho, sigma.v0, sigma.v1,rho.v, 
mu.W, sigma.W, mu.X, mu.X0, mu.X1, sigma.X, sigma.X0, sigma.X1,R,Opt.Method,is.rho.zero)},future.seed = 0xBEEF)

# Distribution of the parameters: fixed effects 

b00.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[2]]))
b00.se = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[3]]))
b00.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[4]]))
CI.b00 = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[5]]))
power.b00 = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[6]]))

b01.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[2]][[2]]))
b01.se = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[2]][[3]]))
b01.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[2]][[4]]))
CI.b01 = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[2]][[5]]))
power.b01 = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[2]][[6]]))

b10.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[3]][[2]]))
b10.se = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[3]][[3]]))
b10.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[3]][[4]]))
CI.b10 = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[3]][[5]]))
power.b10 = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[3]][[6]]))

b11.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[4]][[2]]))
b11.se = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[4]][[3]]))
b11.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[4]][[4]]))
CI.b11 = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[4]][[5]]))
power.b11 = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[4]][[6]]))

# Table fixed fredictors 

coef.names.b00 = rep(0,length(N))
coef.names.b01 = rep(0,length(N))
coef.names.b10 = rep(0,length(N))
coef.names.b11 = rep(0,length(N))

for (i in 1:length(N)){
coef.names.b00[i] = rep(paste('Fixed intercept','N',N[i]))
coef.names.b01[i] = rep(paste('Effect of the level-2 continuous variable on the intercept','N',N[i]))
coef.names.b10[i] = rep(paste('Fixed slope','N',N[i]))
coef.names.b11[i] = rep(paste('Effect of the level-2 continuous variable on the slope','N',N[i]))
}

coef.sim = data.frame(round(rbind(cbind(rep(b00,length(N)),b00.mean,b00.se,b00.bias,CI.b00,power.b00),
cbind(rep(b01.W,length(N)),b01.mean,b01.se,b01.bias,CI.b01,power.b01),
cbind(rep(b10,length(N)),b10.mean,b10.se,b10.bias,CI.b10,power.b10),
cbind(rep(b11.W,length(N)),b11.mean,b11.se,b11.bias,CI.b11,power.b11)),digits=4))
rownames(coef.sim) = c(coef.names.b00,coef.names.b01,coef.names.b10,coef.names.b11)
colnames(coef.sim) = c('True value','Mean','Std.error','Bias','(1-alpha)% Coverage','Power')

# Distribution of the parameters: random effects

sigma.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[1]][[2]]))
sigma.se = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[3]]))
sigma.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[1]][[1]][[4]]))

sigma.v0.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[2]][[2]]))
sigma.v0.se = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[2]][[3]]))
sigma.v0.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[2]][[4]]))

sigma.v1.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[3]][[2]]))
sigma.v1.se = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[3]][[3]]))
sigma.v1.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[3]][[4]]))

rho.v.mean = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[4]][[2]]))
rho.v.se = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[4]][[3]]))
rho.v.bias = unlist(lapply(1:length(N), function(i) fit[[i]][[2]][[4]][[4]]))

# Table random effects 

coef.names.sigma = rep(0,length(N))
coef.names.sigma.v0 = rep(0,length(N))
coef.names.sigma.v1 = rep(0,length(N))
coef.names.rho.v = rep(0,length(N))

for (i in 1:length(N)){
coef.names.sigma[i] = rep(paste('Standard deviation of the level-1 error','N',N[i]))
coef.names.sigma.v0[i] = rep(paste('Standard deviation of the random intercept','N',N[i]))
coef.names.sigma.v1[i] = rep(paste('Standard deviation of the random slope','N',N[i]))
coef.names.rho.v[i] = rep(paste('Correlation between the random intercept and the random slope','N',N[i]))
}

cov.sim = data.frame(round(rbind(cbind(rep(sigma,length(N)),sigma.mean,sigma.se,sigma.bias),
cbind(rep(sigma.v0,length(N)),sigma.v0.mean,sigma.v0.se,sigma.v0.bias),
cbind(rep(sigma.v1,length(N)),sigma.v1.mean,sigma.v1.se,sigma.v1.bias),
cbind(rep(rho.v,length(N)),rho.v.mean,rho.v.se,rho.v.bias)),digits=4))
rownames(cov.sim) = c(coef.names.sigma,coef.names.sigma.v0,coef.names.sigma.v1,coef.names.rho.v)
colnames(cov.sim) = c('True value','Mean','Std.error','Bias')

# Power (for the power curve!)

power.curve = cbind(N,power.b00,power.b01,power.b10,power.b11)

# Parameters distribution across the R Monte Carlo replicates for maximum N

N.max = which.max(N)

b00.sim = fit[[N.max]][[1]][[1]][[1]]
b01.sim = fit[[N.max]][[1]][[2]][[1]]
b10.sim = fit[[N.max]][[1]][[3]][[1]]
b11.sim = fit[[N.max]][[1]][[4]][[1]]
sigma.sim = fit[[N.max]][[2]][[1]][[1]]
sigma.v0.sim = fit[[N.max]][[2]][[2]][[1]]
sigma.v1.sim = fit[[N.max]][[2]][[3]][[1]]
rho.v.sim = fit[[N.max]][[2]][[4]][[1]]

b.sim = data.frame(cbind(b00.sim,b01.sim,b10.sim,b11.sim,sigma.sim,sigma.v0.sim,sigma.v1.sim,rho.v.sim))
colnames(b.sim) = c('b00.hat','b01.hat','b10.hat','b11.hat','sigma.hat','sigma.v0.hat','sigma.v1.hat','rho.v.hat')

# Trajectories of Y for one replicate

data = Sim.Data.IL(Model,N[N.max],N.0, N.1, T,isX.center,Ylag.center,isW.center, 
b00, b01.Z, b01.W, b10, b11.Z, b11.W, sigma, rho, sigma.v0, sigma.v1, 
rho.v, mu.W, sigma.W, mu.X, mu.X0, mu.X1, sigma.X, sigma.X0, sigma.X1)

Ymean = aggregate(data, by=list(data$subjno),FUN=mean, na.rm=TRUE)

Q.05 = quantile(Ymean$Y,probs=0.05)
N.Q05 = Ymean$subjno[which.max(Ymean$Y<=Q.05)]

Q.5 = quantile(Ymean$Y,probs=0.5)
N.Q5 = Ymean$subjno[which.max(Ymean$Y<=Q.5)]

Q.95 = quantile(Ymean$Y,probs=0.95)
N.Q95 = Ymean$subjno[which.max(Ymean$Y>=Q.95)]

T.list = rep(1:T,1)
Ni.05 = which(data$subjno==N.Q05)
Ni.5 = which(data$subjno==N.Q5)
Ni.95 = which(data$subjno==N.Q95)

data.IL.Q05 = data.frame(cbind(data[Ni.05,],Time=T.list))
data.IL.Q5 = data.frame(cbind(data[Ni.5,],Time=T.list))
data.IL.Q95 = data.frame(cbind(data[Ni.95,],Time=T.list))

Y.pool = rbind(data.IL.Q05,data.IL.Q5,data.IL.Q95)$Y
Y.IL.min = min(Y.pool)
Y.IL.max = max(Y.pool)
}

return(list(power.curve=power.curve,coef.sim=coef.sim,b.sim=b.sim,
data.IL.Q05=data.IL.Q05,data.IL.Q5=data.IL.Q5,data.IL.Q95=data.IL.Q95,
Y.IL.min=Y.IL.min,Y.IL.max=Y.IL.max,cov.sim=cov.sim))}

#####################################################################################











