###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################

# Function to compute power for IL studies as a function of the number of participants

Summary.model.IL = function(Model, N, N.0, N.1, T, 
isX.center, Ylag.center, isW.center, alpha, 
b00, b01.Z, b01.W, b10, b11.Z, b11.W, sigma, rho, sigma.v0, sigma.v1,rho.v, 
mu.W, sigma.W, mu.X, mu.X0, mu.X1, sigma.X, sigma.X0, sigma.X1,R,Opt.Method,is.rho.zero){

cmp.Fit.model.IL = cmpfun(Fit.model.IL)

########################################################################################
########################################################################################
########################################################################################

# Simulate data from Model 1: Y ~ b00 + b01*Z

if (Model == 1){

plan(multisession)
fit = future_lapply(1:R, function(r)
cmp.Fit.model.IL(Model, N, N.0, N.1, T, 
isX.center, Ylag.center, isW.center,
b00, b01.Z, b01.W, b10, b11.Z, b11.W, sigma, rho, sigma.v0, sigma.v1,rho.v, 
mu.W, sigma.W, mu.X, mu.X0, mu.X1, sigma.X, sigma.X0, sigma.X1,Opt.Method,is.rho.zero),future.seed = 0xBEEF)

errors = rep(0,R)
for (r in 1:R){errors[r] = length(fit[[r]])}
errors = sum(ifelse(errors==1,1,0)) 

if (errors>0){stop(paste(errors, 'replications produce convergence errors. 
Check the value of the parameters or set the number of participants in Group 0 larger to',
N.0,'or set the number of participants in Group 1 larger to',N.1))}

# Distribution of the parameters

# Fixed Effects 

b00.sim = unlist(lapply(1:R, function(r) coef(fit[[r]])[1,1]))
LCI.b00 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[1,1]))
UCI.b00 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[1,3]))
CI.b00 = unlist(lapply(1:R, function(r) (LCI.b00[r] < b00 & UCI.b00[r] > b00)))
power.b00 = unlist(lapply(1:R, function(r) coef(fit[[r]])[1,5]<alpha))

b01.sim = unlist(lapply(1:R, function(r) coef(fit[[r]])[2,1]))
LCI.b01 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[2,1]))
UCI.b01 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[2,3]))
CI.b01 = unlist(lapply(1:R, function(r) (LCI.b01[r] < b01.Z & UCI.b01[r] > b01.Z)))
power.b01 = unlist(lapply(1:R, function(r) coef(fit[[r]])[2,5]<alpha))

# Random Effects
sigma.sim = unlist(lapply(1:R, function(r) as.numeric(VarCorr(fit[[r]])[2,2])))
sigma.v0.sim = unlist(lapply(1:R, function(r) as.numeric(VarCorr(fit[[r]])[1,2])))

if (is.rho.zero==TRUE){
rho.sim = unlist(lapply(1:R, function(r) as.numeric(coef(fit[[r]]$modelStruct$corStruct,unconstrained=FALSE))))
}

# Summary 

# Fixed effects

b00.mean = mean(b00.sim)
b00.se = sd(b00.sim)/sqrt(R)
b00.bias = b00.mean - b00
CI.b00 = mean(CI.b00) 

b01.mean = mean(b01.sim)
b01.se = sd(b01.sim)/sqrt(R)
b01.bias = b01.mean - b01.Z 
CI.b01 = mean(CI.b01)

# Power 
# Compute Power for H0: b00=0
power.b00 = mean(power.b00)

# Compute Power for H0: b01=0
power.b01 = mean(power.b01)

# Distribution of the parameters: random effects

sigma.mean = mean(sigma.sim)
sigma.se = sd(sigma.sim)/sqrt(R)
sigma.bias = sigma.mean - sigma 

sigma.v0.mean = mean(sigma.v0.sim)
sigma.v0.se = sd(sigma.v0.sim)/sqrt(R)
sigma.v0.bias = sigma.v0.mean - sigma.v0 

if (is.rho.zero==TRUE){
rho.mean = mean(rho.sim)
rho.se = sd(rho.sim)/sqrt(R)
rho.bias = rho.mean - rho 
}

# Output

fixed.effects = list(list(b00.sim,b00.mean,b00.se,b00.bias,CI.b00,power.b00),
list(b01.sim,b01.mean,b01.se,b01.bias,CI.b01,power.b01))

if (is.rho.zero==TRUE){
random.effects = list(list(sigma.sim,sigma.mean,sigma.se,sigma.bias),
list(rho.sim,rho.mean,rho.se,rho.bias),
list(sigma.v0.sim,sigma.v0.mean,sigma.v0.se,sigma.v0.bias))
}

if (is.rho.zero==FALSE){
random.effects = list(list(sigma.sim,sigma.mean,sigma.se,sigma.bias),
list(sigma.v0.sim,sigma.v0.mean,sigma.v0.se,sigma.v0.bias))
}
}

########################################################################################

# Simulate data from Model 2: Y ~ b00 + b01*W

if (Model == 2){

plan(multisession)
fit = future_lapply(1:R, function(r)
cmp.Fit.model.IL(Model, N, N.0, N.1, T, 
isX.center, Ylag.center, isW.center,
b00, b01.Z, b01.W, b10, b11.Z, b11.W, sigma, rho, sigma.v0, sigma.v1,rho.v, 
mu.W, sigma.W, mu.X, mu.X0, mu.X1, sigma.X, sigma.X0, sigma.X1,Opt.Method,is.rho.zero),future.seed = 0xBEEF)

errors = rep(0,R)
for (r in 1:R){errors[r] = length(fit[[r]])}
errors = sum(ifelse(errors==1,1,0)) 

if (errors>0){stop(paste(errors, 'replications produce convergence errors. 
Check the value of the parameters or set the number of participants larger to',
N))}

# Distribution of the parameters

# Fixed Effects 

b00.sim = unlist(lapply(1:R, function(r) coef(fit[[r]])[1,1]))
LCI.b00 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[1,1]))
UCI.b00 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[1,3]))
CI.b00 = unlist(lapply(1:R, function(r) (LCI.b00[r] < b00 & UCI.b00[r] > b00)))
power.b00 = unlist(lapply(1:R, function(r) coef(fit[[r]])[1,5]<alpha))

b01.sim = unlist(lapply(1:R, function(r) coef(fit[[r]])[2,1]))
LCI.b01 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[2,1]))
UCI.b01 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[2,3]))
CI.b01 = unlist(lapply(1:R, function(r) (LCI.b01[r] < b01.W & UCI.b01[r] > b01.W)))
power.b01 = unlist(lapply(1:R, function(r) coef(fit[[r]])[2,5]<alpha))

# Random Effects
sigma.sim = unlist(lapply(1:R, function(r) as.numeric(VarCorr(fit[[r]])[2,2])))
sigma.v0.sim = unlist(lapply(1:R, function(r) as.numeric(VarCorr(fit[[r]])[1,2])))

if (is.rho.zero==TRUE){
rho.sim = unlist(lapply(1:R, function(r) as.numeric(coef(fit[[r]]$modelStruct$corStruct,unconstrained=FALSE))))
}

# Summary 

# Fixed effects

b00.mean = mean(b00.sim)
b00.se = sd(b00.sim)/sqrt(R)
b00.bias = b00.mean - b00
CI.b00 = mean(CI.b00) 

b01.mean = mean(b01.sim)
b01.se = sd(b01.sim)/sqrt(R)
b01.bias = b01.mean - b01.W 
CI.b01 = mean(CI.b01)

# Power 
# Compute Power for H0: b00=0
power.b00 = mean(power.b00)

# Compute Power for H0: b01=0
power.b01 = mean(power.b01)

# Distribution of the parameters: random effects

sigma.mean = mean(sigma.sim)
sigma.se = sd(sigma.sim)/sqrt(R)
sigma.bias = sigma.mean - sigma 

sigma.v0.mean = mean(sigma.v0.sim)
sigma.v0.se = sd(sigma.v0.sim)/sqrt(R)
sigma.v0.bias = sigma.v0.mean - sigma.v0 

if (is.rho.zero==TRUE){
rho.mean = mean(rho.sim)
rho.se = sd(rho.sim)/sqrt(R)
rho.bias = rho.mean - rho 
}

# Output

fixed.effects = list(list(b00.sim,b00.mean,b00.se,b00.bias,CI.b00,power.b00),
list(b01.sim,b01.mean,b01.se,b01.bias,CI.b01,power.b01))

if (is.rho.zero==TRUE){
random.effects = list(list(sigma.sim,sigma.mean,sigma.se,sigma.bias),
list(rho.sim,rho.mean,rho.se,rho.bias),
list(sigma.v0.sim,sigma.v0.mean,sigma.v0.se,sigma.v0.bias))
}

if (is.rho.zero==FALSE){
random.effects = list(list(sigma.sim,sigma.mean,sigma.se,sigma.bias),
list(sigma.v0.sim,sigma.v0.mean,sigma.v0.se,sigma.v0.bias))
}
}

########################################################################################

# Simulate data from Model 3: Y ~ b00 + b10*X

if (Model == 3){

plan(multisession)
fit = future_lapply(1:R, function(r)
cmp.Fit.model.IL(Model, N, N.0, N.1, T, 
isX.center, Ylag.center, isW.center,
b00, b01.Z, b01.W, b10, b11.Z, b11.W, sigma, rho, sigma.v0, sigma.v1,rho.v, 
mu.W, sigma.W, mu.X, mu.X0, mu.X1, sigma.X, sigma.X0, sigma.X1,Opt.Method,is.rho.zero),future.seed = 0xBEEF)

errors = rep(0,R)
for (r in 1:R){errors[r] = length(fit[[r]])}
errors = sum(ifelse(errors==1,1,0)) 

if (errors>0){stop(paste(errors, 'replications produce convergence errors. 
Check the value of the parameters or set the number of participants larger to',
N))}


# Distribution of the parameters

# Fixed Effects 

b00.sim = unlist(lapply(1:R, function(r) coef(fit[[r]])[1,1]))
LCI.b00 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[1,1]))
UCI.b00 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[1,3]))
CI.b00 = unlist(lapply(1:R, function(r) (LCI.b00[r] < b00 & UCI.b00[r] > b00)))
power.b00 = unlist(lapply(1:R, function(r) coef(fit[[r]])[1,5]<alpha))

b10.sim = unlist(lapply(1:R, function(r) coef(fit[[r]])[2,1]))
LCI.b10 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[2,1]))
UCI.b10 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[2,3]))
CI.b10 = unlist(lapply(1:R, function(r) (LCI.b10[r] < b10 & UCI.b10[r] > b10)))
power.b10 = unlist(lapply(1:R, function(r) coef(fit[[r]])[2,5]<alpha))

# Random Effects
sigma.sim = unlist(lapply(1:R, function(r) as.numeric(VarCorr(fit[[r]])[3,2])))
sigma.v0.sim = unlist(lapply(1:R, function(r) as.numeric(VarCorr(fit[[r]])[1,2])))
sigma.v1.sim = unlist(lapply(1:R, function(r) as.numeric(VarCorr(fit[[r]])[2,2])))
rho.v.sim = unlist(lapply(1:R, function(r) as.numeric(VarCorr(fit[[r]])[2,3])))

if (is.rho.zero==TRUE){
rho.sim = unlist(lapply(1:R, function(r) as.numeric(coef(fit[[r]]$modelStruct$corStruct,unconstrained=FALSE))))
}

# Summary 

# Fixed effects

b00.mean = mean(b00.sim)
b00.se = sd(b00.sim)/sqrt(R)
b00.bias = b00.mean - b00
CI.b00 = mean(CI.b00) 

b10.mean = mean(b10.sim)
b10.se = sd(b10.sim)/sqrt(R)
b10.bias = b10.mean - b10 
CI.b10 = mean(CI.b10)

# Power 
# Compute Power for H0: b00=0
power.b00 = mean(power.b00)

# Compute Power for H0: b10=0
power.b10 = mean(power.b10)

# Distribution of the parameters: random effects

sigma.mean = mean(sigma.sim)
sigma.se = sd(sigma.sim)/sqrt(R)
sigma.bias = sigma.mean - sigma 

sigma.v0.mean = mean(sigma.v0.sim)
sigma.v0.se = sd(sigma.v0.sim)/sqrt(R)
sigma.v0.bias = sigma.v0.mean - sigma.v0 

sigma.v1.mean = mean(sigma.v1.sim)
sigma.v1.se = sd(sigma.v1.sim)/sqrt(R)
sigma.v1.bias = sigma.v1.mean - sigma.v1 

rho.v.mean = mean(rho.v.sim)
rho.v.se = sd(rho.v.sim)/sqrt(R)
rho.v.bias = rho.v.mean - rho.v 

if (is.rho.zero==TRUE){
rho.mean = mean(rho.sim)
rho.se = sd(rho.sim)/sqrt(R)
rho.bias = rho.mean - rho 
}

# Output

fixed.effects = list(list(b00.sim,b00.mean,b00.se,b00.bias,CI.b00,power.b00),
list(b10.sim,b10.mean,b10.se,b10.bias,CI.b10,power.b10))

if (is.rho.zero==TRUE){
random.effects = list(list(sigma.sim,sigma.mean,sigma.se,sigma.bias),
list(rho.sim,rho.mean,rho.se,rho.bias),
list(sigma.v0.sim,sigma.v0.mean,sigma.v0.se,sigma.v0.bias),
list(sigma.v1.sim,sigma.v1.mean,sigma.v1.se,sigma.v1.bias),
list(rho.v.sim,rho.v.mean,rho.v.se,rho.v.bias))
}

if (is.rho.zero==FALSE){
random.effects = list(list(sigma.sim,sigma.mean,sigma.se,sigma.bias),
list(sigma.v0.sim,sigma.v0.mean,sigma.v0.se,sigma.v0.bias),
list(sigma.v1.sim,sigma.v1.mean,sigma.v1.se,sigma.v1.bias),
list(rho.v.sim,rho.v.mean,rho.v.se,rho.v.bias))
}
}

########################################################################################

# Simulate data from Model 4: Y ~ b00 + b10*X fixed slope

if (Model == 4){

plan(multisession)
fit = future_lapply(1:R, function(r)
cmp.Fit.model.IL(Model, N, N.0, N.1, T, 
isX.center, Ylag.center, isW.center,
b00, b01.Z, b01.W, b10, b11.Z, b11.W, sigma, rho, sigma.v0, sigma.v1,rho.v, 
mu.W, sigma.W, mu.X, mu.X0, mu.X1, sigma.X, sigma.X0, sigma.X1,Opt.Method,is.rho.zero),future.seed = 0xBEEF)

errors = rep(0,R)
for (r in 1:R){errors[r] = length(fit[[r]])}
errors = sum(ifelse(errors==1,1,0)) 

if (errors>0){stop(paste(errors, 'replications produce convergence errors. 
Check the value of the parameters or set the number of participants larger to',
N))}


# Distribution of the parameters

# Fixed Effects 

b00.sim = unlist(lapply(1:R, function(r) coef(fit[[r]])[1,1]))
LCI.b00 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[1,1]))
UCI.b00 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[1,3]))
CI.b00 = unlist(lapply(1:R, function(r) (LCI.b00[r] < b00 & UCI.b00[r] > b00)))
power.b00 = unlist(lapply(1:R, function(r) coef(fit[[r]])[1,5]<alpha))

b10.sim = unlist(lapply(1:R, function(r) coef(fit[[r]])[2,1]))
LCI.b10 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[2,1]))
UCI.b10 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[2,3]))
CI.b10 = unlist(lapply(1:R, function(r) (LCI.b10[r] < b10 & UCI.b10[r] > b10)))
power.b10 = unlist(lapply(1:R, function(r) coef(fit[[r]])[2,5]<alpha))

# Random Effects
sigma.sim = unlist(lapply(1:R, function(r) as.numeric(VarCorr(fit[[r]])[2,2])))
sigma.v0.sim = unlist(lapply(1:R, function(r) as.numeric(VarCorr(fit[[r]])[1,2])))

if (is.rho.zero==TRUE){
rho.sim = unlist(lapply(1:R, function(r) as.numeric(coef(fit[[r]]$modelStruct$corStruct,unconstrained=FALSE))))
}

# Summary 

# Fixed effects

b00.mean = mean(b00.sim)
b00.se = sd(b00.sim)/sqrt(R)
b00.bias = b00.mean - b00
CI.b00 = mean(CI.b00) 

b10.mean = mean(b10.sim)
b10.se = sd(b10.sim)/sqrt(R)
b10.bias = b10.mean - b10 
CI.b10 = mean(CI.b10)

# Power 
# Compute Power for H0: b00=0
power.b00 = mean(power.b00)

# Compute Power for H0: b10=0
power.b10 = mean(power.b10)

# Distribution of the parameters: random effects

sigma.mean = mean(sigma.sim)
sigma.se = sd(sigma.sim)/sqrt(R)
sigma.bias = sigma.mean - sigma 

sigma.v0.mean = mean(sigma.v0.sim)
sigma.v0.se = sd(sigma.v0.sim)/sqrt(R)
sigma.v0.bias = sigma.v0.mean - sigma.v0 

if (is.rho.zero==TRUE){
rho.mean = mean(rho.sim)
rho.se = sd(rho.sim)/sqrt(R)
rho.bias = rho.mean - rho 
}

# Output

fixed.effects = list(list(b00.sim,b00.mean,b00.se,b00.bias,CI.b00,power.b00),
list(b10.sim,b10.mean,b10.se,b10.bias,CI.b10,power.b10))

if (is.rho.zero==TRUE){
random.effects = list(list(sigma.sim,sigma.mean,sigma.se,sigma.bias),
list(rho.sim,rho.mean,rho.se,rho.bias),
list(sigma.v0.sim,sigma.v0.mean,sigma.v0.se,sigma.v0.bias))
}

if (is.rho.zero==FALSE){
random.effects = list(list(sigma.sim,sigma.mean,sigma.se,sigma.bias),
list(sigma.v0.sim,sigma.v0.mean,sigma.v0.se,sigma.v0.bias))
}
}

########################################################################################

# Simulate data from Model 5: Y ~ b00 + b01*Z + b10*X + b11*Z*X

if (Model == 5){

plan(multisession)
fit = future_lapply(1:R, function(r)
cmp.Fit.model.IL(Model, N, N.0, N.1, T, 
isX.center, Ylag.center, isW.center,
b00, b01.Z, b01.W, b10, b11.Z, b11.W, sigma, rho, sigma.v0, sigma.v1,rho.v, 
mu.W, sigma.W, mu.X, mu.X0, mu.X1, sigma.X, sigma.X0, sigma.X1,Opt.Method,is.rho.zero),future.seed = 0xBEEF)

errors = rep(0,R)
for (r in 1:R){errors[r] = length(fit[[r]])}
errors = sum(ifelse(errors==1,1,0)) 

if (errors>0){stop(paste(errors, 'replications produce convergence errors. 
Check the value of the parameters or set the number of participants in Group 0 larger to',
N.0,'or set the number of participants in Group 1 larger to',N.1))}

# Distribution of the parameters

# Fixed Effects 

b00.sim = unlist(lapply(1:R, function(r) coef(fit[[r]])[1,1]))
LCI.b00 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[1,1]))
UCI.b00 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[1,3]))
CI.b00 = unlist(lapply(1:R, function(r) (LCI.b00[r] < b00 & UCI.b00[r] > b00)))
power.b00 = unlist(lapply(1:R, function(r) coef(fit[[r]])[1,5]<alpha))

b01.sim = unlist(lapply(1:R, function(r) coef(fit[[r]])[2,1]))
LCI.b01 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[2,1]))
UCI.b01 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[2,3]))
CI.b01 = unlist(lapply(1:R, function(r) (LCI.b01[r] < b01.Z & UCI.b01[r] > b01.Z)))
power.b01 = unlist(lapply(1:R, function(r) coef(fit[[r]])[2,5]<alpha))

b10.sim = unlist(lapply(1:R, function(r) coef(fit[[r]])[3,1]))
LCI.b10 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[3,1]))
UCI.b10 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[3,3]))
CI.b10 = unlist(lapply(1:R, function(r) (LCI.b10[r] < b10 & UCI.b10[r] > b10)))
power.b10 = unlist(lapply(1:R, function(r) coef(fit[[r]])[3,5]<alpha))

b11.sim = unlist(lapply(1:R, function(r) coef(fit[[r]])[4,1]))
LCI.b11 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[4,1]))
UCI.b11 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[4,3]))
CI.b11 = unlist(lapply(1:R, function(r) (LCI.b11[r] < b11.Z & UCI.b11[r] > b11.Z)))
power.b11 = unlist(lapply(1:R, function(r) coef(fit[[r]])[4,5]<alpha))

# Random Effects
sigma.sim = unlist(lapply(1:R, function(r) as.numeric(VarCorr(fit[[r]])[3,2])))
sigma.v0.sim = unlist(lapply(1:R, function(r) as.numeric(VarCorr(fit[[r]])[1,2])))
sigma.v1.sim = unlist(lapply(1:R, function(r) as.numeric(VarCorr(fit[[r]])[2,2])))
rho.v.sim = unlist(lapply(1:R, function(r) as.numeric(VarCorr(fit[[r]])[2,3])))

if (is.rho.zero==TRUE){
rho.sim = unlist(lapply(1:R, function(r) as.numeric(coef(fit[[r]]$modelStruct$corStruct,unconstrained=FALSE))))
}

# Summary 

# Fixed effects

b00.mean = mean(b00.sim)
b00.se = sd(b00.sim)/sqrt(R)
b00.bias = b00.mean - b00
CI.b00 = mean(CI.b00) 

b01.mean = mean(b01.sim)
b01.se = sd(b01.sim)/sqrt(R)
b01.bias = b01.mean - b01.Z
CI.b01 = mean(CI.b01) 

b10.mean = mean(b10.sim)
b10.se = sd(b10.sim)/sqrt(R)
b10.bias = b10.mean - b10 
CI.b10 = mean(CI.b10)

b11.mean = mean(b11.sim)
b11.se = sd(b11.sim)/sqrt(R)
b11.bias = b11.mean - b11.Z 
CI.b11 = mean(CI.b11)

# Power 
# Compute Power for H0: b00=0
power.b00 = mean(power.b00)

# Compute Power for H0: b01=0
power.b01 = mean(power.b01)

# Compute Power for H0: b10=0
power.b10 = mean(power.b10)

# Compute Power for H0: b11=0
power.b11 = mean(power.b11)

# Distribution of the parameters: random effects

sigma.mean = mean(sigma.sim)
sigma.se = sd(sigma.sim)/sqrt(R)
sigma.bias = sigma.mean - sigma 

sigma.v0.mean = mean(sigma.v0.sim)
sigma.v0.se = sd(sigma.v0.sim)/sqrt(R)
sigma.v0.bias = sigma.v0.mean - sigma.v0 

sigma.v1.mean = mean(sigma.v1.sim)
sigma.v1.se = sd(sigma.v1.sim)/sqrt(R)
sigma.v1.bias = sigma.v1.mean - sigma.v1 

rho.v.mean = mean(rho.v.sim)
rho.v.se = sd(rho.v.sim)/sqrt(R)
rho.v.bias = rho.v.mean - rho.v 

if (is.rho.zero==TRUE){
rho.mean = mean(rho.sim)
rho.se = sd(rho.sim)/sqrt(R)
rho.bias = rho.mean - rho 
}

# Output

fixed.effects = list(list(b00.sim,b00.mean,b00.se,b00.bias,CI.b00,power.b00),
list(b01.sim,b01.mean,b01.se,b01.bias,CI.b01,power.b01),
list(b10.sim,b10.mean,b10.se,b10.bias,CI.b10,power.b10),
list(b11.sim,b11.mean,b11.se,b11.bias,CI.b11,power.b11))

if (is.rho.zero==TRUE){
random.effects = list(list(sigma.sim,sigma.mean,sigma.se,sigma.bias),
list(rho.sim,rho.mean,rho.se,rho.bias),
list(sigma.v0.sim,sigma.v0.mean,sigma.v0.se,sigma.v0.bias),
list(sigma.v1.sim,sigma.v1.mean,sigma.v1.se,sigma.v1.bias),
list(rho.v.sim,rho.v.mean,rho.v.se,rho.v.bias))
}

if (is.rho.zero==FALSE){
random.effects = list(list(sigma.sim,sigma.mean,sigma.se,sigma.bias),
list(sigma.v0.sim,sigma.v0.mean,sigma.v0.se,sigma.v0.bias),
list(sigma.v1.sim,sigma.v1.mean,sigma.v1.se,sigma.v1.bias),
list(rho.v.sim,rho.v.mean,rho.v.se,rho.v.bias))
}
}

########################################################################################

# Simulate data from Model 6: Y ~ b00 + b01*Z + b10*X + b11*Z*X fixed slope

if (Model == 6){

plan(multisession)
fit = future_lapply(1:R, function(r)
cmp.Fit.model.IL(Model, N, N.0, N.1, T, 
isX.center, Ylag.center, isW.center,
b00, b01.Z, b01.W, b10, b11.Z, b11.W, sigma, rho, sigma.v0, sigma.v1,rho.v, 
mu.W, sigma.W, mu.X, mu.X0, mu.X1, sigma.X, sigma.X0, sigma.X1,Opt.Method,is.rho.zero),future.seed = 0xBEEF)

errors = rep(0,R)
for (r in 1:R){errors[r] = length(fit[[r]])}
errors = sum(ifelse(errors==1,1,0)) 

if (errors>0){stop(paste(errors, 'replications produce convergence errors. 
Check the value of the parameters or set the number of participants in Group 0 larger to',
N.0,'or set the number of participants in Group 1 larger to',N.1))}

# Distribution of the parameters

# Fixed Effects 

b00.sim = unlist(lapply(1:R, function(r) coef(fit[[r]])[1,1]))
LCI.b00 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[1,1]))
UCI.b00 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[1,3]))
CI.b00 = unlist(lapply(1:R, function(r) (LCI.b00[r] < b00 & UCI.b00[r] > b00)))
power.b00 = unlist(lapply(1:R, function(r) coef(fit[[r]])[1,5]<alpha))

b01.sim = unlist(lapply(1:R, function(r) coef(fit[[r]])[2,1]))
LCI.b01 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[2,1]))
UCI.b01 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[2,3]))
CI.b01 = unlist(lapply(1:R, function(r) (LCI.b01[r] < b01.Z & UCI.b01[r] > b01.Z)))
power.b01 = unlist(lapply(1:R, function(r) coef(fit[[r]])[2,5]<alpha))

b10.sim = unlist(lapply(1:R, function(r) coef(fit[[r]])[3,1]))
LCI.b10 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[3,1]))
UCI.b10 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[3,3]))
CI.b10 = unlist(lapply(1:R, function(r) (LCI.b10[r] < b10 & UCI.b10[r] > b10)))
power.b10 = unlist(lapply(1:R, function(r) coef(fit[[r]])[3,5]<alpha))

b11.sim = unlist(lapply(1:R, function(r) coef(fit[[r]])[4,1]))
LCI.b11 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[4,1]))
UCI.b11 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[4,3]))
CI.b11 = unlist(lapply(1:R, function(r) (LCI.b11[r] < b11.Z & UCI.b11[r] > b11.Z)))
power.b11 = unlist(lapply(1:R, function(r) coef(fit[[r]])[4,5]<alpha))

# Random Effects
sigma.sim = unlist(lapply(1:R, function(r) as.numeric(VarCorr(fit[[r]])[2,2])))
sigma.v0.sim = unlist(lapply(1:R, function(r) as.numeric(VarCorr(fit[[r]])[1,2])))

if (is.rho.zero==TRUE){
rho.sim = unlist(lapply(1:R, function(r) as.numeric(coef(fit[[r]]$modelStruct$corStruct,unconstrained=FALSE))))
}

# Summary 

# Fixed effects

b00.mean = mean(b00.sim)
b00.se = sd(b00.sim)/sqrt(R)
b00.bias = b00.mean - b00
CI.b00 = mean(CI.b00) 

b01.mean = mean(b01.sim)
b01.se = sd(b01.sim)/sqrt(R)
b01.bias = b01.mean - b01.Z
CI.b01 = mean(CI.b01) 

b10.mean = mean(b10.sim)
b10.se = sd(b10.sim)/sqrt(R)
b10.bias = b10.mean - b10 
CI.b10 = mean(CI.b10)

b11.mean = mean(b11.sim)
b11.se = sd(b11.sim)/sqrt(R)
b11.bias = b11.mean - b11.Z 
CI.b11 = mean(CI.b11)

# Power 
# Compute Power for H0: b00=0
power.b00 = mean(power.b00)

# Compute Power for H0: b01=0
power.b01 = mean(power.b01)

# Compute Power for H0: b10=0
power.b10 = mean(power.b10)

# Compute Power for H0: b11=0
power.b11 = mean(power.b11)

# Distribution of the parameters: random effects

sigma.mean = mean(sigma.sim)
sigma.se = sd(sigma.sim)/sqrt(R)
sigma.bias = sigma.mean - sigma 

sigma.v0.mean = mean(sigma.v0.sim)
sigma.v0.se = sd(sigma.v0.sim)/sqrt(R)
sigma.v0.bias = sigma.v0.mean - sigma.v0 

if (is.rho.zero==TRUE){
rho.mean = mean(rho.sim)
rho.se = sd(rho.sim)/sqrt(R)
rho.bias = rho.mean - rho 
}

# Output

fixed.effects = list(list(b00.sim,b00.mean,b00.se,b00.bias,CI.b00,power.b00),
list(b01.sim,b01.mean,b01.se,b01.bias,CI.b01,power.b01),
list(b10.sim,b10.mean,b10.se,b10.bias,CI.b10,power.b10),
list(b11.sim,b11.mean,b11.se,b11.bias,CI.b11,power.b11))

if (is.rho.zero==TRUE){
random.effects = list(list(sigma.sim,sigma.mean,sigma.se,sigma.bias),
list(rho.sim,rho.mean,rho.se,rho.bias),
list(sigma.v0.sim,sigma.v0.mean,sigma.v0.se,sigma.v0.bias))
}

if (is.rho.zero==FALSE){
random.effects = list(list(sigma.sim,sigma.mean,sigma.se,sigma.bias),
list(sigma.v0.sim,sigma.v0.mean,sigma.v0.se,sigma.v0.bias))
}
}

########################################################################################

# Simulate data from Model 7: Y ~ b00 + b01*W + b10*X + b11*W*X

if (Model == 7){

plan(multisession)
fit = future_lapply(1:R, function(r)
cmp.Fit.model.IL(Model, N, N.0, N.1, T, 
isX.center, Ylag.center, isW.center,
b00, b01.Z, b01.W, b10, b11.Z, b11.W, sigma, rho, sigma.v0, sigma.v1,rho.v, 
mu.W, sigma.W, mu.X, mu.X0, mu.X1, sigma.X, sigma.X0, sigma.X1,Opt.Method,is.rho.zero),future.seed = 0xBEEF)

errors = rep(0,R)
for (r in 1:R){errors[r] = length(fit[[r]])}
errors = sum(ifelse(errors==1,1,0)) 

if (errors>0){stop(paste(errors, 'replications produce convergence errors. 
Check the value of the parameters or set the number of participants larger to',
N))}

# Distribution of the parameters

# Fixed Effects 

b00.sim = unlist(lapply(1:R, function(r) coef(fit[[r]])[1,1]))
LCI.b00 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[1,1]))
UCI.b00 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[1,3]))
CI.b00 = unlist(lapply(1:R, function(r) (LCI.b00[r] < b00 & UCI.b00[r] > b00)))
power.b00 = unlist(lapply(1:R, function(r) coef(fit[[r]])[1,5]<alpha))

b01.sim = unlist(lapply(1:R, function(r) coef(fit[[r]])[2,1]))
LCI.b01 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[2,1]))
UCI.b01 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[2,3]))
CI.b01 = unlist(lapply(1:R, function(r) (LCI.b01[r] < b01.W & UCI.b01[r] > b01.W)))
power.b01 = unlist(lapply(1:R, function(r) coef(fit[[r]])[2,5]<alpha))

b10.sim = unlist(lapply(1:R, function(r) coef(fit[[r]])[3,1]))
LCI.b10 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[3,1]))
UCI.b10 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[3,3]))
CI.b10 = unlist(lapply(1:R, function(r) (LCI.b10[r] < b10 & UCI.b10[r] > b10)))
power.b10 = unlist(lapply(1:R, function(r) coef(fit[[r]])[3,5]<alpha))

b11.sim = unlist(lapply(1:R, function(r) coef(fit[[r]])[4,1]))
LCI.b11 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[4,1]))
UCI.b11 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[4,3]))
CI.b11 = unlist(lapply(1:R, function(r) (LCI.b11[r] < b11.W & UCI.b11[r] > b11.W)))
power.b11 = unlist(lapply(1:R, function(r) coef(fit[[r]])[4,5]<alpha))

# Random Effects
sigma.sim = unlist(lapply(1:R, function(r) as.numeric(VarCorr(fit[[r]])[3,2])))
sigma.v0.sim = unlist(lapply(1:R, function(r) as.numeric(VarCorr(fit[[r]])[1,2])))
sigma.v1.sim = unlist(lapply(1:R, function(r) as.numeric(VarCorr(fit[[r]])[2,2])))
rho.v.sim = unlist(lapply(1:R, function(r) as.numeric(VarCorr(fit[[r]])[2,3])))

if (is.rho.zero==TRUE){
rho.sim = unlist(lapply(1:R, function(r) as.numeric(coef(fit[[r]]$modelStruct$corStruct,unconstrained=FALSE))))
}

# Summary 

# Fixed effects

b00.mean = mean(b00.sim)
b00.se = sd(b00.sim)/sqrt(R)
b00.bias = b00.mean - b00
CI.b00 = mean(CI.b00) 

b01.mean = mean(b01.sim)
b01.se = sd(b01.sim)/sqrt(R)
b01.bias = b01.mean - b01.W
CI.b01 = mean(CI.b01) 

b10.mean = mean(b10.sim)
b10.se = sd(b10.sim)/sqrt(R)
b10.bias = b10.mean - b10 
CI.b10 = mean(CI.b10)

b11.mean = mean(b11.sim)
b11.se = sd(b11.sim)/sqrt(R)
b11.bias = b11.mean - b11.W 
CI.b11 = mean(CI.b11)

# Power 
# Compute Power for H0: b00=0
power.b00 = mean(power.b00)

# Compute Power for H0: b01=0
power.b01 = mean(power.b01)

# Compute Power for H0: b10=0
power.b10 = mean(power.b10)

# Compute Power for H0: b11=0
power.b11 = mean(power.b11)

# Distribution of the parameters: random effects

sigma.mean = mean(sigma.sim)
sigma.se = sd(sigma.sim)/sqrt(R)
sigma.bias = sigma.mean - sigma 

sigma.v0.mean = mean(sigma.v0.sim)
sigma.v0.se = sd(sigma.v0.sim)/sqrt(R)
sigma.v0.bias = sigma.v0.mean - sigma.v0 

sigma.v1.mean = mean(sigma.v1.sim)
sigma.v1.se = sd(sigma.v1.sim)/sqrt(R)
sigma.v1.bias = sigma.v1.mean - sigma.v1 

rho.v.mean = mean(rho.v.sim)
rho.v.se = sd(rho.v.sim)/sqrt(R)
rho.v.bias = rho.v.mean - rho.v 

if (is.rho.zero==TRUE){
rho.mean = mean(rho.sim)
rho.se = sd(rho.sim)/sqrt(R)
rho.bias = rho.mean - rho 
}

# Output

fixed.effects = list(list(b00.sim,b00.mean,b00.se,b00.bias,CI.b00,power.b00),
list(b01.sim,b01.mean,b01.se,b01.bias,CI.b01,power.b01),
list(b10.sim,b10.mean,b10.se,b10.bias,CI.b10,power.b10),
list(b11.sim,b11.mean,b11.se,b11.bias,CI.b11,power.b11))

if (is.rho.zero==TRUE){
random.effects = list(list(sigma.sim,sigma.mean,sigma.se,sigma.bias),
list(rho.sim,rho.mean,rho.se,rho.bias),
list(sigma.v0.sim,sigma.v0.mean,sigma.v0.se,sigma.v0.bias),
list(sigma.v1.sim,sigma.v1.mean,sigma.v1.se,sigma.v1.bias),
list(rho.v.sim,rho.v.mean,rho.v.se,rho.v.bias))
}

if (is.rho.zero==FALSE){
random.effects = list(list(sigma.sim,sigma.mean,sigma.se,sigma.bias),
list(sigma.v0.sim,sigma.v0.mean,sigma.v0.se,sigma.v0.bias),
list(sigma.v1.sim,sigma.v1.mean,sigma.v1.se,sigma.v1.bias),
list(rho.v.sim,rho.v.mean,rho.v.se,rho.v.bias))
}
}

########################################################################################

# Simulate data from Model 8: Y ~ b00 + b01*W + b10*X + b11*W*X with fixed slope

if (Model == 8){

plan(multisession)
fit = future_lapply(1:R, function(r)
cmp.Fit.model.IL(Model, N, N.0, N.1, T, 
isX.center, Ylag.center, isW.center,
b00, b01.Z, b01.W, b10, b11.Z, b11.W, sigma, rho, sigma.v0, sigma.v1,rho.v, 
mu.W, sigma.W, mu.X, mu.X0, mu.X1, sigma.X, sigma.X0, sigma.X1,Opt.Method,is.rho.zero),future.seed = 0xBEEF)

errors = rep(0,R)
for (r in 1:R){errors[r] = length(fit[[r]])}
errors = sum(ifelse(errors==1,1,0)) 

if (errors>0){stop(paste(errors, 'replications produce convergence errors. 
Check the value of the parameters or set the number of participants larger to',
N))}

# Distribution of the parameters

# Fixed Effects 

b00.sim = unlist(lapply(1:R, function(r) coef(fit[[r]])[1,1]))
LCI.b00 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[1,1]))
UCI.b00 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[1,3]))
CI.b00 = unlist(lapply(1:R, function(r) (LCI.b00[r] < b00 & UCI.b00[r] > b00)))
power.b00 = unlist(lapply(1:R, function(r) coef(fit[[r]])[1,5]<alpha))

b01.sim = unlist(lapply(1:R, function(r) coef(fit[[r]])[2,1]))
LCI.b01 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[2,1]))
UCI.b01 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[2,3]))
CI.b01 = unlist(lapply(1:R, function(r) (LCI.b01[r] < b01.W & UCI.b01[r] > b01.W)))
power.b01 = unlist(lapply(1:R, function(r) coef(fit[[r]])[2,5]<alpha))

b10.sim = unlist(lapply(1:R, function(r) coef(fit[[r]])[3,1]))
LCI.b10 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[3,1]))
UCI.b10 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[3,3]))
CI.b10 = unlist(lapply(1:R, function(r) (LCI.b10[r] < b10 & UCI.b10[r] > b10)))
power.b10 = unlist(lapply(1:R, function(r) coef(fit[[r]])[3,5]<alpha))

b11.sim = unlist(lapply(1:R, function(r) coef(fit[[r]])[4,1]))
LCI.b11 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[4,1]))
UCI.b11 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[4,3]))
CI.b11 = unlist(lapply(1:R, function(r) (LCI.b11[r] < b11.W & UCI.b11[r] > b11.W)))
power.b11 = unlist(lapply(1:R, function(r) coef(fit[[r]])[4,5]<alpha))

# Random Effects
sigma.sim = unlist(lapply(1:R, function(r) as.numeric(VarCorr(fit[[r]])[2,2])))
sigma.v0.sim = unlist(lapply(1:R, function(r) as.numeric(VarCorr(fit[[r]])[1,2])))

if (is.rho.zero==TRUE){
rho.sim = unlist(lapply(1:R, function(r) as.numeric(coef(fit[[r]]$modelStruct$corStruct,unconstrained=FALSE))))
}

# Summary 

# Fixed effects

b00.mean = mean(b00.sim)
b00.se = sd(b00.sim)/sqrt(R)
b00.bias = b00.mean - b00
CI.b00 = mean(CI.b00) 

b01.mean = mean(b01.sim)
b01.se = sd(b01.sim)/sqrt(R)
b01.bias = b01.mean - b01.W
CI.b01 = mean(CI.b01) 

b10.mean = mean(b10.sim)
b10.se = sd(b10.sim)/sqrt(R)
b10.bias = b10.mean - b10 
CI.b10 = mean(CI.b10)

b11.mean = mean(b11.sim)
b11.se = sd(b11.sim)/sqrt(R)
b11.bias = b11.mean - b11.W 
CI.b11 = mean(CI.b11)

# Power 
# Compute Power for H0: b00=0
power.b00 = mean(power.b00)

# Compute Power for H0: b01=0
power.b01 = mean(power.b01)

# Compute Power for H0: b10=0
power.b10 = mean(power.b10)

# Compute Power for H0: b11=0
power.b11 = mean(power.b11)

# Distribution of the parameters: random effects

sigma.mean = mean(sigma.sim)
sigma.se = sd(sigma.sim)/sqrt(R)
sigma.bias = sigma.mean - sigma 

sigma.v0.mean = mean(sigma.v0.sim)
sigma.v0.se = sd(sigma.v0.sim)/sqrt(R)
sigma.v0.bias = sigma.v0.mean - sigma.v0 

if (is.rho.zero==TRUE){
rho.mean = mean(rho.sim)
rho.se = sd(rho.sim)/sqrt(R)
rho.bias = rho.mean - rho 
}

# Output

fixed.effects = list(list(b00.sim,b00.mean,b00.se,b00.bias,CI.b00,power.b00),
list(b01.sim,b01.mean,b01.se,b01.bias,CI.b01,power.b01),
list(b10.sim,b10.mean,b10.se,b10.bias,CI.b10,power.b10),
list(b11.sim,b11.mean,b11.se,b11.bias,CI.b11,power.b11))

if (is.rho.zero==TRUE){
random.effects = list(list(sigma.sim,sigma.mean,sigma.se,sigma.bias),
list(rho.sim,rho.mean,rho.se,rho.bias),
list(sigma.v0.sim,sigma.v0.mean,sigma.v0.se,sigma.v0.bias))
}

if (is.rho.zero==FALSE){
random.effects = list(list(sigma.sim,sigma.mean,sigma.se,sigma.bias),
list(sigma.v0.sim,sigma.v0.mean,sigma.v0.se,sigma.v0.bias))
}
}

########################################################################################

# Simulate data from Model 9: Y ~ b00 + b10*lag(Y)

if (Model == 9){

plan(multisession)
fit = future_lapply(1:R, function(r)
cmp.Fit.model.IL(Model, N, N.0, N.1, T, 
isX.center, Ylag.center, isW.center,
b00, b01.Z, b01.W, b10, b11.Z, b11.W, sigma, rho, sigma.v0, sigma.v1,rho.v, 
mu.W, sigma.W, mu.X, mu.X0, mu.X1, sigma.X, sigma.X0, sigma.X1,Opt.Method,is.rho.zero),future.seed = 0xBEEF)

errors = rep(0,R)
for (r in 1:R){errors[r] = length(fit[[r]])}
errors = sum(ifelse(errors==1,1,0)) 

if (errors>0){stop(paste(errors, 'replications produce convergence errors. 
Check the value of the parameters or set the number of participants larger to',
N))}


# Distribution of the parameters

# Fixed Effects 

b00.sim = unlist(lapply(1:R, function(r) coef(fit[[r]])[1,1]))
LCI.b00 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[1,1]))
UCI.b00 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[1,3]))
CI.b00 = unlist(lapply(1:R, function(r) (LCI.b00[r] < b00 & UCI.b00[r] > b00)))
power.b00 = unlist(lapply(1:R, function(r) coef(fit[[r]])[1,5]<alpha))

b10.sim = unlist(lapply(1:R, function(r) coef(fit[[r]])[2,1]))
LCI.b10 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[2,1]))
UCI.b10 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[2,3]))
CI.b10 = unlist(lapply(1:R, function(r) (LCI.b10[r] < b10 & UCI.b10[r] > b10)))
power.b10 = unlist(lapply(1:R, function(r) coef(fit[[r]])[2,5]<alpha))

# Random Effects
sigma.sim = unlist(lapply(1:R, function(r) as.numeric(VarCorr(fit[[r]])[3,2])))
sigma.v0.sim = unlist(lapply(1:R, function(r) as.numeric(VarCorr(fit[[r]])[1,2])))
sigma.v1.sim = unlist(lapply(1:R, function(r) as.numeric(VarCorr(fit[[r]])[2,2])))
rho.v.sim = unlist(lapply(1:R, function(r) as.numeric(VarCorr(fit[[r]])[2,3])))


# Summary 

# Fixed effects

b00.mean = mean(b00.sim)
b00.se = sd(b00.sim)/sqrt(R)
b00.bias = b00.mean - b00
CI.b00 = mean(CI.b00) 

b10.mean = mean(b10.sim)
b10.se = sd(b10.sim)/sqrt(R)
b10.bias = b10.mean - b10 
CI.b10 = mean(CI.b10)

# Power 
# Compute Power for H0: b00=0
power.b00 = mean(power.b00)

# Compute Power for H0: b10=0
power.b10 = mean(power.b10)

# Distribution of the parameters: random effects

sigma.mean = mean(sigma.sim)
sigma.se = sd(sigma.sim)/sqrt(R)
sigma.bias = sigma.mean - sigma 

sigma.v0.mean = mean(sigma.v0.sim)
sigma.v0.se = sd(sigma.v0.sim)/sqrt(R)
sigma.v0.bias = sigma.v0.mean - sigma.v0 

sigma.v1.mean = mean(sigma.v1.sim)
sigma.v1.se = sd(sigma.v1.sim)/sqrt(R)
sigma.v1.bias = sigma.v1.mean - sigma.v1 

rho.v.mean = mean(rho.v.sim)
rho.v.se = sd(rho.v.sim)/sqrt(R)
rho.v.bias = rho.v.mean - rho.v 

# Output

fixed.effects = list(list(b00.sim,b00.mean,b00.se,b00.bias,CI.b00,power.b00),
list(b10.sim,b10.mean,b10.se,b10.bias,CI.b10,power.b10))

random.effects = list(list(sigma.sim,sigma.mean,sigma.se,sigma.bias),
list(sigma.v0.sim,sigma.v0.mean,sigma.v0.se,sigma.v0.bias),
list(sigma.v1.sim,sigma.v1.mean,sigma.v1.se,sigma.v1.bias),
list(rho.v.sim,rho.v.mean,rho.v.se,rho.v.bias))
}


########################################################################################

# Simulate data from Model 10: Y ~ b00 + b01*Z + + b10*lag(Y) + + b11*Z*lag(Y)

if (Model == 10){

plan(multisession)
fit = future_lapply(1:R, function(r)
cmp.Fit.model.IL(Model, N, N.0, N.1, T, 
isX.center, Ylag.center, isW.center,
b00, b01.Z, b01.W, b10, b11.Z, b11.W, sigma, rho, sigma.v0, sigma.v1,rho.v, 
mu.W, sigma.W, mu.X, mu.X0, mu.X1, sigma.X, sigma.X0, sigma.X1,Opt.Method,is.rho.zero),future.seed = 0xBEEF)

errors = rep(0,R)
for (r in 1:R){errors[r] = length(fit[[r]])}
errors = sum(ifelse(errors==1,1,0)) 

if (errors>0){stop(paste(errors, 'replications produce convergence errors. 
Check the value of the parameters or set the number of participants in Group 0 larger to',
N.0,'or set the number of participants in Group 1 larger to',N.1))}

# Distribution of the parameters

# Fixed Effects 

b00.sim = unlist(lapply(1:R, function(r) coef(fit[[r]])[1,1]))
LCI.b00 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[1,1]))
UCI.b00 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[1,3]))
CI.b00 = unlist(lapply(1:R, function(r) (LCI.b00[r] < b00 & UCI.b00[r] > b00)))
power.b00 = unlist(lapply(1:R, function(r) coef(fit[[r]])[1,5]<alpha))

b01.sim = unlist(lapply(1:R, function(r) coef(fit[[r]])[2,1]))
LCI.b01 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[2,1]))
UCI.b01 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[2,3]))
CI.b01 = unlist(lapply(1:R, function(r) (LCI.b01[r] < b01.Z & UCI.b01[r] > b01.Z)))
power.b01 = unlist(lapply(1:R, function(r) coef(fit[[r]])[2,5]<alpha))

b10.sim = unlist(lapply(1:R, function(r) coef(fit[[r]])[3,1]))
LCI.b10 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[3,1]))
UCI.b10 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[3,3]))
CI.b10 = unlist(lapply(1:R, function(r) (LCI.b10[r] < b10 & UCI.b10[r] > b10)))
power.b10 = unlist(lapply(1:R, function(r) coef(fit[[r]])[3,5]<alpha))

b11.sim = unlist(lapply(1:R, function(r) coef(fit[[r]])[4,1]))
LCI.b11 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[4,1]))
UCI.b11 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[4,3]))
CI.b11 = unlist(lapply(1:R, function(r) (LCI.b11[r] < b11.Z & UCI.b11[r] > b11.Z)))
power.b11 = unlist(lapply(1:R, function(r) coef(fit[[r]])[4,5]<alpha))

# Random Effects
sigma.sim = unlist(lapply(1:R, function(r) as.numeric(VarCorr(fit[[r]])[3,2])))
sigma.v0.sim = unlist(lapply(1:R, function(r) as.numeric(VarCorr(fit[[r]])[1,2])))
sigma.v1.sim = unlist(lapply(1:R, function(r) as.numeric(VarCorr(fit[[r]])[2,2])))
rho.v.sim = unlist(lapply(1:R, function(r) as.numeric(VarCorr(fit[[r]])[2,3])))

# Summary 

# Fixed effects

b00.mean = mean(b00.sim)
b00.se = sd(b00.sim)/sqrt(R)
b00.bias = b00.mean - b00
CI.b00 = mean(CI.b00) 

b01.mean = mean(b01.sim)
b01.se = sd(b01.sim)/sqrt(R)
b01.bias = b01.mean - b01.Z
CI.b01 = mean(CI.b01) 

b10.mean = mean(b10.sim)
b10.se = sd(b10.sim)/sqrt(R)
b10.bias = b10.mean - b10 
CI.b10 = mean(CI.b10)

b11.mean = mean(b11.sim)
b11.se = sd(b11.sim)/sqrt(R)
b11.bias = b11.mean - b11.Z 
CI.b11 = mean(CI.b11)

# Power 
# Compute Power for H0: b00=0
power.b00 = mean(power.b00)

# Compute Power for H0: b01=0
power.b01 = mean(power.b01)

# Compute Power for H0: b10=0
power.b10 = mean(power.b10)

# Compute Power for H0: b11=0
power.b11 = mean(power.b11)

# Distribution of the parameters: random effects

sigma.mean = mean(sigma.sim)
sigma.se = sd(sigma.sim)/sqrt(R)
sigma.bias = sigma.mean - sigma 

sigma.v0.mean = mean(sigma.v0.sim)
sigma.v0.se = sd(sigma.v0.sim)/sqrt(R)
sigma.v0.bias = sigma.v0.mean - sigma.v0 

sigma.v1.mean = mean(sigma.v1.sim)
sigma.v1.se = sd(sigma.v1.sim)/sqrt(R)
sigma.v1.bias = sigma.v1.mean - sigma.v1 

rho.v.mean = mean(rho.v.sim)
rho.v.se = sd(rho.v.sim)/sqrt(R)
rho.v.bias = rho.v.mean - rho.v 

# Output

fixed.effects = list(list(b00.sim,b00.mean,b00.se,b00.bias,CI.b00,power.b00),
list(b01.sim,b01.mean,b01.se,b01.bias,CI.b01,power.b01),
list(b10.sim,b10.mean,b10.se,b10.bias,CI.b10,power.b10),
list(b11.sim,b11.mean,b11.se,b11.bias,CI.b11,power.b11))

random.effects = list(list(sigma.sim,sigma.mean,sigma.se,sigma.bias),
list(sigma.v0.sim,sigma.v0.mean,sigma.v0.se,sigma.v0.bias),
list(sigma.v1.sim,sigma.v1.mean,sigma.v1.se,sigma.v1.bias),
list(rho.v.sim,rho.v.mean,rho.v.se,rho.v.bias))
}

########################################################################################

# Simulate data from Model 11: Y ~ b00 + b01*W+ + b10*lag(Y) + + b11*W*lag(Y)

if (Model == 11){

plan(multisession)
fit = future_lapply(1:R, function(r)
cmp.Fit.model.IL(Model, N, N.0, N.1, T, 
isX.center, Ylag.center, isW.center,
b00, b01.Z, b01.W, b10, b11.Z, b11.W, sigma, rho, sigma.v0, sigma.v1,rho.v, 
mu.W, sigma.W, mu.X, mu.X0, mu.X1, sigma.X, sigma.X0, sigma.X1,Opt.Method,is.rho.zero),future.seed = 0xBEEF)

errors = rep(0,R)
for (r in 1:R){errors[r] = length(fit[[r]])}
errors = sum(ifelse(errors==1,1,0)) 

if (errors>0){stop(paste(errors, 'replications produce convergence errors. 
Check the value of the parameters or set the number of participants larger to',
N))}

# Distribution of the parameters

# Fixed Effects 

b00.sim = unlist(lapply(1:R, function(r) coef(fit[[r]])[1,1]))
LCI.b00 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[1,1]))
UCI.b00 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[1,3]))
CI.b00 = unlist(lapply(1:R, function(r) (LCI.b00[r] < b00 & UCI.b00[r] > b00)))
power.b00 = unlist(lapply(1:R, function(r) coef(fit[[r]])[1,5]<alpha))

b01.sim = unlist(lapply(1:R, function(r) coef(fit[[r]])[2,1]))
LCI.b01 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[2,1]))
UCI.b01 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[2,3]))
CI.b01 = unlist(lapply(1:R, function(r) (LCI.b01[r] < b01.W & UCI.b01[r] > b01.W)))
power.b01 = unlist(lapply(1:R, function(r) coef(fit[[r]])[2,5]<alpha))

b10.sim = unlist(lapply(1:R, function(r) coef(fit[[r]])[3,1]))
LCI.b10 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[3,1]))
UCI.b10 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[3,3]))
CI.b10 = unlist(lapply(1:R, function(r) (LCI.b10[r] < b10 & UCI.b10[r] > b10)))
power.b10 = unlist(lapply(1:R, function(r) coef(fit[[r]])[3,5]<alpha))

b11.sim = unlist(lapply(1:R, function(r) coef(fit[[r]])[4,1]))
LCI.b11 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[4,1]))
UCI.b11 = unlist(lapply(1:R, function(r) intervals(fit[[r]], level = (1-alpha), which = "fixed")$fixed[4,3]))
CI.b11 = unlist(lapply(1:R, function(r) (LCI.b11[r] < b11.W & UCI.b11[r] > b11.W)))
power.b11 = unlist(lapply(1:R, function(r) coef(fit[[r]])[4,5]<alpha))

# Random Effects
sigma.sim = unlist(lapply(1:R, function(r) as.numeric(VarCorr(fit[[r]])[3,2])))
sigma.v0.sim = unlist(lapply(1:R, function(r) as.numeric(VarCorr(fit[[r]])[1,2])))
sigma.v1.sim = unlist(lapply(1:R, function(r) as.numeric(VarCorr(fit[[r]])[2,2])))
rho.v.sim = unlist(lapply(1:R, function(r) as.numeric(VarCorr(fit[[r]])[2,3])))

# Summary 

# Fixed effects

b00.mean = mean(b00.sim)
b00.se = sd(b00.sim)/sqrt(R)
b00.bias = b00.mean - b00
CI.b00 = mean(CI.b00) 

b01.mean = mean(b01.sim)
b01.se = sd(b01.sim)/sqrt(R)
b01.bias = b01.mean - b01.W
CI.b01 = mean(CI.b01) 

b10.mean = mean(b10.sim)
b10.se = sd(b10.sim)/sqrt(R)
b10.bias = b10.mean - b10 
CI.b10 = mean(CI.b10)

b11.mean = mean(b11.sim)
b11.se = sd(b11.sim)/sqrt(R)
b11.bias = b11.mean - b11.W 
CI.b11 = mean(CI.b11)

# Power 
# Compute Power for H0: b00=0
power.b00 = mean(power.b00)

# Compute Power for H0: b01=0
power.b01 = mean(power.b01)

# Compute Power for H0: b10=0
power.b10 = mean(power.b10)

# Compute Power for H0: b11=0
power.b11 = mean(power.b11)

# Distribution of the parameters: random effects

sigma.mean = mean(sigma.sim)
sigma.se = sd(sigma.sim)/sqrt(R)
sigma.bias = sigma.mean - sigma 

sigma.v0.mean = mean(sigma.v0.sim)
sigma.v0.se = sd(sigma.v0.sim)/sqrt(R)
sigma.v0.bias = sigma.v0.mean - sigma.v0 

sigma.v1.mean = mean(sigma.v1.sim)
sigma.v1.se = sd(sigma.v1.sim)/sqrt(R)
sigma.v1.bias = sigma.v1.mean - sigma.v1 

rho.v.mean = mean(rho.v.sim)
rho.v.se = sd(rho.v.sim)/sqrt(R)
rho.v.bias = rho.v.mean - rho.v 

# Output

fixed.effects = list(list(b00.sim,b00.mean,b00.se,b00.bias,CI.b00,power.b00),
list(b01.sim,b01.mean,b01.se,b01.bias,CI.b01,power.b01),
list(b10.sim,b10.mean,b10.se,b10.bias,CI.b10,power.b10),
list(b11.sim,b11.mean,b11.se,b11.bias,CI.b11,power.b11))

random.effects = list(list(sigma.sim,sigma.mean,sigma.se,sigma.bias),
list(sigma.v0.sim,sigma.v0.mean,sigma.v0.se,sigma.v0.bias),
list(sigma.v1.sim,sigma.v1.mean,sigma.v1.se,sigma.v1.bias),
list(rho.v.sim,rho.v.mean,rho.v.se,rho.v.bias))
}

return(list(fixed.effects=fixed.effects,random.effects=random.effects))}

#####################################################################################
