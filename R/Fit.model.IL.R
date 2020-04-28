###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################

# Function to compute power for IL studies as a function of the number of participants

Fit.model.IL = function(Model, N, N.0, N.1, T, 
isX.center, Ylag.center, isW.center,  
b00, b01.Z, b01.W, b10, b11.Z, b11.W, sigma, rho, sigma.v0, sigma.v1,rho.v, 
mu.W, sigma.W, mu.X, mu.X0, mu.X1, sigma.X, sigma.X0, sigma.X1,Opt.Method,is.rho.zero){

cmp.Sim.Data.IL = cmpfun(Sim.Data.IL)

# Simulate Data 
data = cmp.Sim.Data.IL(Model,N,N.0, N.1, 
T, isX.center, Ylag.center, isW.center, 
b00, b01.Z, b01.W, b10, b11.Z, b11.W, sigma, rho, sigma.v0, sigma.v1, 
rho.v, mu.W, sigma.W, mu.X, mu.X0, mu.X1, sigma.X, sigma.X0, sigma.X1)

########################################################################################
########################################################################################
########################################################################################

# Simulate data from Model 1: Y ~ b00 + b01*Z

if (Model == 1){

# Estimate Mixed Model Effects

if (Opt.Method == 1){
if (is.rho.zero==TRUE){
fit = try(summary(lme(Y ~ Z ,random = ~1 | subjno, data=data,
control=lmeControl(opt='optim'),method='ML',correlation = corAR1())),silent = TRUE)
}
if (is.rho.zero==FALSE){
fit = try(summary(lme(Y ~ Z ,random = ~1 | subjno, data=data,
control=lmeControl(opt='optim'),method='ML',correlation = NULL)),silent = TRUE)
}}

if (Opt.Method == 2){
if (is.rho.zero==TRUE){
fit = try(summary(lme(Y ~ Z ,random = ~1 | subjno, data=data,
control=lmeControl(opt='optim'),method='REML',correlation = corAR1())),silent = TRUE)
}
if (is.rho.zero==FALSE){
fit = try(summary(lme(Y ~ Z ,random = ~1 | subjno, data=data,
control=lmeControl(opt='optim'),method='REML',correlation = NULL)),silent = TRUE)
}}
}

########################################################################################

# Simulate data from Model 2: Y ~ b00 + b01*W

if (Model == 2){

# Estimate Mixed Model Effects

if (Opt.Method == 1){
if (is.rho.zero==TRUE){
fit = try(summary(lme(Y ~ W ,random = ~1 | subjno, data=data,
control=lmeControl(opt='optim'),method='ML',correlation = corAR1())),silent = TRUE)
}
if (is.rho.zero==FALSE){
fit = try(summary(lme(Y ~ W ,random = ~1 | subjno, data=data,
control=lmeControl(opt='optim'),method='ML',correlation = NULL)),silent = TRUE)
}}

if (Opt.Method == 2){
if (is.rho.zero==TRUE){
fit = try(summary(lme(Y ~ W ,random = ~1 | subjno, data=data,
control=lmeControl(opt='optim'),method='REML',correlation = corAR1())),silent = TRUE)
}
if (is.rho.zero==FALSE){
fit = try(summary(lme(Y ~ W ,random = ~1 | subjno, data=data,
control=lmeControl(opt='optim'),method='REML',correlation = NULL)),silent = TRUE)
}}
}

########################################################################################

# Simulate data from Model 3: Y ~ b00 + b10*X

if (Model == 3){

# Estimate Mixed Model Effects

if (Opt.Method == 1){
if (is.rho.zero==TRUE){
fit = try(summary(lme(Y ~ X ,random = ~1 + X | subjno, data=data,
control=lmeControl(opt='optim'),method='ML',correlation = corAR1())),silent = TRUE)
}
if (is.rho.zero==FALSE){
fit = try(summary(lme(Y ~ X ,random = ~1 + X | subjno, data=data,
control=lmeControl(opt='optim'),method='ML',correlation = NULL)),silent = TRUE)
}}

if (Opt.Method == 2){
if (is.rho.zero==TRUE){
fit = try(summary(lme(Y ~ X ,random = ~1 + X | subjno, data=data,
control=lmeControl(opt='optim'),method='REML',correlation = corAR1())),silent = TRUE)
}
if (is.rho.zero==FALSE){
fit = try(summary(lme(Y ~ X ,random = ~1 + X | subjno, data=data,
control=lmeControl(opt='optim'),method='REML',correlation = NULL)),silent = TRUE)
}}
}

########################################################################################

# Simulate data from Model 4: Y ~ b00 + b10*X fixed slope

if (Model == 4){

# Estimate Mixed Model Effects

if (Opt.Method == 1){
if (is.rho.zero==TRUE){
fit = try(summary(lme(Y ~ X ,random = ~1 | subjno, data=data,
control=lmeControl(opt='optim'),method='ML',correlation = corAR1())),silent = TRUE)
}
if (is.rho.zero==FALSE){
fit = try(summary(lme(Y ~ X ,random = ~1 | subjno, data=data,
control=lmeControl(opt='optim'),method='ML',correlation = NULL)),silent = TRUE)
}}

if (Opt.Method == 2){
if (is.rho.zero==TRUE){
fit = try(summary(lme(Y ~ X ,random = ~1 | subjno, data=data,
control=lmeControl(opt='optim'),method='REML',correlation = corAR1())),silent = TRUE)
}
if (is.rho.zero==FALSE){
fit = try(summary(lme(Y ~ X ,random = ~1 | subjno, data=data,
control=lmeControl(opt='optim'),method='REML',correlation = NULL)),silent = TRUE)
}}
}

########################################################################################

# Simulate data from Model 5: Y ~ b00 + b01*Z + b10*X + b11*Z*X

if (Model == 5){

# Estimate Mixed Model Effects

if (Opt.Method == 1){
if (is.rho.zero==TRUE){
fit = try(summary(lme(Y ~ Z + X + Z*X ,random = ~1 + X | subjno, data=data,
control=lmeControl(opt='optim'),method='ML',correlation = corAR1())),silent = TRUE)
}
if (is.rho.zero==FALSE){
fit = try(summary(lme(Y ~ Z + X + Z*X ,random = ~1 + X | subjno, data=data,
control=lmeControl(opt='optim'),method='ML',correlation = NULL)),silent = TRUE)
}}

if (Opt.Method == 2){
if (is.rho.zero==TRUE){
fit = try(summary(lme(Y ~ Z + X + Z*X ,random = ~1 + X | subjno, data=data,
control=lmeControl(opt='optim'),method='REML',correlation = corAR1())),silent = TRUE)
}
if (is.rho.zero==FALSE){
fit = try(summary(lme(Y ~ Z + X + Z*X ,random = ~1 + X | subjno, data=data,
control=lmeControl(opt='optim'),method='REML',correlation = NULL)),silent = TRUE)
}}
}

########################################################################################

# Simulate data from Model 6: Y ~ b00 + b01*Z + b10*X + b11*Z*X fixed slope

if (Model == 6){

# Estimate Mixed Model Effects

if (Opt.Method == 1){
if (is.rho.zero==TRUE){
fit = try(summary(lme(Y ~ Z + X + Z*X ,random = ~1 | subjno, data=data,
control=lmeControl(opt='optim'),method='ML',correlation = corAR1())),silent = TRUE)
}
if (is.rho.zero==FALSE){
fit = try(summary(lme(Y ~ Z + X + Z*X ,random = ~1 | subjno, data=data,
control=lmeControl(opt='optim'),method='ML',correlation = NULL)),silent = TRUE)
}}

if (Opt.Method == 2){
if (is.rho.zero==TRUE){
fit = try(summary(lme(Y ~ Z + X + Z*X ,random = ~1 | subjno, data=data,
control=lmeControl(opt='optim'),method='REML',correlation = corAR1())),silent = TRUE)
}
if (is.rho.zero==FALSE){
fit = try(summary(lme(Y ~ Z + X + Z*X ,random = ~1 | subjno, data=data,
control=lmeControl(opt='optim'),method='REML',correlation = NULL)),silent = TRUE)
}}
}

########################################################################################

# Simulate data from Model 7: Y ~ b00 + b01*W + b10*X + b11*W*X

if (Model == 7){

# Estimate Mixed Model Effects

if (Opt.Method == 1){
if (is.rho.zero==TRUE){
fit = try(summary(lme(Y ~ W + X + W*X ,random = ~1 + X | subjno, data=data,
control=lmeControl(opt='optim'),method='ML',correlation = corAR1())),silent = TRUE)
}
if (is.rho.zero==FALSE){
fit = try(summary(lme(Y ~ W + X + W*X ,random = ~1 + X | subjno, data=data,
control=lmeControl(opt='optim'),method='ML',correlation = NULL)),silent = TRUE)
}}

if (Opt.Method == 2){
if (is.rho.zero==TRUE){
fit = try(summary(lme(Y ~ W + X + W*X ,random = ~1 + X | subjno, data=data,
control=lmeControl(opt='optim'),method='REML',correlation = corAR1())),silent = TRUE)
}
if (is.rho.zero==FALSE){
fit = try(summary(lme(Y ~ W + X + W*X ,random = ~1 + X | subjno, data=data,
control=lmeControl(opt='optim'),method='REML',correlation = NULL)),silent = TRUE)
}}
}

########################################################################################

# Simulate data from Model 8: Y ~ b00 + b01*W + b10*X + b11*W*X with fixed slope

if (Model == 8){

# Estimate Mixed Model Effects

if (Opt.Method == 1){
if (is.rho.zero==TRUE){
fit = try(summary(lme(Y ~ W + X + W*X ,random = ~1 | subjno, data=data,
control=lmeControl(opt='optim'),method='ML',correlation = corAR1())),silent = TRUE)
}
if (is.rho.zero==FALSE){
fit = try(summary(lme(Y ~ W + X + W*X ,random = ~1 | subjno, data=data,
control=lmeControl(opt='optim'),method='ML',correlation = NULL)),silent = TRUE)
}}

if (Opt.Method == 2){
if (is.rho.zero==TRUE){
fit = try(summary(lme(Y ~ W + X + W*X ,random = ~1 | subjno, data=data,
control=lmeControl(opt='optim'),method='REML',correlation = corAR1())),silent = TRUE)
}
if (is.rho.zero==FALSE){
fit = try(summary(lme(Y ~ W + X + W*X ,random = ~1 | subjno, data=data,
control=lmeControl(opt='optim'),method='REML',correlation = NULL)),silent = TRUE)
}}
}

########################################################################################

# Simulate data from Model 9: Y ~ b00 + b10*lag(Y)

if (Model == 9){

# Estimate Mixed Model Effects

# If Ylag.center is TRUE Mean centered lag varying variable per-individual
if (Ylag.center == TRUE){
N.subject = unique(data$subjno)
for (i in N.subject){
  data$Ylag[which(data$subjno==i)] = data$Ylag[which(data$subjno==i)] - mean(data$Y[which(data$subjno==i)])
}}

if (Opt.Method == 1){
fit = try(summary(lme(Y ~ Ylag, random = ~1 + Ylag | subjno, data=data,
na.action=na.omit,method='ML',control=lmeControl(opt='optim'))),silent = TRUE)
}

if (Opt.Method == 2){
fit = try(summary(lme(Y ~ Ylag, random = ~1 + Ylag | subjno, data=data,
na.action=na.omit,method='REML',control=lmeControl(opt='optim'))),silent = TRUE)
}
}

########################################################################################

# Simulate data from Model 10: Y ~ b00 + b01*Z + + b10*lag(Y) + + b11*Z*lag(Y)

if (Model == 10){

# Estimate Mixed Model Effects
if (Ylag.center == TRUE){
# If Ylag.center is TRUE Mean centered lag varying variable per-individual
N.subject = unique(data$subjno)
for (i in N.subject){
  data$Ylag[which(data$subjno==i)] = data$Ylag[which(data$subjno==i)] - mean(data$Y[which(data$subjno==i)])
}}

if (Opt.Method == 1){
fit = try(summary(lme(Y ~ Z + Ylag + Z*Ylag, random = ~1 + Ylag | subjno, data=data,
na.action=na.omit,method='ML',control=lmeControl(opt='optim'))),silent = TRUE)
}

if (Opt.Method == 2){
fit = try(summary(lme(Y ~ Z + Ylag + Z*Ylag, random = ~1 + Ylag | subjno, data=data,
na.action=na.omit,method='REML',control=lmeControl(opt='optim'))),silent = TRUE)
}
}

########################################################################################

# Simulate data from Model 11: Y ~ b00 + b01*W+ + b10*lag(Y) + + b11*W*lag(Y)

if (Model == 11){

# Estimate Mixed Model Effects
if (Ylag.center == TRUE){
# If Ylag.center is TRUE Mean centered lag varying variable per-individual
N.subject = unique(data$subjno)
for (i in N.subject){
  data$Ylag[which(data$subjno==i)] = data$Ylag[which(data$subjno==i)] - mean(data$Y[which(data$subjno==i)])
}}

if (Opt.Method == 1){
fit = try(summary(lme(Y ~ W + Ylag + W*Ylag, random = ~1 + Ylag | subjno, data=data,
na.action=na.omit,method='ML',control=lmeControl(opt='optim'))),silent = TRUE)
}

if (Opt.Method == 2){
fit = try(summary(lme(Y ~ W + Ylag + W*Ylag, random = ~1 + Ylag | subjno, data=data,
na.action=na.omit,method='REML',control=lmeControl(opt='optim'))),silent = TRUE)
}
}

return(fit)}
