###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################

# Function to simulate an IL data set

Sim.Data.IL = function(Model,N,N.0, N.1,T,isX.center,Ylag.center,isW.center, 
b00, b01.Z, b01.W, b10, b11.Z, b11.W, sigma, rho, sigma.v0, sigma.v1, 
rho.v, mu.W, sigma.W, mu.X, mu.X0, mu.X1, sigma.X, sigma.X0, sigma.X1){

library(MASS)
library(data.table) 
########################################################################################

# Simulate data from Model 1: Y ~ b00 + b01*Z

if (Model == 1){

# Total number of subjects
N = N.0 + N.1 
 
# Number of subjects in each group
n.group = c(rep(0,N.0),rep(1,N.1))

# Create variables days, beeps per day and Z
data.IL = expand.grid(Time=1:T,Z=n.group)

# Create variable subjno
subjno = expand.grid(1:T,1:N)[,2]
data.IL = cbind(subjno,data.IL)

Z = data.IL$Z

# Simulate error level-1 
if (rho == 0 | length(rho) == 0){E = rnorm(T*N,0,sigma)}
else{
AR.epsilon = list(order=c(1,0,0), ar=rho)
E = rep(0,T*N)
for (i in 1:N){
E[which(data.IL$subjno==i)] = arima.sim(n=T,AR.epsilon)*sigma*sqrt(1-rho^2)
}}

# Simulate error level-2
# Simulate between-subject random effect
V.i = rnorm(N,0,sigma.v0)
V = rep(0,T*N)
for (i in 1:N){V[which(data.IL$subjno==i)] = V.i[i]}

B00 = rep(b00,nrow(data.IL))
B01 = rep(b01.Z,nrow(data.IL))

# Simulate Dependent Variable
Y = B00 + B01*Z + V + E

# Create a data frame
data = data.frame(cbind(data.IL,Y))
}

########################################################################################

# Simulate data from Model 2: Y ~ b00 + b01*W 

if (Model == 2){

if (sigma.W<= 0) {stop('The variance of the time-invarying predictor must be positive')}

# Simulate level-2 variable W
W.i = rnorm(N,mu.W,sigma.W)

# Centered variable W
if (isW.center == TRUE){W.i = W.i - mean(W.i)}

# Create variables days, beeps per day and Z
data.IL = expand.grid(Time=1:T,W=W.i)

# Create variable subjno
subjno = expand.grid(1:T,1:N)[,2]
data.IL = cbind(subjno,data.IL)

W = data.IL$W

# Simulate error level-1 
if (rho == 0 | length(rho) == 0){E = rnorm(T*N,0,sigma)}
else{
AR.epsilon = list(order=c(1,0,0), ar=rho)
E = rep(0,T*N)
for (i in 1:N){
E[which(data.IL$subjno==i)] = arima.sim(n=T,AR.epsilon)*sigma*sqrt(1-rho^2)
}}

# Simulate error level-2
# Simulate between-subject random effect
V.i = rnorm(N,0,sigma.v0)
V = rep(0,T*N)
for (i in 1:N){V[which(data.IL$subjno==i)] = V.i[i]}

# Set parameters and variables
B00 = rep(b00,nrow(data.IL))
B01 = rep(b01.W,nrow(data.IL))

# Simulate Dependent Variable
Y = B00 + B01*W + V + E


# Create a data frame
data = data.frame(cbind(data.IL,Y))
}

########################################################################################

# Simulate data from Model 3: Y ~ b00 + b10*X with random slope

if (Model == 3){

# Total number of subjects
N = N

# Create variables days, beeps per day and Z
data.IL = expand.grid(Time=1:T,subjno=1:N)

# Simulate error level-1 
if (rho == 0 | length(rho) == 0){E = rnorm(T*N,0,sigma)}
else{
AR.epsilon = list(order=c(1,0,0), ar=rho)
E = rep(0,T*N)
for (i in 1:N){
E[which(data.IL$subjno==i)] = arima.sim(n=T,AR.epsilon)*sigma*sqrt(1-rho^2)
}}

# Simulate error level-2
# Simulate between-subject random effect
Sigma.v = diag(c(sigma.v0^2,sigma.v1^2))
Sigma.v[lower.tri(Sigma.v)] = rho.v*sigma.v0*sigma.v1
Sigma.v[upper.tri(Sigma.v)] = rho.v*sigma.v0*sigma.v1

if (eigen(Sigma.v)$values[2] <= 0) {stop('The covariance matrix of the level-2 errors must be positive definite')}

V.i = mvrnorm(N,rep(0,ncol(Sigma.v)),Sigma.v)
V = matrix(0,T*N,2)
for (i in 1:N){
  V[which(data.IL$subjno==i),1] = V.i[i,1]
  V[which(data.IL$subjno==i),2] = V.i[i,2]
}

# Simulate time varying variable X
if (sigma.X<= 0) {stop('The variance of the time-varying predictor must be positive')}
X = rep(0,N*T)
for (i in 1:N){
  X[which(data.IL$subjno==i)] = rnorm(T,mean=mu.X,sd=sigma.X)
}

B00 = rep(b00,nrow(data.IL))
B10 = rep(b10,nrow(data.IL))

if (isX.center==TRUE){
# Mean centered time varying variable per-individual
for (i in 1:N){
  X[which(data.IL$subjno==i)] = X[which(data.IL$subjno==i)] - mean(X[which(data.IL$subjno==i)])
}}

# Compute Dependent Variable
Y = B00 + B10*X + V[,1] + V[,2]*X + E

# Create a data frame
data = data.frame(cbind(data.IL,Y,X)) 
}


########################################################################################

# Simulate data from Model 4: Y ~ b00 + b10*X with fixed slope

if (Model == 4){

# Total number of subjects
N = N

# Create variables days, beeps per day and Z
data.IL = expand.grid(Time=1:T,subjno=1:N)

# Simulate error level-1 
if (rho == 0 | length(rho) == 0){E = rnorm(T*N,0,sigma)}
else{
AR.epsilon = list(order=c(1,0,0), ar=rho)
E = rep(0,T*N)
for (i in 1:N){
E[which(data.IL$subjno==i)] = arima.sim(n=T,AR.epsilon)*sigma*sqrt(1-rho^2)
}}

# Simulate error level-2
# Simulate between-subject random effect
if (sigma.v0<= 0) {stop('The variance of the random intercept must be positive')}
V.i = rnorm(N,0,sigma.v0)
V = rep(0,T*N)
for (i in 1:N){V[which(data.IL$subjno==i)] = V.i[i]}


# Simulate time varying variable X
if (sigma.X<= 0) {stop('The variance of the time-varying predictor must be positive')}
X = rep(0,N*T)
for (i in 1:N){
  X[which(data.IL$subjno==i)] = rnorm(T,mean=mu.X,sd=sigma.X)
}

B00 = rep(b00,nrow(data.IL))
B10 = rep(b10,nrow(data.IL))

if (isX.center == TRUE){
# Mean centered time varying variable per-individual
for (i in 1:N){
  X[which(data.IL$subjno==i)] = X[which(data.IL$subjno==i)] - mean(X[which(data.IL$subjno==i)])
}}

# Compute Dependent Variable
Y = B00 + B10*X + V + E

# Create a data frame
data = data.frame(cbind(data.IL,Y,X)) 
}


########################################################################################

# Simulate data from Model 5: Y ~ b00 + b01*Z + b10*X + b11*Z*X

if (Model == 5){

# Total number of subjects
N = N.0 + N.1 
 
# Number of subjects in each group
n.group = c(rep(0,N.0),rep(1,N.1))

# Create variables days, beeps per day and Z
data.IL = expand.grid(Time=1:T,Z=n.group)

# Create variable subjno
subjno = expand.grid(1:T,1:N)[,2]
data.IL = cbind(subjno,data.IL)

Z = data.IL$Z

# Simulate error level-1 
if (rho == 0 | length(rho) == 0){E = rnorm(T*N,0,sigma)}
else{
AR.epsilon = list(order=c(1,0,0), ar=rho)
E = rep(0,T*N)
for (i in 1:N){
E[which(data.IL$subjno==i)] = arima.sim(n=T,AR.epsilon)*sigma*sqrt(1-rho^2)
}}

# Simulate error level-2
# Simulate between-subject random effect
Sigma.v = diag(c(sigma.v0^2,sigma.v1^2))
Sigma.v[lower.tri(Sigma.v)] = rho.v*sigma.v0*sigma.v1
Sigma.v[upper.tri(Sigma.v)] = rho.v*sigma.v0*sigma.v1

if (eigen(Sigma.v)$values[2] <= 0) {stop('The covariance matrix of the level-2 errors must be positive definite')}

V.i = mvrnorm(N,rep(0,ncol(Sigma.v)),Sigma.v)
V = matrix(0,T*N,2)
for (i in 1:N){
  V[which(data.IL$subjno==i),1] = V.i[i,1]
  V[which(data.IL$subjno==i),2] = V.i[i,2]
}

# Simulate time varying variable X
if (sigma.X0<= 0) {stop('The variance of the time-varying predictor in Group 0 must be positive')}
if (sigma.X1<= 0) {stop('The variance of the time-varying predictor in Group 1 must be positive')}

X = rep(0,N*T)
for (i in 1:N){
  X[which(data.IL$subjno==i)] = ifelse(Z[which(data.IL$subjno==i)]==0,rnorm(T,mean=mu.X0,sd=sigma.X0),
  rnorm(T,mean=mu.X1,sd=sigma.X1))
}

# Set parameters and variables
B00 = rep(b00,nrow(data.IL))
B01 = rep(b01.Z,nrow(data.IL))
B10 = rep(b10,nrow(data.IL))
B11 = rep(b11.Z,nrow(data.IL))

if (isX.center == TRUE){
# Mean centered time varying variable per-individual
for (i in 1:N){
  X[which(data.IL$subjno==i)] = X[which(data.IL$subjno==i)] - mean(X[which(data.IL$subjno==i)])
}}

# Compute Dependent Variable
Y = B00 + B01*Z + B10*X + B11*Z*X + V[,1] + V[,2]*X + E

# Create a data frame
data = data.frame(cbind(data.IL,Y,X))
}

########################################################################################

# Simulate data from Model 6: Y ~ b00 + b01*Z + b10*X + b11*Z*X with fixed slope

if (Model == 6){

# Total number of subjects
N = N.0 + N.1 
 
# Number of subjects in each group
n.group = c(rep(0,N.0),rep(1,N.1))

# Create variables days, beeps per day and Z
data.IL = expand.grid(Time=1:T,Z=n.group)

# Create variable subjno
subjno = expand.grid(1:T,1:N)[,2]
data.IL = cbind(subjno,data.IL)

Z = data.IL$Z

# Simulate error level-1 
if (rho == 0 | length(rho) == 0){E = rnorm(T*N,0,sigma)}
else{
AR.epsilon = list(order=c(1,0,0), ar=rho)
E = rep(0,T*N)
for (i in 1:N){
E[which(data.IL$subjno==i)] = arima.sim(n=T,AR.epsilon)*sigma*sqrt(1-rho^2)
}}

# Simulate error level-2
# Simulate between-subject random effect
if (sigma.v0<= 0) {stop('The variance of the random intercept must be positive')}
V.i = rnorm(N,0,sigma.v0)
V = rep(0,T*N)
for (i in 1:N){V[which(data.IL$subjno==i)] = V.i[i]}

# Simulate time varying variable X
if (sigma.X0<= 0) {stop('The variance of the time-varying predictor in Group 0 must be positive')}
if (sigma.X1<= 0) {stop('The variance of the time-varying predictor in Group 1 must be positive')}

X = rep(0,N*T)
for (i in 1:N){
  X[which(data.IL$subjno==i)] = ifelse(Z[which(data.IL$subjno==i)]==0,rnorm(T,mean=mu.X0,sd=sigma.X0),
  rnorm(T,mean=mu.X1,sd=sigma.X1))
}

# Set parameters and variables
B00 = rep(b00,nrow(data.IL))
B01 = rep(b01.Z,nrow(data.IL))
B10 = rep(b10,nrow(data.IL))
B11 = rep(b11.Z,nrow(data.IL))

if (isX.center == TRUE){
# Mean centered time varying variable per-individual
for (i in 1:N){
  X[which(data.IL$subjno==i)] = X[which(data.IL$subjno==i)] - mean(X[which(data.IL$subjno==i)])
}}

# Compute Dependent Variable
Y = B00 + B01*Z + B10*X + B11*Z*X + V + E

# Create a data frame
data = data.frame(cbind(data.IL,Y,X))
}

########################################################################################

# Simulate data from Model 7: Y ~ b00 + b01*W + b10*X + b11*W*X

if (Model == 7){

if (sigma.W<= 0) {stop('The variance of the time-invarying predictor must be positive')}

# Simulate level-2 variable W
W.i = rnorm(N,mu.W,sigma.W)

# Centered variable W
if (isW.center == TRUE){W.i = W.i - mean(W.i)}

# Create variables days, beeps per day and Z
data.IL = expand.grid(Time=1:T,W=W.i)

# Create variable subjno
subjno = expand.grid(1:T,1:N)[,2]
data.IL = cbind(subjno,data.IL)

W = data.IL$W

# Simulate error level-1 
if (rho == 0 | length(rho) == 0){E = rnorm(T*N,0,sigma)}
else{
AR.epsilon = list(order=c(1,0,0), ar=rho)
E = rep(0,T*N)
for (i in 1:N){
E[which(data.IL$subjno==i)] = arima.sim(n=T,AR.epsilon)*sigma*sqrt(1-rho^2)
}}

# Simulate error level-2
# Simulate between-subject random effect
Sigma.v = diag(c(sigma.v0^2,sigma.v1^2))
Sigma.v[lower.tri(Sigma.v)] = rho.v*sigma.v0*sigma.v1
Sigma.v[upper.tri(Sigma.v)] = rho.v*sigma.v0*sigma.v1

if (eigen(Sigma.v)$values[2] <= 0) {stop('The covariance matrix of the level-2 errors must be positive definite')}

V.i = mvrnorm(N,rep(0,ncol(Sigma.v)),Sigma.v)
V = matrix(0,T*N,2)
for (i in 1:N){
  V[which(data.IL$subjno==i),1] = V.i[i,1]
  V[which(data.IL$subjno==i),2] = V.i[i,2]
}

# Simulate time varying variable X
if (sigma.X<= 0) {stop('The variance of the time-varying predictor must be positive')}
X = rep(0,N*T)
for (i in 1:N){
  X[which(data.IL$subjno==i)] = rnorm(T,mean=mu.X,sd=sigma.X)
}

# Set parameters and variables
B00 = rep(b00,nrow(data.IL))
B01 = rep(b01.W,nrow(data.IL))
B10 = rep(b10,nrow(data.IL))
B11 = rep(b11.W,nrow(data.IL))

if (isX.center == TRUE){
# Mean centered time varying variable per-individual
for (i in 1:N){
  X[which(data.IL$subjno==i)] = X[which(data.IL$subjno==i)] - mean(X[which(data.IL$subjno==i)])
}}

# Simulate Dependent Variable
Y = B00 + B01*W + B10*X + B11*W*X + V[,1] + V[,2]*X + E


# Create a data frame
data = data.frame(cbind(data.IL,Y,X))
}

########################################################################################

# Simulate data from Model 8: Y ~ b00 + b01*W + b10*X + b11*W*X with fixed slope

if (Model == 8){

if (sigma.W<= 0) {stop('The variance of the time-invarying predictor must be positive')}

# Simulate level-2 variable W
W.i = rnorm(N,mu.W,sigma.W)

# Centered variable W
if (isW.center == TRUE){W.i = W.i - mean(W.i)}

# Create variables days, beeps per day and Z
data.IL = expand.grid(Time=1:T,W=W.i)

# Create variable subjno
subjno = expand.grid(1:T,1:N)[,2]
data.IL = cbind(subjno,data.IL)

W = data.IL$W

# Simulate error level-1 
if (rho == 0 | length(rho) == 0){E = rnorm(T*N,0,sigma)}
else{
AR.epsilon = list(order=c(1,0,0), ar=rho)
E = rep(0,T*N)
for (i in 1:N){
E[which(data.IL$subjno==i)] = arima.sim(n=T,AR.epsilon)*sigma*sqrt(1-rho^2)
}}

# Simulate error level-2
# Simulate between-subject random effect
if (sigma.v0<= 0) {stop('The variance of the random intercept must be positive')}
V.i = rnorm(N,0,sigma.v0)
V = rep(0,T*N)
for (i in 1:N){V[which(data.IL$subjno==i)] = V.i[i]}

# Simulate time varying variable X
if (sigma.X<= 0) {stop('The variance of the time-varying predictor must be positive')}
X = rep(0,N*T)
for (i in 1:N){
  X[which(data.IL$subjno==i)] = rnorm(T,mean=mu.X,sd=sigma.X)
}

# Set parameters and variables
B00 = rep(b00,nrow(data.IL))
B01 = rep(b01.W,nrow(data.IL))
B10 = rep(b10,nrow(data.IL))
B11 = rep(b11.W,nrow(data.IL))

if (isX.center == TRUE){
# Mean centered time varying variable per-individual
for (i in 1:N){
  X[which(data.IL$subjno==i)] = X[which(data.IL$subjno==i)] - mean(X[which(data.IL$subjno==i)])
}}

# Simulate Dependent Variable
Y = B00 + B01*W + B10*X + B11*W*X + V + E

# Create a data frame
data = data.frame(cbind(data.IL,Y,X))
}

########################################################################################

# Simulate data from Model 9: Y ~ b00 + b10*lag(Y) 

if (Model == 9){

# Total number of subjects
N = N

# Create variables days, beeps per day and Z
data.IL = expand.grid(Time=1:T,subjno=1:N)

# Simulate error level-2
# Simulate between-subject random effect
Sigma.v = diag(c(sigma.v0^2,sigma.v1^2))
Sigma.v[lower.tri(Sigma.v)] = rho.v*sigma.v0*sigma.v1
Sigma.v[upper.tri(Sigma.v)] = rho.v*sigma.v0*sigma.v1

if (eigen(Sigma.v)$values[2] <= 0) {stop('The covariance matrix of the level-2 errors must be positive definite')}

# Simulate Y variable as an AR(1) process for each individual

Y = rep(0,T*N)

# Parameters of random intercept and random slope are generated from a Beta distribution
# Stationarity condition: sigma.v1 < sqrt(1-b10^2)
if (sigma.v1 > sqrt(1-b10^2)) {stop('To ensure that the model is stationary check that standard deviation of the random 
slope is smaller than sqrt(1-b10^2) where b10 is the fixed autorregressive effect')}

mu.beta = (b10+1)/2
sigma.beta = sigma.v1/2
alpha.beta = mu.beta^2*(((1-mu.beta)/sigma.beta^2) - (1/mu.beta)) 
beta.beta =  alpha.beta*((1/mu.beta) - 1)
gamma.1 = rbeta(N, alpha.beta, beta.beta)*2-1
gamma.0 = sigma.v0*(rho.v*scale(gamma.1)+sqrt(1-rho.v^2)*rnorm(N)) + rep(b00,N)

if (Ylag.center == TRUE){
for (i in 1:N){
AR.epsilon = list(order=c(1,0,0), ar=gamma.1[i], include.mean = FALSE)
Y[which(data.IL$subjno==i)] = arima.sim(n=T,AR.epsilon)*sqrt(1-gamma.1[i]^2)*sigma + gamma.0[i]}
}

if (Ylag.center == FALSE){
for (i in 1:N){
AR.epsilon = list(order=c(1,0,0), ar=gamma.1[i], include.mean = FALSE)
Y[which(data.IL$subjno==i)] = arima.sim(n=T,AR.epsilon)*sqrt(1-gamma.1[i]^2)*sigma + gamma.0[i]/(1-gamma.1[i])
}}

# Create a data frame
data = data.frame(cbind(data.IL,Y)) 

# Create lag Y variable within days for each individual
Ylag = lag.Y(data)
data = data.frame(cbind(data,Ylag))
}

########################################################################################

# Simulate data from Model 10: Y ~ b00 + b01*Z + b10*lag(Y) + b11*Z*lag(Y)

if (Model == 10){

# Total number of subjects
N = N.0 + N.1 

# Number of subjects in each group
n.group = c(rep(0,N.0),rep(1,N.1))

# Create variables days, beeps per day and Z
data.IL = expand.grid(Time=1:T,Z=n.group)

# Create variable subjno
subjno = expand.grid(1:T,1:N)[,2]
data.IL = cbind(subjno,data.IL)

Z = data.IL$Z

# Simulate error level-2
# Simulate between-subject random effect
Sigma.v = diag(c(sigma.v0^2,sigma.v1^2))
Sigma.v[lower.tri(Sigma.v)] = rho.v*sigma.v0*sigma.v1
Sigma.v[upper.tri(Sigma.v)] = rho.v*sigma.v0*sigma.v1

if (eigen(Sigma.v)$values[2] <= 0) {stop('The covariance matrix of the level-2 errors must be positive definite')}

# Simulate Y variable as an AR(1) process for each individual

Y = rep(0,T*N)

# Parameters of random intercept and random slope are generated from a Beta distribution
# Parameters of Group=0
# Stationarity condition: sigma.v1 < sqrt(1-b10^2)
if (sigma.v1 > sqrt(1-b10^2)) {stop('To ensure that the model in Group 0 is stationary check that standard deviation of the random 
slope is smaller than sqrt(1-b10^2) where b10 is the fixed autorregressive effect')}

mu.beta.0 = (b10+1)/2
sigma.beta.0 = sigma.v1/2
alpha.beta.0 = mu.beta.0^2*(((1-mu.beta.0)/sigma.beta.0^2) - (1/mu.beta.0)) 
beta.beta.0 =  alpha.beta.0*((1/mu.beta.0) - 1)
gamma.01 = rbeta(N.0, alpha.beta.0, beta.beta.0)*2-1
gamma.00 = sigma.v0*(rho.v*scale(gamma.01)+sqrt(1-rho.v^2)*rnorm(N.0)) + rep(b00,N.0)

# Parameters of Group=1
# Stationarity condition: sigma.v1 < sqrt(1-(b10+b11.Z)^2)
if (sigma.v1 > sqrt(1-(b10+b11.Z)^2)) {stop('To ensure that the model in Group 1 is stationary check that standard deviation of the random 
slope is smaller than sqrt(1-(b10+b11)^2) where (b10+b11) is the fixed autorregressive effect')}

mu.beta.1 = (b10+b11.Z+1)/2
sigma.beta.1 = sigma.v1/2
alpha.beta.1 = mu.beta.1^2*(((1-mu.beta.1)/sigma.beta.1^2) - (1/mu.beta.1)) 
beta.beta.1 =  alpha.beta.1*((1/mu.beta.1) - 1)
gamma.11 = rbeta(N.1, alpha.beta.1, beta.beta.1)*2-1
gamma.10 = sigma.v0*(rho.v*scale(gamma.11)+sqrt(1-rho.v^2)*rnorm(N.1)) + rep(b00+b01.Z,N.1)

gamma.0 = c(gamma.00,gamma.10)
gamma.1 = c(gamma.01,gamma.11)

if (Ylag.center == TRUE){
for (i in 1:N){
AR.epsilon = list(order=c(1,0,0), ar=gamma.1[i], include.mean = FALSE)
Y[which(data.IL$subjno==i)] = arima.sim(n=T,AR.epsilon)*sqrt(1-gamma.1[i]^2)*sigma + gamma.0[i]
}}

if (Ylag.center == FALSE){
for (i in 1:N){
AR.epsilon = list(order=c(1,0,0), ar=gamma.1[i], include.mean = FALSE)
Y[which(data.IL$subjno==i)] = arima.sim(n=T,AR.epsilon)*sqrt(1-gamma.1[i]^2)*sigma + gamma.0[i]/(1-gamma.1[i])
}}

# Create a data frame
data = data.frame(cbind(data.IL,Y)) 

# Create lag Y variable within days for each individual
Ylag = lag.Y(data)
data = data.frame(cbind(data,Ylag))
}

########################################################################################

# Simulate data from Model 11: Y ~ b00 + b01*W + b10*lag(Y) + b11*W*lag(Y)

if (Model == 11){

# Simulate level-2 variable W
W.i = rnorm(N,mu.W,sigma.W)

# Centered variable W
if (isW.center == TRUE){W.i = W.i - mean(W.i)}

# Create variables days, beeps per day and Z
data.IL = expand.grid(Time=1:T,W=W.i)

# Create variable subjno
subjno = expand.grid(1:T,1:N)[,2]
data.IL = cbind(subjno,data.IL)

W = data.IL$W

# Simulate error level-2
# Simulate between-subject random effect with a cross-level interaction

# Simulate error level-2
# Simulate between-subject random effect
Sigma.v = diag(c(sigma.v0^2,sigma.v1^2))
Sigma.v[lower.tri(Sigma.v)] = rho.v*sigma.v0*sigma.v1
Sigma.v[upper.tri(Sigma.v)] = rho.v*sigma.v0*sigma.v1

if (eigen(Sigma.v)$values[2] <= 0) {stop('The covariance matrix of the level-2 random effects must be positive definite')}

# Simulate Y variable as an AR(1) process for each individual

Y = rep(0,T*N)

# Stationarity condition: sigma.v1 < sqrt(1-(b10+b11.W*max(abs(W.i)))^2))

if ((1-max((b10+b11.W*W.i)^2))<0){stop('To ensure that the model is 
stationary check that standard deviation of the random slope is smaller than 
the maximum sqrt(1-(b10+b11*W.i)^2) where (b10+b11*W.i) is the fixed 
autorregressive effect for the i-th participant')}

if ((1-max((b10+b11.W*W.i)^2))>0){

b11.W.max = max(sqrt(1-(b10+b11.W*W.i)^2))

if (sigma.v1 > b11.W.max) {stop('To ensure that the model is 
stationary check that standard deviation of the random slope is smaller than 
the maximum sqrt(1-(b10+b11*W.i)^2) where (b10+b11*W.i) is the fixed 
autorregressive effect for the i-th participant')}
}

for (i in 1:N){
# Parameters of random intercept and random slope are generated from a Beta distribution
mu.beta.1 = (b10+b11.W*W.i[i]+1)/2
sigma.beta.1 = sigma.v1/2
alpha.beta.1 = mu.beta.1^2*(((1-mu.beta.1)/sigma.beta.1^2) - (1/mu.beta.1)) 
beta.beta.1 =  alpha.beta.1*((1/mu.beta.1) - 1)
gamma.1 = rbeta(1, alpha.beta.1, beta.beta.1)*2-1
gamma.0 = sigma.v0*((rho.v*(gamma.1-(b10+b11.W*W.i[i]))/sigma.v1) +
sqrt(1-rho.v^2)*rnorm(1)) + b00+b01.W*W.i[i]

if (Ylag.center == TRUE){
AR.epsilon = list(order=c(1,0,0), ar=gamma.1, include.mean = FALSE)
Y[which(data.IL$subjno==i)] = arima.sim(n=T,AR.epsilon)*sqrt(1-gamma.1^2)*sigma + gamma.0
}

if (Ylag.center == FALSE){
AR.epsilon = list(order=c(1,0,0), ar=gamma.1, include.mean = FALSE)
Y[which(data.IL$subjno==i)] = arima.sim(n=T,AR.epsilon)*sqrt(1-gamma.1^2)*sigma + gamma.0/(1-gamma.1)
}}

# Create a data frame
data = data.frame(cbind(data.IL,Y)) 

# Create lag Y variable within days for each individual
Ylag = lag.Y(data)
data = data.frame(cbind(data,Ylag))
}

# End of function ---> Return simulated IL data set

return(data)}
