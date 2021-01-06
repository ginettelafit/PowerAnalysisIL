library(htmltools)
library(shiny)
library(DT)
library(nlme)
library(ggplot2)
library(gridExtra)
library(data.table)
library(plyr)
library(dplyr)
library(formattable)
library(tidyr)
library(MASS)
library(shinyjs)
library(compiler)
library(future.apply)

library(devtools)

library(PowerAnalysisIL)

# Simulate pilot data to estimate differences in NA between participants with psychosis and controls

# Set parameters for simulating data from Model 1 
Model = 1
N = NULL
N.0 = 60
N.1 = 40
T = 70
isX.center = NULL
Ylag.center = NULL
isW.center = NULL 
b00 = 1
b01.Z = 0.3
b01.W = NULL
b10 = NULL
b11.Z = NULL
b11.W = NULL
sigma = 1
rho = 0.3
sigma.v0 = 0.5
sigma.v1 = NULL 
rho.v = NULL
mu.W = NULL
sigma.W = NULL
mu.X = NULL
mu.X0 = NULL
mu.X1 = NULL
sigma.X = NULL
sigma.X0 = NULL
sigma.X1 = NULL

data.Sim = Sim.Data.IL(Model,N,N.0, N.1,T,isX.center,Ylag.center,isW.center, 
b00, b01.Z, b01.W, b10, b11.Z, b11.W, sigma, rho, sigma.v0, sigma.v1, 
rho.v, mu.W, sigma.W, mu.X, mu.X0, mu.X1, sigma.X, sigma.X0, sigma.X1)

n.beeps = 10
n.days = 7

# Create variables days, beeps per day and Z
data.ESM = expand.grid(Beep=1:n.beeps,Day=1:n.days,1:(N.0+N.1))[,1:2]

# Create data set

data_pilot = data.frame(cbind(id=data.Sim$subjno,day=data.ESM$Day,beep=data.ESM$Beep,
obs=data.Sim$Time,NegAff=data.Sim$Y,
Diagnosis=data.Sim$Z))

data_pilot$NegAff = round(data_pilot$NegAff)+1
data_pilot$NegAff = ifelse(data_pilot$NegAff>7,7,data_pilot$NegAff)

summary(data_pilot)

write.table(data_pilot, file='data_pilot.txt',row.names = FALSE)

fit.Model = lme(NegAff ~ Diagnosis, random = ~ 1|id,
na.action=na.omit, data=data_pilot, method="REML",correlation=corAR1())
summary(fit.Model)


PowerAnalysisIL::RunShiny()