################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

## Function to generate data from a Multilevel AR(1) Model which allows to investigate 
## differences in the autoregressive effect between two groups of persons

Sim.AR.Model.Group.Diff = function(N.0, N.1,T.obs,Ylag.center, 
                        b00, b01.Z, b10, b11.Z, 
                        sigma, sigma.v0, sigma.v1, rho.v){
  
  # Create number of observations: T.obs + T.burning
  T.burning = 1000
  T.total = T.burning + T.obs
  
  # Total number of subjects
  N = N.0 + N.1 
  
  # Number of subjects in each group
  n.group = c(rep(0,N.0),rep(1,N.1))
  
  # Create variables days, beeps per day and Z
  data.IL = expand.grid(Time=1:T.total,Z=n.group)
  
  # Create variable subjno
  subjno = expand.grid(1:T.total,1:N)[,2]
  data.IL = cbind(subjno,data.IL)
  
  Z = data.IL$Z
  
  # Simulate error within-person errors
  E = rnorm(T.total*N,0,sigma)
  
  # Simulate error level-2
  # Simulate between-subject random effect
  Sigma.nu = cbind(c(sigma.v0^2,rho.v*sigma.v0*sigma.v1),c(rho.v*sigma.v0*sigma.v1,sigma.v1^2))
  
  # Ensure the model is stationary
  V.j = matrix(0,N,ncol(Sigma.nu))
  colnames(V.j) = c('V.0','V.1')
  
  for (i in 1:N){
    lambda.max = 1 
    iter = 1
    while(abs(lambda.max)>=1){
      nu.i = mvrnorm(1,rep(0,ncol(Sigma.nu)),Sigma.nu)
      names(nu.i) = c('V.0','V.1')
      Psi.i = c(b10+b11.Z*n.group[i]+nu.i['V.1'])
      lambda.max = Psi.i
      iter = iter + 1
    }
    V.j[i,] = nu.i
  }
  
  V = NULL
  for (j in 1:N){
    V = rbind(V,matrix(unlist(lapply(1:ncol(Sigma.nu), function(p) rep(V.j[j,p],T.total))), ncol=ncol(Sigma.nu), byrow=F))
  }
  colnames(V) = c('V.0','V.1')
  
  # Function to get recursive equation
  data.Y = cbind(expand.grid(Obs=1:T.total,subjno=1:N))
  Y = rep(0,T.total*N)
  n.ID = unique(data.Y$subjno)
  for (i in n.ID){
    T.obs.i = which(data.Y$subjno==i) 
    # Initialized values
    Y[T.obs.i[1]] = b00 + +b01.Z*n.group[i] + V[T.obs.i[1],'V.0'] + E[T.obs.i[1]]
    
    for (t in T.obs.i[-1]){
      # Simulate Dependent Variables
      Y[t] = b00 + b01.Z*n.group[i] + ((b10+b11.Z*n.group[i]))*Y[t-1] + V[t,'V.0'] + V[t,'V.1']*Y[t-1] + E[t]
    }}
  
  data.Y = cbind(data.IL,Y)
  
  T.total.i = NULL
  for (i in n.ID){
    T.total.i = c(T.total.i,which(data.Y$subjno==i)[-seq(1:T.burning)])  
  }
  
  # Create a data frame for T.obs
  data.Y = data.Y[T.total.i,]
  
  # Create lag variable
  Ylag = rep(0,nrow(data.Y))
  n.subject = unique(data.Y$subjno)
  for (j in n.subject){
    Ylag[which(data.Y$subjno==j)] = data.table::shift(data.Y$Y[which(data.Y$subjno==j)])
  }
  
  data = cbind(data.Y,Ylag=Ylag)
  
  return(data=data)
}
