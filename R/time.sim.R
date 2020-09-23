time.sim = function(Model, N, N.0, N.1, T, 
b00, b01.Z, b01.W, b10, b11.Z, b11.W, 
sigma, rho, sigma.v0, sigma.v1,rho.v, 
mu.W, sigma.W, mu.X, mu.X0, mu.X1, sigma.X, 
sigma.X0, sigma.X1,
is.rho.zero,isW.center,isX.center,Ylag.center, 
alpha,R,Opt.Method){

start = Sys.time() 
Sim.model.IL(Model, N, N.0, N.1, T, 
b00, b01.Z, b01.W, b10, b11.Z, b11.W, 
sigma, rho, sigma.v0, sigma.v1,rho.v, 
mu.W, sigma.W, mu.X, mu.X0, mu.X1, sigma.X, 
sigma.X0, sigma.X1,
is.rho.zero,isW.center,isX.center,Ylag.center, 
alpha,10,Opt.Method)
end = Sys.time() 

time.hat = round(as.numeric(end-start)*R/(10*3600),3)
time.sim = paste('The estimated computational time is',time.hat,'hours')

return(list(time.sim=time.sim,time.hat=time.hat))
}
