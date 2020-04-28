###############################################################
###############################################################
###############################################################

# Create a lag variable
# the data is lag within person and within days

lag.Y = function(data){ 

Y_lag = rep(0,nrow(data))
subjno.i = unique(data$subjno)
for (i in subjno.i){
n.i = which(data$subjno==i)
Y_lag[n.i] = shift(data$Y[n.i],1)
}

return(Y_lag)
}
