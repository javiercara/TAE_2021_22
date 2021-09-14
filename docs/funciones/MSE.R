###########################
MSE = function(y,yp){
  
  y = as.numeric(y)
  yp = as.numeric(yp)
  
  n = length(y)
  d = (y - yp)^2
  suma = sum(d)
  MSE = 1/n*suma
  return(MSE)
}
###########################