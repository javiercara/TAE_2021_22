# Función de verosimilitud
logL = function(beta,y,X){
  # asumimos que beta es un vector 
  # beta = [beta0 beta1 .. betak]
  
  n = length(y)
  suma = 0
  for (i in 1:n){
    suma = suma + y[i]*sum(X[i,]*beta) - 
      log(1 + exp( sum(t(X[i,])*beta) ))
  }
  return(suma)
}
#----------------------------------------------------------
# gradiente de la función de verosimilitud
grad_logL = function(beta,y,X){
  X = as.matrix(X)
  n = length(y)
  y = matrix(y, nrow = n, ncol = 1)
  pi = matrix(0, nrow = n, ncol = 1)
  for (i in 1:n){
    pi[i,1] = exp(sum(X[i,]*beta))/(1 + exp(sum(X[i,]*beta)))
  }
  grad = t(X) %*% (y - pi)
  return(grad)
}
#--------------------------------------------------------
# Hessiano de la matriz de verosimilitud
hess_logL = function(beta,X){
  X = as.matrix(X)
  n = nrow(X)
  W = matrix(0, nrow = n, ncol = n)
  for (i in 1:n){
    pi = exp(sum(X[i,]*beta))/(1 + exp(sum(X[i,]*beta)))
    W[i,i] = pi*(1-pi)
  }
  hess = - t(X) %*% W %*% X
  return(hess)
}
# -----------------------------------------------------------
logL_optim = function(beta,y,X){
  logL = logL(beta,y,X)
  return(-logL)
}



