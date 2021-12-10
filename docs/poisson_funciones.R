# lambda
lambda = function(beta,x){
  lambda_i = exp(sum(beta*x))
  return(lambda_i)
}
# Función de verosimilitud
logL = function(beta,y,X){
  # asumimos que beta es un vector 
  # beta = [beta0 beta1 .. betak]
  
  n = length(y)
  suma = 0
  for (i in 1:n){
    lambda_i = lambda(beta,X[i,])
    suma = suma + y[i]*log(lambda_i) - lambda_i - log(factorial(y[i])) 
  }
  return(suma)
}
#----------------------------------------------------------
# gradiente de la función de verosimilitud
grad_logL = function(beta,y,X){
  X = as.matrix(X)
  n = length(y)
  y = matrix(y, nrow = n, ncol = 1)
  lambda = matrix(0, nrow = n, ncol = 1)
  for (i in 1:n){
    lambda[i,1] = lambda(beta,X[i,])
  }
  grad = t(X) %*% (y - lambda)
  return(grad)
}
#--------------------------------------------------------
# Hessiano de la matriz de verosimilitud
hess_logL = function(beta,X){
  X = as.matrix(X)
  n = nrow(X)
  W = matrix(0, nrow = n, ncol = n)
  for (i in 1:n){
    W[i,i] = lambda(beta,X[i,])
  }
  hess = - t(X) %*% W %*% X
  return(hess)
}
# -----------------------------------------------------------
logL_optim = function(beta,y,X){
  logL = logL(beta,y,X)
  return(-logL)
}



