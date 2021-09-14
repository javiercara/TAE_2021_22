predict.regsubsets = function(objeto_regsubsets, newdata, id){
  # funcion para predecir con regsubsets
  # objeto_regsubsets: objeto calculado con la funcion regbsubsets()
  # newdata: datos para predecir (data.frame)
  # id: numero de variables incluidas en el modelo objeto_regsubsets
  # ----------------------------------------------------------
  
  formu = as.formula(objeto_regsubsets$call[[2]])
  X_mat = model.matrix(formu,newdata)
  coefi = coef(objeto_regsubsets, id)
  X_col = names(coefi)
  pred = X_mat[,X_col] %*% coefi
  return(pred)
}