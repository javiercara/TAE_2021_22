cross_val_pos = function(num_datos,num_folds){
  # -------------------------------------------------------------------
  # esta funcion calcula las posiciones que corresponden al train
  # y las posiciones que corresponden al test en cross validation
  #
  # num_datos: numero de datos
  # num_folds: numero de folds
  # 
  # javier.cara@upm.es, version 2019.10
  # ------------------------------------------------------------------
  
  # numero de datos de cada fold
  n1 = trunc(num_datos/num_folds)
  
  v = sample(1:num_datos,num_datos,replace = F)
  train = list()
  test = list()
  for (k in 1:(num_folds-1)){
    pos_test = ((k-1)*n1+1):(k*n1)
    test[[k]] = v[pos_test]
    train[[k]] = v[-pos_test]
  }
  # el ultimo puede tener un numero diferente de datos (por trunc)
  pos_test = ((num_folds-1)*n1+1):num_datos
  test[[num_folds]] = v[pos_test]
  train[[num_folds]] = v[-pos_test]
  
  return(list(train = train, test = test))
}

