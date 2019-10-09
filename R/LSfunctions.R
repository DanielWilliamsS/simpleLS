
LS.r = function(formula, data){

  ys = all.vars(formula)[1]
  y=Prostate[,ys]
  X = model.matrix(formula, data)

  wLS =  solve(t(X) %*% X) %*% t(X) %*% y

  return(list(Parameters = wLS))
}

LS.p = function(model){
  model$df %*% model$Parameters
}
