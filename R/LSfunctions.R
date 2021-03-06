#' Least Squares Regression
#'
#' @param formula an object of class "formula"
#' @param data data frame to which the formula relates
#'
#' @return list containing three elements: Parameters, df, y
#' @export
#' @import stats
#' @examples
#' df = data.frame(y = c(1,2,3,4), x = c(2,5,3,1))
#' LS.model(y~x, data=df)
LS.model = function(formula, data = NULL){

  ys = all.vars(formula)[1]
  y = data[,ys]
  X = model.matrix(formula, data)

  wLS =  solve(t(X) %*% X) %*% t(X) %*% y

  return(list(Parameters = wLS, df = X, y = y))
}
#' Least Squares Prediction
#'
#' @param model output from \code{LS.r} function
#' @param newdata input data to predict new output
#'
#' @return In-sample prediction vector if \code{newdata} not supplied. Otherwise out-of-sample prediction given by input \code{newdata}
#' @export
#'
#' @examples
#' df = data.frame(y = c(1,2,3,4), x = c(2,5,3,1))
#' m = LS.model(y~x, data=df)
#' LS.predict(m)
LS.predict = function(model, newdata=NULL){

  if(is.null(newdata)) return(model$df %*% model$Parameters)
  if(!is.null(newdata)) {
    if(dim(as.matrix(newdata))[2]!=dim(as.matrix(newdata))[2]) {
      stop("Dimension of newdata different to dimension of training data")
    }
    nd = cbind(1,newdata)
    return((nd) %*% model$Parameters)
    }
}

#' Plot least squares regression
#'
#' @param model output from LS.model
#' @param var string value of which input variable to plot
#' @param ... additional arguments to be passed to \code{plot}
#'
#' @return a plot of the model output against an input variable with predicted line from model
#' @export
#'
#' @import graphics
#' @examples
#' df = data.frame(y = c(1,2,3,4), x = c(2,5,3,1))
#' m = LS.model(y~x, data=df)
#' LS.plot(m,var="x")
LS.plot = function(model, var = NULL, ...){
  X = model$df
  y = model$y
  d = dim(X)

  if(is.null(var)){
    print("var not specified, taking first input value")
    names = colnames(X)[colnames(X)!="(Intercept)"]
    var = names[1]
  }

  preds = LS.predict(model)
  o = order(preds)
  plot(X[,var], y, xlab = var, ...)
  lines(X[o,var], preds[o], col="red",lwd=2)

}
