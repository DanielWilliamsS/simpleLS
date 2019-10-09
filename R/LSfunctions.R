
#' Least Squares Regression
#'
#' @param formula an object of class "formula"
#' @param data data frame to which the formula relates
#'
#' @return list containing two elements: Parameters, df
#' @export
#'
#' @examples
#' library(lasso2)
#' data(Prostate)
#' LS.r(lpsa ~ lcavol, data = Prostate)
LS.r = function(formula, data = NULL){

  ys = all.vars(formula)[1]
  y = data[,ys]
  X = model.matrix(formula, data)

  wLS =  solve(t(X) %*% X) %*% t(X) %*% y

  return(list(Parameters = wLS, df = X))
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
#' library(lasso2)
#' data(Prostate)
#' m = LS.r(lpsa ~ lcavol, data = Prostate)
#' plot(Prostate[,c("lcavol","lpsa")])
#' plot(Prostate$lcavol,LS.p(m))
LS.p = function(model, newdata=NULL){

  if(is.null(newdata))return(model$df %*% model$Parameters)
  if(!is.null(newdata)) {
    if(dim(as.matrix(newdata))[2]!=dim(as.matrix(newdata))[2]) {
      stop("Dimension of newdata different to dimension of training data")
    }
    nd = cbind(1,newdata)
    return((nd) %*% model$Parameters)
    }
}

