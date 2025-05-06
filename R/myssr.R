#' Calculate Sum of Squares Residuals and Standard Error
#'
#' This function performs simple linear regression calculations and returns
#' the sum of squares residuals (SSR) and standard error of the estimate.
#'
#' @param x Numeric vector containing predictor variable values
#' @param y Numeric vector containing response variable values
#'
#' @return A list with two components:
#'   \item{ssr}{Sum of squares residuals}
#'   \item{sq}{Standard error of the estimate}
#' @export
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' y <- c(2, 3, 5, 4, 6)
#' myssr(x, y)
myssr=function(x,y){
  n=length(x) # or y
  ssxx=sum((x-mean(x))^2) ## Calculate the Sum of Squares xx
  ssxy=sum((x-mean(x))*y) ## Calculate the Sum of Squares xy
  b1hat=ssxy/ssxx ## Beta1hat = ssxy/ssxx
  b0hat=mean(y) - b1hat*mean(x)   ## Beta0hat = mean(y) - b1hat*mean(x)
  yhat=b0hat+(b1hat*x)  ## yhat = b0hat + b1hat*x
  ssr=sum((y-yhat)^2) ## Calculate the Sum of Squares of the Residuals
  sq=sqrt(ssr/(n-2)) ## Calculate the Standard Error of the Estimate
  return(list(ssr=ssr,sq=sq))
}
