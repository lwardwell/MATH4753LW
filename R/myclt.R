utils::globalVariables("x")

#' My Central Limit Theorem Function
#'
#' Demonstrates the Central Limit Theorem by generating the distribution of sums of uniform random variables and
#' comparing it to the normal distribution.
#'
#' @param n Number of uniform random variables to sum in each iteration
#' @param iter Number of iterations (sample size for the distribution of the sum of uniform random variables)
#' @param a Lower bound of the uniform distribution
#' @param b Upper bound of the uniform distribution
#'
#' @importFrom stats runif dnorm
#' @importFrom graphics hist curve
#'
#' @return A histogram of the distribution of the sum of uniform random variables and the normal distribution
#' @export
#'
#' @examples
#' # Basic usage with default parameters
#' myclt(10,1000)
#'
#' # Custom usage with distribution from -1 to 1
#' myclt(10,1000,-1,1)
#'
myclt=function(n,iter,a=0,b=5){
  y=runif(n*iter,a,b) # Generate random variables
  data=matrix(y,nrow=n,ncol=iter,byrow=TRUE) # Reshape the data into a matrix
  sm=apply(data,2,sum) # Calculate the sum of each column
  h=hist(sm,plot=FALSE) # Creates a histogram of the sums of the random variables, but does not plot it
  hist(sm,col=rainbow(length(h$mids)),freq=FALSE,main="Distribution of the sum of uniforms") # Creates a histogram of the sums of the random variables
  curve(dnorm(x,mean=n*(a+b)/2,sd=sqrt(n*(b-a)^2/12)),add=TRUE,lwd=2,col="Blue") # Adds the normal distribution curve to the histogram
  sm
}
