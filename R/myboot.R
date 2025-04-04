#' Bootstrap Confidence Interval and Plot
#'
#' Creates bootstrap samples from input data, calculates a specified statistic,
#' and visualizes the bootstrap distribution with confidence intervals.
#'
#' @param iter Number of bootstrap iterations to perform (default: 10000)
#' @param x Numeric vector of data to bootstrap
#' @param fun Function to apply to bootstrap samples (default: "mean")
#' @param alpha Significance level for confidence interval calculation (default: 0.05)
#' @param cx Character expansion factor for text in plot (default: 1.5)
#' @param ... Additional graphical parameters passed to hist()
#'
#' @returns Invisibly returns a list with components:
#'  \item{ci}{A vector containing the lower and upper confidence interval bounds}
#'  \item{fun}{The function used for calculating statistics}
#'  \item{x}{The original data vector}
#'
#' @importFrom stats quantile
#' @export
#'
#' @examples
#' # Bootstrap mean with default 95% confidence interval
#' set.seed(123)
#' sample_data <- rnorm(30, mean=10, sd=2)
#' myboot(x=sample_data)
#'
#' # Bootstrap variance with 90% confidence interval
#' myboot(x=sample_data, fun="var", alpha=0.1)
#'
#' # Using additional histogram parameters
#' myboot(x=sample_data, breaks=20, col="lightblue")
#'
myboot<-function(iter=10000,x,fun="mean",alpha=0.05,cx=1.5,...){  #Notice where the ... is repeated in the code
  n=length(x)   #sample size

  y=sample(x,n*iter,replace=TRUE)
  rs.mat=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun) # xstat is a vector of means and will have iter values in it
  ci=quantile(xstat,c(alpha/2,1-alpha/2))# Nice way to form a confidence interval
  # A histogram follows
  # The object para will contain the parameters used to make the histogram
  para=hist(xstat,freq=FALSE,las=1,
            main=paste("Histogram of Bootstrap sample ",fun, " statistics","\n","alpha=",alpha," iter=",iter,sep=""),
            ...)

  #mat will be a matrix that contains the data, this is done so that I can use apply()
  mat=matrix(x,nrow=length(x),ncol=1,byrow=TRUE)

  #pte is the point estimate
  #This uses whatever fun is
  pte=apply(mat,2,fun)
  abline(v=pte,lwd=3,col="Black")# Vertical line
  segments(ci[1],0,ci[2],0,lwd=4)      #Make the segment for the ci
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=cx)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=cx)

  # plot the point estimate 1/2 way up the density
  text(pte,max(para$density)/2, paste(fun, "=",round(pte,2)),cex=cx)

  invisible(list(ci=ci,fun=fun,x=x))# Some output to use if necessary
}
