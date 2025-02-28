#' Hypergeometric Distribution Simulation
#'
#' This function simulates random samples from a **hypergeometric distribution**
#' and creates a bar plot of the resulting proportions.
#'
#' @param iter Integer. The number of iterations (samples) to simulate. Default is `100`.
#' @param N Integer. The total population size. Default is `20`.
#' @param r Integer. The number of "successes" in the population. Default is `12`.
#' @param n Integer. The sample size for each iteration. Default is `5`.
#'
#' @returns A table of proportions for each possible number of successes.
#' Additionally, a bar plot of the proportions is displayed.
#'
#' @export
#'
#' @examples
#' # Simulate 100 samples with default parameters
#' myhyper()
#'
#' # Simulate 500 samples with a larger population
#' myhyper(iter = 500, N = 50, r = 30, n = 10)
#'
#' # Simulate 200 samples with different success probability
#' myhyper(iter = 200, N = 25, r = 5, n = 7)
myhyper=function(iter=100,N=20,r=12,n=5){
  # make a matrix to hold the samples
  # initially filled with NA's
  sam.mat=matrix(NA,nrow=n,ncol=iter, byrow=TRUE)
  #Make a vector to hold the number of successes over the trials
  succ=c()
  for( i in 1:iter){
    #Fill each column with a new sample
    sam.mat[,i]=sample(rep(c(1,0),c(r,N-r)),n,replace=FALSE)
    #Calculate a statistic from the sample (this case it is the sum)
    succ[i]=sum(sam.mat[,i])
  }
  #Make a table of successes
  succ.tab=table(factor(succ,levels=0:n))
  #Make a barplot of the proportions
  barplot(succ.tab/(iter), col=rainbow(n+1), main="HYPERGEOMETRIC simulation", xlab="Number of successes")
  succ.tab/iter
}
