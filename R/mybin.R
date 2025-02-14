#' Binomial Simulation
#'
#' @description Simulates a binomial distribution by performing repeated Bernoulli trials.
#'
#' @param iter Number of iterations (default: 100)
#' @param n Number of trials per iteration (default: 10)
#' @param p Probability of success in each trial (default: 0.5)
#'
#' @returns A table of simulated binomial proportions and a barplot.
#' @export
#'
#' @examples
#' # Example 1: Run simulation with default parameters
#' mybin()
#'
#' # Example 2: Increase the number of iterations
#' mybin(iter = 500)
#'
#' # Example 3: Change number of trials per iteration
#' mybin(n = 20)
#'
#' # Example 4: Adjust probability of success
#' mybin(n = 15, p = 0.7)
#'
mybin=function(iter=100, n=10, p=0.5){
  # Make a matrix to hold the samples (initially filled with NA's)
  sam.mat = matrix(NA, nrow=n, ncol=iter, byrow=TRUE)

  # Vector to hold the number of successes in each trial
  succ = numeric(iter)

  for(i in 1:iter){
    # Fill each column with a new sample
    sam.mat[,i] = sample(c(1,0), n, replace=TRUE, prob=c(p,1-p))
    # Sum successes in the sample
    succ[i] = sum(sam.mat[,i])
  }

  # Make a table of successes
  succ.tab = table(factor(succ, levels=0:n))

  # Generate barplot of proportions
  barplot(succ.tab / iter, col=rainbow(n+1),
          main="Binomial simulation", xlab="Number of successes")

  # Return proportion table
  succ.tab / iter
}
