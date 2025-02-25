#' My Normal Curve Function
#'
#' Plots a normal probability density function with specified mean and standard deviation
#'
#' @param mu The mean of the normal distribution
#' @param sigma The standard deviation of the normal distribution
#'
#' @returns A list with the mean and standard deviation
#' @export
#'
#' @examples
#' # Plot a standard normal curve
#' myncurve(0, 1)
#'
#' # Plot a normal curve with mean 10 and standard deviation 2
#' myncurve(10, 2)
#'
myncurve = function(mu, sigma){
  # Validate that sigma is positive
  if(sigma <= 0) {
    stop("sigma must be positive")
  }
  curve(dnorm(x, mean=mu, sd=sigma), xlim = c(mu - 3*sigma, mu + 3*sigma))
  list(mu = mu, sigma = sigma)
}
