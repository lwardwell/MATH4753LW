#' My Normal Curve Function
#'
#' Plots a normal probability density function with specified mean and standard deviation
#'
#' @param mu The mean of the normal distribution
#' @param sigma The standard deviation of the normal distribution
#' @param a The value to shade the area under the curve to the left of
#'
#' @importFrom stats dnorm pnorm
#' @importFrom graphics plot polygon
#' @returns A list with the mean and standard deviation
#' @export
#'
#' @examples
#' # Plot a standard normal curve
#' myncurve(0, 1, 2)
#'
#' # Plot a normal curve with mean 10 and standard deviation 2
#' myncurve(10, 2, 12)
#'
myncurve = function(mu, sigma, a){
  # Validate that sigma is positive
  if(sigma <= 0) {
    stop("sigma must be positive")
  }
  # Define the x values for the normal curve
        xcurve=seq(mu - 3*sigma, mu + 3*sigma, length=1000)
        ycurve=dnorm(xcurve, mean=mu, sd=sigma)
  # Plot the normal curve
        plot(xcurve, ycurve, type="l", lwd=2,
        xlab="x", ylab="Density",
        main=paste("Normal Curve: mu=", mu, ", sigma=", sigma))
  #Shade the area under the curve to the left of a
        xshade = seq(mu - 3*sigma, a, length=1000)
        yshade = dnorm(xshade, mean=mu, sd=sigma)
                polygon(c(xshade,a),
                c(yshade, 0), col="pink")
        prob = pnorm(a, mean=mu, sd=sigma)
  list(mu = mu, sigma = sigma, a = a, prob = prob)
}
