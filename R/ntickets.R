#' Optimal Number of Tickets to Sell to Avoid an Oversold Plane
#'
#' Calculates the optimal number of tickets to sell for a flight with N seats,
#' given a desired probability of overbooking and passenger show-up rate.
#' Uses both discrete (binomial) and continuous (normal approximation) methods.
#'
#' @param N Integer, the number of seats available on the plane
#' @param gamma Numeric between 0 and 1, the acceptable probability that the plane will be truly oversold
#' @param p Numeric between 0 and 1, the probability that a ticket holder will show up
#'
#' @return A list containing five elements:
#'   \item{nd}{Number of tickets to sell based on discrete binomial distribution}
#'   \item{nc}{Number of tickets to sell based on normal approximation}
#'   \item{N}{The input number of seats}
#'   \item{p}{The input show-up probability}
#'   \item{gamma}{The input acceptable oversold probability}
#' @export
#'
#' @importFrom graphics abline layout plot
#' @importFrom stats pbinom pnorm uniroot
#'
#' @examples
#' # Calculate for a 100-seat plane (90% show-up, 5% oversold probability)
#' ntickets(N = 100, gamma = 0.05, p = 0.9)
#'
#' # Compare results with different parameters
#' ntickets(N = 200, gamma = 0.01, p = 0.85)
ntickets<-function(N, gamma, p){
  # N = number of seats on the plane
  # gamma = probability the plane will be truly oversold
  # p = probability that a ticket holder will show up

  # Use the appropriate discrete function
  # Discrete case: Find nd using which.min()
  nposs <- seq(N, 1.1*N, by = 1)  # Range of ticket sales to search

  fnposs <- 1 - pbinom(N, nposs, p) - gamma # Create a function for the upper tail probability that the number of passengers exceeds N and subtract gamma

  nd <- nposs[which.min(abs(fnposs))]  # Optimize the function and find closest to zero
  nd

  # Use the normal approximation
  # Continuous case: Find nc using uniroot()
  ncontinuous <- function(n) {
    mu <- n * p
    sigma <- sqrt(n * p * (1 - p))
    return((1 - pnorm(N, mean = mu, sd = sigma)) - gamma) # Create a function for the normal approximation
  }

   # Check if solution exists within the range
  lower_val <- ncontinuous(N)
  upper_val <- ncontinuous(1.1*N)

  # If values have opposite signs, uniroot will work
  if (sign(lower_val) != sign(upper_val)) {
    nc <- uniroot(ncontinuous, lower = N, upper = (1.1*N))$root
    nc <- ceiling(nc) # Round up to the nearest integer
  } else {
    # If no solution in range, use the nd value as a fallback
    nc <- nd
    warning("No root found in the specified range. Using discrete approximation result.")
  }

  # Create a plot of the Objective Function vs. n, make one plot for the continuous case and one for the discrete case
  # Generate Plots
  layout(matrix(c(1, 2), nrow = 2, byrow = TRUE))

  # Calculate continuous values BEFORE plotting
  n_cont <- seq(N, 1.1*N, by = 0.1)
  obj_continuous <- sapply(n_cont, ncontinuous)

  # Discrete Plot
  plot(nposs, fnposs, type = "b", col = "blue",
       main = paste("Objective Vs n to find optimal tickets sold\n(", nd, ") gamma=", gamma, "N=", N, "discrete"),
       xlab = "n", ylab = "Objective",
       ylim = c(min(fnposs, obj_continuous, na.rm = TRUE), 1))  # Set y-axis maximum to 1
  abline(h = 0, col = "red")
  abline(v = nd, col = "red")

# Continuous Plot

  plot(n_cont, obj_continuous, type = "l", col = "black",
       main = paste("Objective Vs n to find optimal tickets sold\n(", nc, ") gamma=", gamma, "N=", N, "continuous"),
       xlab = "n", ylab = "Objective",
       ylim = c(min(obj_continuous, fnposs, na.rm = TRUE), 1))  # Set y-axis maximum to 1
  abline(h = 0, col = "blue")
  abline(v = nc, col = "blue")

  # Print a named list containing nd, nc, N, p, and gamma
  # nd = calculated using the discrete distribution
  # nc = calculated using the normal approximation

  return(list(nd = nd, nc = nc, N = N, p = p, gamma = gamma))
}
