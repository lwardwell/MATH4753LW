#' My Chebyshev's Theorem Check Function
#'
#' Calculates the proportion of data points that fall within k standard deviations
#' of the mean and compares it with the theoretical bounds from Chebyshev's theorem.
#' @param data A numeric vector containing the data to analyze
#' @param k_values A numeric vector specifying the number of standard deviations to check (default: c(1, 2, 3, 4, 5))
#'
#' @importFrom stats sd
#' @returns A data frame with three columns:
#'   \item{k}{The number of standard deviations}
#'   \item{theoretical_bound}{The minimum proportion according to Chebyshev's theorem (1 - 1/k²)}
#'   \item{actual_proportion}{The actual proportion of data within k standard deviations}
#' @export
#'
#' @examples# Generate sample data
#' set.seed(123)
#' sample_data <- rnorm(1000, mean = 50, sd = 10)
#'
#' # Check Chebyshev's theorem
#' results <- mychebyshev(sample_data)
#' print(results)
#'
#' # Use custom k values
#' custom_results <- mychebyshev(sample_data, k_values = c(1.5, 2.5, 3.5))
mychebyshev <- function(data, k_values = c(1, 2, 3, 4, 5)) {
  # Calculate mean and standard deviation
  data_mean <- mean(data, na.rm = TRUE)
  data_sd <- sd(data, na.rm = TRUE)

  # Create a data frame to store results
  results <- data.frame(
    k = k_values,
    theoretical_bound = 1 - 1/(k_values^2),
    actual_proportion = NA
  )

  # Calculate actual proportions for each k
  for (i in seq_along(k_values)) {
    k <- k_values[i]
    lower_bound <- data_mean - k * data_sd
    upper_bound <- data_mean + k * data_sd

    # Count data points within bounds
    points_within <- sum(data >= lower_bound & data <= upper_bound, na.rm = TRUE)
    total_points <- sum(!is.na(data))

    # Calculate proportion
    results$actual_proportion[i] <- points_within / total_points
  }

  return(results)
}
