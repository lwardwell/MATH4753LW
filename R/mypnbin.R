#' Cumulative Probability for Negative Binomial Distribution
#'
#' This function calculates the cumulative probability
#' \eqn{P(Y \geq r)} for a **negative binomial distribution**
#' with parameters `r` (number of successes) and `p` (success probability).
#'
#' @param y Integer. The upper bound of the cumulative probability calculation.
#' @param r Integer. The number of required successes in the negative binomial distribution.
#' @param p Numeric (0 < p < 1). The probability of success in each trial.
#'
#' @returns A numeric probability between 0 and 1, representing  \eqn{P(Y \geq r)}.
#'
#' @export
#'
#' @examples
#' # Compute P(Y ≥ 3) for a Negative Binomial with p = 0.4
#' mypnbin(y = 5, r = 3, p = 0.4)
#'
#' # Compute P(Y ≥ 2) for a Negative Binomial with p = 0.6
#' mypnbin(y = 6, r = 2, p = 0.6)
mypnbin = function(y, r, p){
  y <- r:y  # Create a sequence from r to y
  obj = choose(y - 1, r - 1) * p^r * (1 - p)^(y - r)  # Compute probability mass function values
  sum(obj)  # Sum probabilities to get cumulative probability
}
