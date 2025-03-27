#' My Bayes Function
#'
#' Calculates the posterior probability using Bayes' theorem for a specific scenario,
#' typically used in diagnostic or classification contexts.
#'
#' @param x Numeric. The probability of a true positive, P(+|U), where + indicates a positive test result
#'        and U indicates the condition being present.
#' @param y Numeric. The prior probability, P(U), representing the probability of the condition being present.
#' @param z Numeric. The probability of a false positive, P(+|NU), where + indicates a positive test result
#'        and NU indicates the condition being absent.
#'
#' @return Numeric. The posterior probability P(U|+), which is the probability that the condition is absent
#'         given a positive test result.
#' @export
#'
#' @examples
#' # Calculate posterior probability with 90% true positive rate,
#' # 5% prevalence, and 10% false positive rate
#' mybayes(0.9, 0.05, 0.1)
#'
#' # For a medical test with 95% sensitivity, 1% disease prevalence,
#' # and 8% false positive rate
#' mybayes(0.95, 0.01, 0.08)
mybayes <- function(x, y, z){
  # x is the probability of (+|U) (True Positive Rate/Sensitivity)
  # y is the probability of (U) (Prior Probability/Prevalence)
  # z is the probability of (+|NU) (False Positive Rate/1-Specificity)
  x <- as.numeric(x)
  y <- as.numeric(y)
  z <- as.numeric(z)

  # Calculate the posterior P(U|+) using Bayes' theorem
  # P(U|+) = [P(+|U) × P(U)] / [P(+|U) × P(U) + P(+|NU) × P(NU)]
  bayes <- (x*y)/((x*y)+(z*(1-y)))
  return(bayes)
}
