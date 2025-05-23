% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mychebyshev.R
\name{mychebyshev}
\alias{mychebyshev}
\title{My Chebyshev's Theorem Check Function}
\usage{
mychebyshev(data, k_values = c(1, 2, 3, 4, 5))
}
\arguments{
\item{data}{A numeric vector containing the data to analyze}

\item{k_values}{A numeric vector specifying the number of standard deviations to check (default: c(1, 2, 3, 4, 5))}
}
\value{
A data frame with three columns:
  \item{k}{The number of standard deviations}
  \item{theoretical_bound}{The minimum proportion according to Chebyshev's theorem (1 - 1/k²)}
  \item{actual_proportion}{The actual proportion of data within k standard deviations}
}
\description{
Calculates the proportion of data points that fall within k standard deviations
of the mean and compares it with the theoretical bounds from Chebyshev's theorem.
}
\examples{
# Generate sample data
set.seed(123)
sample_data <- rnorm(1000, mean = 50, sd = 10)

# Check Chebyshev's theorem
results <- mychebyshev(sample_data)
print(results)

# Use custom k values
custom_results <- mychebyshev(sample_data, k_values = c(1.5, 2.5, 3.5))
}
