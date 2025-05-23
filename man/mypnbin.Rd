% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mypnbin.R
\name{mypnbin}
\alias{mypnbin}
\title{Cumulative Probability for Negative Binomial Distribution}
\usage{
mypnbin(y, r, p)
}
\arguments{
\item{y}{Integer. The upper bound of the cumulative probability calculation.}

\item{r}{Integer. The number of required successes in the negative binomial distribution.}

\item{p}{Numeric (0 < p < 1). The probability of success in each trial.}
}
\value{
A numeric probability between 0 and 1, representing  \eqn{P(Y \geq r)}.
}
\description{
This function calculates the cumulative probability
\eqn{P(Y \geq r)} for a **negative binomial distribution**
with parameters `r` (number of successes) and `p` (success probability).
}
\examples{
# Compute P(Y ≥ 3) for a Negative Binomial with p = 0.4
mypnbin(y = 5, r = 3, p = 0.4)

# Compute P(Y ≥ 2) for a Negative Binomial with p = 0.6
mypnbin(y = 6, r = 2, p = 0.6)
}
