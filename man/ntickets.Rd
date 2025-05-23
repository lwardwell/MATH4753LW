% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ntickets.R
\name{ntickets}
\alias{ntickets}
\title{Optimal Number of Tickets to Sell to Avoid an Oversold Plane}
\usage{
ntickets(N, gamma, p)
}
\arguments{
\item{N}{Integer, the number of seats available on the plane}

\item{gamma}{Numeric between 0 and 1, the acceptable probability that the plane will be truly oversold}

\item{p}{Numeric between 0 and 1, the probability that a ticket holder will show up}
}
\value{
A list containing five elements:
  \item{nd}{Number of tickets to sell based on discrete binomial distribution}
  \item{nc}{Number of tickets to sell based on normal approximation}
  \item{N}{The input number of seats}
  \item{p}{The input show-up probability}
  \item{gamma}{The input acceptable oversold probability}
}
\description{
Calculates the optimal number of tickets to sell for a flight with N seats,
given a desired probability of overbooking and passenger show-up rate.
Uses both discrete (binomial) and continuous (normal approximation) methods.
}
\examples{
# Calculate for a 100-seat plane (90\% show-up, 5\% oversold probability)
ntickets(N = 100, gamma = 0.05, p = 0.9)

# Compare results with different parameters
ntickets(N = 200, gamma = 0.01, p = 0.85)
}
