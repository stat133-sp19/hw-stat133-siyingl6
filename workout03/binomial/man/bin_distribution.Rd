% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binomial.R
\name{bin_distribution}
\alias{bin_distribution}
\title{bin_distribution function}
\usage{
bin_distribution(trials, prob)
}
\arguments{
\item{trials}{number of trials}

\item{prob}{probability of success on each trial}
}
\value{
data frame of a binomial probability distribution
}
\description{
generate a binomial probability distribution
}
\examples{
bin_distribution(trials = 5, prob = 0.5)

}
