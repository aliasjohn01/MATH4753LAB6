% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myci.R
\name{myci}
\alias{myci}
\title{myci() Function}
\usage{
myci(x, p)
}
\arguments{
\item{x}{a random sample}

\item{p}{percentage of probability}
}
\value{
a confidence interval with a lower and upper limit.
}
\description{
Takes the size of the sample and the percentage of probability as parameters
in order to produce a confidence interval as required.
}
\examples{
set.seed(23);x = rnorm(30,mean=10,sd=12)
myci(x,0.95)
}
