% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myboot2.R
\name{myboot2}
\alias{myboot2}
\title{myboot2}
\usage{
myboot2(iter = 10000, x, fun = "mean", alpha, cx = 1.5, ...)
}
\arguments{
\item{iter}{number of iterations}

\item{x}{a random sample}

\item{fun}{the type of function}

\item{alpha}{the value for alpha}

\item{cx}{a default value of 1.5 has been input for this function}

\item{...}{other parameters can also be applied}
}
\value{
a histogram with the confidence interval
}
\description{
Uses the bootstrap simulation technique to find the confidence interval of
the mean statistic for a given sample.
}
\examples{
 myboot2(x=c(5,6,3,4,5,68,9,77,66,46,7,88,9), alpha=0.05, col=rainbow(11))

}
