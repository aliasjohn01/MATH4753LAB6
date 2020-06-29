#' myci() Function
#'
#' Takes the size of the sample and the percentage of probability as parameters
#' in order to produce a confidence interval as required.
#'
#' @param x a random sample
#' @param p  percentage of probability
#'
#' @return a confidence interval with a lower and upper limit.
#' @export
#'
#' @examples
#' set.seed(23);x = rnorm(30,mean=10,sd=12)
#' myci(x,0.95)
myci=function(x,p){
  mn = mean(x)
  sd = sd(x)
  n = length(x)
  error = qnorm(p)*(sd/(sqrt(n)))
  L = mn - error
  U = mn + error
  list(Lower=L, Upper=U)
}
