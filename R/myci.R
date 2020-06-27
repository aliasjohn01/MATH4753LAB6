#' myci() Function
#'
#' Takes the size of the sample and the percentage of probability as parameters
#' in order to produce a confidence interval as required.
#'
#' @param x
#' @param p
#'
#' @return a confidence interval with a lower and upper limit.
#' @export
#'
#' @examples
myci=function(x,p){
  mn = mean(x)
  sd = sd(x)
  n = length(x)
  error = qnorm(p)*(sd/(sqrt(n)))
  L = mn - error
  U = mn + error
  list(Lower=L, Upper=U)
}
