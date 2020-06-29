#' Normal distribution function
#'
#' Takes parameters mu, sigma and a to give a normal distribution curve with
#' area from - infinity to (a).
#'
#' @param mu
#' @param sigma
#' @param a
#'
#' @return a normal distribution curve and probability from negative infinity
#' to (a).
#' @export
#'
#' @examples myncurve(10, 5, 6)
#'
myncurve=function(mu, sigma, a){
  curve(dnorm(x,mean=mu, sd=sigma), xlim=c(mu-3*sigma, mu + 3*sigma))
  xcurve=seq(0.0,a,length=10)
  ycurve=dnorm(xcurve, mean=mu, sd=sigma)
  plot(x=xcurve, y=ycurve)
  polygon(c(0.0,xcurve,a),c(0,ycurve,0),col="Red")
  p16=pnorm(a, mean=mu, sd=sigma)
  p16=round(p16,4)
  text(x=xcurve, y=ycurve, paste("Area = ", p16, sep=""), adj=0)
}
