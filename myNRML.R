#' @title myNRML
#'
#' @param x0 Initial guess for the parameter value from which the Newton-Raphson iteration starts.
#' @param delta A small increment used for numerical differentiation.
#' @param llik You must define this function separately and pass it in. It should take a single parameter and return the log-likelihood value for that parameter.
#' @param xrange A range of x-values or parameter values over which to plot the log-likelihood and its derivative.
#' @param parameter A label for the x-axis in the plots, representing the name of the parameter being estimated.
#'
#' @returns Finds the MLE numerically using Newton-Raphson and returns the whole path of guesses x and slopes y.
#' @export
#'
#' @examples \dontrun{llik <- Vectorize(function(lambda) {sum(dpois(y, lambda, log = TRUE))})
#' myNRML(x0 = 2, delta = 0.001, llik = llik, xrange = c(0, 10), parameter = "lambda")}
myNRML=function(x0,delta=0.001,llik,xrange,parameter="param"){
  f=function(x) (llik(x+delta)-llik(x))/delta
  fdash=function(x) (f(x+delta)-f(x))/delta
  d=1000
  i=0
  x=c()
  y=c()
  x[1]=x0
  y[1]=f(x[1])
  while(d > delta & i<100){
    i=i+1
    x[i+1]=x[i]-f(x[i])/fdash(x[i])
    y[i+1]=f(x[i+1])
    d=abs(y[i+1])
  }
  layout(matrix(1:2,nr=1,nc=2,byrow=TRUE),width=c(1,2))
  curve(llik(x), xlim=xrange,xlab=parameter,ylab="log Lik",main="Log Lik")
  curve(f(x),xlim=xrange,xaxt="n", xlab=parameter,ylab="derivative",main=  "Newton-Raphson Algorithm \n on the derivative")
  points(x,y,col="Red",pch=19,cex=1.5)
  axis(1,x,round(x,2),las=2)
  abline(h=0,col="Red")

  segments(x[1:(i-1)],y[1:(i-1)],x[2:i],rep(0,i-1),col="Blue",lwd=2)
  segments(x[2:i],rep(0,i-1),x[2:i],y[2:i],lwd=0.5,col="Green")

  list(x=x,y=y)
}
