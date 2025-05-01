#' @title myclt
#'
#' @param n sample size
#' @param iter the number of iterations or number of samples
#' @param a lower bound of the uniform distribution
#' @param b upper bound of the uniform distribution
#'
#' @returns returns the vector sm, which contains the sums of each sample.
#' @export
#'
#' @examples \dontrun{myclt(n=10,iter=10000)}
myclt=function(n,iter,a=0,b=5){
  y=runif(n*iter,a,b)
  data=matrix(y,nr=n,nc=iter,byrow=TRUE)
  sm=apply(data,2,sum)
  h=hist(sm,plot=FALSE)
  hist(sm,col=rainbow(length(h$mids)),freq=FALSE,main="Distribution of the sum of uniforms")
  curve(dnorm(x,mean=n*(a+b)/2,sd=sqrt(n*(b-a)^2/12)),add=TRUE,lwd=2,col="Blue")
  sm
}
