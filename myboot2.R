#' @title myboot2
#'
#' @param iter the number of bootstrap resamples to generate
#' @param x the original sample data from which the bootstrap resamples are drawn
#' @param fun represents the statistic of interest to be computed on each bootstrap sample.
#' @param alpha alpha sets the significance level for the confidence interval.
#' @param ... allows optional additional arguments to be passed to the function
#'
#' @returns returns a list of the confidence interval, the statistic function used, and the original sample
#' @export
#'
#' @examples \dontrun{set.seed(68)
#' sam <- rnorm(20, mean=10, sd=4)
#' myboot(iter=10000, x=sam, fun="mean", alpha=0.05)}
myboot2<-function(iter=10000,x,fun="mean",alpha=0.05,...){

  #Notice where the ... is repeated in the code
  n=length(x)   #sample size

  #Now sample with replacement
  y=sample(x,n*iter,replace=TRUE) #A

  # Make a matrix with all the resampled values
  rs.mat=matrix(y,nr=n,nc=iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun)
  # xstat is a vector and will have iter values in it
  ci=quantile(xstat,c(alpha/2,1-alpha/2)) #B
  # Nice way to form a confidence interval
  # A histogram follows
  # The object para will contain the parameters used to make the histogram
  para=hist(xstat,freq=FALSE,las=1,main="Histogram of Bootstrap sample statistics",...)

  #mat will be a matrix that contains the data, this is done so that I can use apply()
  mat=matrix(x,nr=length(x),nc=1,byrow=TRUE)

  #pte is the point estimate
  #This uses whatever fun is
  pte=apply(mat,2,fun)
  abline(v=pte,lwd=3,col="Black")# Vertical line
  segments(ci[1],0,ci[2],0,lwd=4)      #Make the segment for the ci
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=3)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=3)

  # plot the point estimate 1/2 way up the density
  text(pte,max(para$density)/2,round(pte,2),cex=3)

  return(list(ci=ci,fun=fun,x=x))# Some output to use if necessary
}
