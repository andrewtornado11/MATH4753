#' @title myclt
#' @description Creates a histogram of the sums of the random samples of the uniform distribtion as defined by your input
#'
#' @param n Number of samples
#' @param iter Number of iterations within each sample
#' @param a Left bound of uniform distribution
#' @param b Right bound of uniform distribution
#'
#' @return Histogram of the sums of the random samples of the uniform function
#'@export
#'
myclt=function(n,iter,a=0,b=5){
  y=runif(n*iter,a,b)
  data=matrix(y,nr=n,nc=iter,byrow=TRUE)
  sm=apply(data,2,sum)
  h=hist(sm,plot=FALSE)
  hist(sm,col=rainbow(length(h$mids)),freq=FALSE,main="Distribution of the sum of uniforms")
  curve(dnorm(x,mean=n*(a+b)/2,sd=sqrt(n*(b-a)^2/12)),add=TRUE,lwd=2,col="Blue")
  sm
}

