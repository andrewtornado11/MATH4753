#' @title mybin
#' @description function for binomial calculation
#'
#' @param y Number of trials
#' @param r Number of successes
#' @param p Probability
#'
#' @return Probability of < = y
#'
#' @examples mynbin(10,3,0.6)
mynbin=function(y,r,p){
  choose(y-1,r-1)*p^r*(1-p)^(y-r)
}
mynbin(10,3,0.4)


#' @title myncurve
#' @description function for normal curve graph and area calculation
#'
#' @param mu mean
#' @param sigma standard deviation
#' @param a maximum x-value where probability will be calculated
#'
#' @return graph, area shaded in red
#' @export
#'
#' @examples myncurve(4,3,9)
myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))

  xcurve = seq(mu-3*sigma,a,length=100)
  ycurve = dnorm(xcurve,mu,sigma)
  polygon(c(mu-3*sigma,xcurve,a),c(0,ycurve,0),col="Red")

  area = pnorm(a,mean=mu,sd=sigma)
  arear = round(area,4)

  list(arear)

}

myncurve(4,3,9)


#' This is data to be included in my package for Lab 7
#'
#' @name fire
#' @docType data
#' @author Mendenhall and Sincich: Statistics for Engineering and the Sciences, Sixth Edition
#' @keywords fire

fire = read.csv("/Users/amuehr/Desktop/Stats/Lab 7/FIREDAM.csv")

