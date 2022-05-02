#' @title mynbin
#' @description function for binomial calculation
#'
#' @param y Number of trials
#' @param r Number of successes
#' @param p Probability
#'
#' @return Probability of < = y
#' @export
#'
mynbin=function(y,r,p){
  choose(y-1,r-1)*p^r*(1-p)^(y-r)
}
