#' mybin1
#'
#' @param y
#' @param r
#' @param p
#'
#' @return Probability of < = y
#' @export
#'
#' @examples
mynbin=function(y,r,p){
  choose(y-1,r-1)*p^r*(1-p)^(y-r)
}
mynbin(10,3,0.4)

