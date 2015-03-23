#' skewT.x
#'
#' plot the x-axis of an empty skewT graph 
#' @param a
#' @param b
#' @param T_min
#' @param T
#' @param p
#' @keywords skewT
#' @export
#' @examples
#' skewT.x()
skewT.x<-function(a,b,T_min,T,p){
   results<-(T-T_min)+skewT.y(a,b,p)
   return(results)
}