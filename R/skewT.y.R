#' skewT.y
#'
#' plot the y-axis of an empty skewT graph 
#' @param a
#' @param b
#' @param p
#' @keywords skewT
#' @export
#' @examples
#' skewT.y()
skewT.y<-function(a,b,p){
   results<-(log10(p)-b)/a
   return(results)
}