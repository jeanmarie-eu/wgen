#' skewT.ab
#'
#' a,b slope and coef
#' @param x1
#' @param x2
#' @param y1
#' @param y2
#' @keywords skewT
#' @export
#' @examples
#' skewT.ab()
skewT.ab<-function(x1,x2,y1,y2){
   a<-(log10(y2)-log10(y1))/(x2-x1)
   b<-log10(y1)-a*x1   
   origin<-x1
   results<-list(a=a,b=b,origin=origin)
   return(results)

}