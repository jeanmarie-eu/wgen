#' entropy.Pij
#'
#' Probability P(i,j)
#' @param X
#' @param Y
#' @keywords entropy
#' @export
#' @examples
#' entropy.Pij()
entropy.Pij<-function(X,Y) {
   tmp<-entropy.counting(X,Y)
   Pij<-tmp/sum(tmp,na.rm=TRUE)
   return(Pij)
}