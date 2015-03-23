
#' entropy.h
#'
#' H(X)
#' @param X
#' @keywords entropy
#' @export
#' @examples
#' entropy.h()
entropy.h<-function(X) {   
   Pi<-entropy.Pi(X)
   h_tmp<--Pi*log(Pi)/log(2)
   h<-sum(h_tmp)
   return(h)
}