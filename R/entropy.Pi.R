#' entropy.Pi
#'
#' Probability P(i)
#' @param X
#' @keywords entropy
#' @export
#' @examples
#' entropy.Pi()
entropy.Pi<-function(X) {

   tmp<-entropy.Xsumup(X)
   nb_classX<-tmp$nb_classX
   val_classX<-tmp$val_classX

   Pi<-entropy.Ni(X)/length(X)
   return(Pi)
}	  
