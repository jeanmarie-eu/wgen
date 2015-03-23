#' entropy.d
#'
#' d(X,Y): Variation of information, d(X,Y)=H(X,Y)-I(X,Y)=H(X)+H(Y)-2I(X,Y)
#' @param X
#' @param Y
#' @keywords entropy
#' @export
#' @examples
#' entropy.d()
entropy.d<-function(X,Y) {

   tmp<-entropy.Xsumup(X)
   nb_classX<-tmp$nb_classX
   val_classX<-tmp$val_classX
   
   tmp<-entropy.Xsumup(Y)
   nb_classY<-tmp$nb_classX
   val_classY<-tmp$val_classX
   
   tmp1<-entropy.hand(X,Y)
   tmp2<-entropy.mi(X,Y) 
   d<-tmp1-tmp2
   return(d)
}