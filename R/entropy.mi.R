#' entropy.mi
#'
#' I(X,Y): Mutual information, I/X,Y)=H(X,Y)-H(X/Y)-H(Y/X)
#' @param X
#' @param Y
#' @keywords entropy
#' @export
#' @examples
#' entropy.mi()
entropy.mi<-function(X,Y) {

   tmp<-entropy.Xsumup(X)
   nb_classX<-tmp$nb_classX
   val_classX<-tmp$val_classX
   
   tmp<-entropy.Xsumup(Y)
   nb_classY<-tmp$nb_classX
   val_classY<-tmp$val_classX
   
   tmp1<-entropy.hand(X,Y)
   tmp2<-entropy.hknow(X,Y)
   tmp3<-entropy.hknow(Y,X)
   mi<-tmp1-tmp2-tmp3
   return(mi)
}