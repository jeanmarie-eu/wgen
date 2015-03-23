#' entropy.DU
#'
#' D(X,Y): Universal metric, Jaccard distance, D(X,Y)=d(X,Y)/H(X,Y)=1-I(X,Y)/H(X,Y)
#' @param X
#' @param Y
#' @keywords entropy
#' @export
#' @examples
#' entropy.DU()
entropy.DU<-function(X,Y) {

   tmp<-entropy.Xsumup(X)
   nb_classX<-tmp$nb_classX
   val_classX<-tmp$val_classX
   
   tmp<-entropy.Xsumup(Y)
   nb_classY<-tmp$nb_classX
   val_classY<-tmp$val_classX
   
   tmp1<-entropy.hand(X,Y)
   tmp2<-entropy.mi(X,Y) 
   DU<-1-tmp2/tmp1
   return(DU)
}