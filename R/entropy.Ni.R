
#' entropy.Ni
#'
#' Number
#' @param X
#' @keywords entropy
#' @export
#' @examples
#' entropy.Ni()
entropy.Ni<-function(X) {
   tmp<-entropy.Xsumup(X)
   nb_classX<-tmp$nb_classX
   val_classX<-tmp$val_classX
   
   Ni<-t(as.matrix(rep(0,nb_classX)))
   for (i in 1:nb_classX) {
      Ni[1,i]<-length(which(X==val_classX[i]))
   }
   colnames(Ni)<-val_classX
   return(Ni)   
}
