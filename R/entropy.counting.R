#' entropy.counting
#'
#' counting X and Y
#' @param X
#' @param Y
#' @keywords entropy
#' @export
#' @examples
#' entropy.counting()
entropy.counting<-function(X,Y) {  

   tmp<-entropy.Xsumup(X)
   nb_classX<-tmp$nb_classX
   val_classX<-tmp$val_classX
   
   tmp<-entropy.Xsumup(Y)
   nb_classY<-tmp$nb_classX
   val_classY<-tmp$val_classX
   
   counting<-matrix(0,nr=nb_classX,nc=nb_classY)
   for (i in 1:nb_classX) {
      for (j in 1:nb_classY) {
         counting[i,j]<-length(which((X==val_classX[i])  &  (Y==val_classY[j])))   
      }	  
   }   
   rownames(counting)<-val_classX
   colnames(counting)<-val_classY
   return(counting)
} 