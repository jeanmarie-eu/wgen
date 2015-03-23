#' entropy.hknow
#'
#' H(X/Y)
#' @param X
#' @param Y
#' @keywords entropy
#' @export
#' @examples
#' entropy.hknow()
entropy.hknow<-function(X,Y) { 

   tmp<-entropy.Xsumup(X)
   nb_classX<-tmp$nb_classX
   val_classX<-tmp$val_classX
   
   tmp<-entropy.Xsumup(Y)
   nb_classY<-tmp$nb_classX
   val_classY<-tmp$val_classX
   
   hknow_tmp<-matrix(0,nr=nb_classX,nc=nb_classY) 
   Pij<-entropy.Pij(X,Y)
   Pj<-entropy.Pi(Y)
	  
   for (i in 1:nb_classX) {
      for (j in 1:nb_classY) {
	     if ((Pij[i,j])!=0) {
            hknow_tmp[i,j]<-(Pij[i,j])*log(Pj[j]/Pij[i,j])/log(2)
		} else hknow_tmp[i,j]<-0 								  
      }
   }	
   hknow<-sum(hknow_tmp,na.rm=TRUE)
   return(hknow)
}