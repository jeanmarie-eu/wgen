#' analyseC.mi
#'
#' MI between variables
#' @param values1, format v1,...,vn
#' @param date1
#' @param values2, format v1,...,vn
#' @param date2
#' @param period_seq
#' @keywords classification analysis
#' @export
#' @examples
#' analyseC.mi()
analyseC.mi<-function(values1,date1,values2=NULL,period=NULL){
   
   nb_v<-dim(values1)[2]
   
   if (is.null(values2)) values2<-values1

   if (!is.null(period)) {
      nb_p<-length(period_seq)
 
      results<-matrix(NA,nc=4,nr=nb_p*nb_v^2)
	  ind_tmp<-0
	  for (k in 1 : nb_p){
	     values1_tmp<-values1[which((as.numeric(substring(date1, period$char[1], period$char[2]))==period$period_seq[k])),]	  
         values2_tmp<-values2[which((as.numeric(substring(date1, period$char[1], period$char[2]))==period$period_seq[k])),]
         for (i in 1:nb_v) {
            for (j in 1:nb_v) {
			   ind_tmp<-ind_tmp+1
               tmp<-entropy.mi(X=values1_tmp[,i],Y=values2_tmp[,j])
	           results[ind_tmp,1:4]<-c(k,i,j,tmp)
			}
         }		 
      }  
   } else {
      results<-matrix(NA,nc=3,nr=nb_v^2)
	  ind_tmp<-0
      for (i in 1:nb_v) {
            for (j in 1:nb_v) {
			   ind_tmp<-ind_tmp+1
               tmp<-entropy.mi(X=values1[,i],Y=values2[,j])
	           results[ind_tmp,1:3]<-c(i,j,tmp)
			}
         }    
   }
   return(results) 
}
