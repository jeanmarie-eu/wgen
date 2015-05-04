#' analyseC.entropy
#'
#' Entropy analysis of a timeseries
#' @param values1, format v1,...,vn
#' @param date1
#' @param values2, format v1,...,vn
#' @param date2
#' @param filterTime
#' @keywords classification analysis
#' @export
#' @examples
#' analyseC.entropy()
analyseC.entropy<-function(values1,date1,values2=NULL,date2=NULL,filterTime){
   
   value<-as.matrix(value)
   nbTS<-dim(value)[2]
   
   if ((is.null(value2)) &  (is.null(date2))) {
      value2<-value1
	  date2<-date1
   }

   results<-c()
   for (i in 1:nbTS) {
      for (j in 1:nbTS) {
         tmp<-analyseC.entropy.XY.filterTime(X=value1[,i],Y=value2[,j],dateX=date1,dateY=date2[,j],filterTime)
	     TS_i<-rep(i,dim(tmp)[1])
		 TS_j<-rep(j,dim(tmp)[1])
         results<-rbind(results,
		                cbind(TS_i,TS_j,tmp))
      }		 
   } 
   
   return(results) 
}
