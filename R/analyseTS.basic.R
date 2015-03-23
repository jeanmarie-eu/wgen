#' analyseTS.basic
#'
#' Basic analysis of a timeseries
#' @param values, format v1,...,vn
#' @param date
#' @param filterTime
#' @param SHIFT
#' @keywords entropy
#' @export
#' @examples
#' analyseTS.basic()
analyseTS.basic<-function(value,date,filterTime,SHIFT){
   
   value<-as.matrix(value)
   nbTS<-dim(value)[2]
   
   if (SHIFT) {
      tmp<-analyseTS.shift(value,date,shift=1)
      value2<-tmp$value_o
      date2<-tmp$date_o
      nobs<-min(dim(value)[1],dim(value2)[1])
      value<-value[1:nobs,]
      value2<-value2[1:nobs,]
      date<-date[1:nobs]
      date2<-date2[1:nobs]
   } else {
      value2<-value
      date2<-date
   }
   
   results<-c()
   for (i in 1:nbTS) {
      for (j in 1:nbTS) {
         tmp<-analyseTS.XYfilterTime(X=value[,i],Y=value2[,j],dateX=date,dateY=date2,filterTime)
	     TS_i<-rep(i,dim(tmp)[1])
		 TS_j<-rep(j,dim(tmp)[1])
         results<-rbind(results,
		                cbind(TS_i,TS_j,tmp))
      }		 
   } 
   
   return(results) 
}
