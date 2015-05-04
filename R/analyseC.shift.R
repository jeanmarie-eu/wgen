
#' analyseC.shift
#'
#' Temporal shift of the complete matrix of timeseries
#' @param values format v1,...,vn
#' @param date
#' @param shift
#' @keywords classification analysis
#' @export
#' @examples
#' analyseC.shift()
analyseC.shift<-function(value,date,shift){ 
   
   value<-as.matrix(value)
   nobs<-dim(value)[1]
   nbcol<-dim(value)[2]
   
   if (length(shift)==1) shift<-rep(shift,nbcol)

   date_o<-matrix(NA,nr=nobs,nc=nbcol)
   value_o<-matrix(NA,nr=nobs,nc=nbcol)
   
   for (i in 1:length(shift)) {
      date_o[(1:(nobs-shift[i])),i]<-as.character(date[((1+shift[i]):nobs)])
      value_o[(1:(nobs-shift[i])),i]<-value[((1+shift[i]):nobs),i]
   }
   
   results<-list(date_o=date_o,
                 value_o=value_o)
				 
   return(results)				 

}
