
#' analyseTS.shift
#'
#' Temporal shift of the complete matrix of timeseries
#' @param valeu
#' @param date
#' @param shift
#' @keywords entropy
#' @export
#' @examples
#' analyseTS.shift()
analyseTS.shift<-function(value,date,shift){
   
   value<-as.matrix(value)
   nobs<-dim(value)[1]
   nbcol<-dim(value)[2]
   
   date_o<-vector(length=(nobs-shift))
   value_o<-matrix(nr=(nobs-shift),nc=nbcol)
   
   date_o[(1:(nobs-shift))]<-as.character(date[1:(nobs-shift)])
   value_o[(1:(nobs-shift)),]<-value[((1+shift):nobs),]

   
   results<-list(date_o=date_o,
                 value_o=value_o)
				 
   return(results)				 

}
