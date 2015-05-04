#' analyseC
#'
#' classification analysis
#' @param values, format v1,...,vn
#' @param date
#' @param action
#' @keywords classification analysis 
#' @export
#' @examples
#' analyseC()
analyseC<-function(values,
                   dates,
                   action=c("TEMPORAL","ENTROPY") #c("ENTROPY","SHIFT","YEARLY","MONTHLY")
				   ) {

   #read action parameters							  
   parameter.action<-analyseTS.readaction(action)

   #Spatial or temporal classification						 
   if (TEMPORAL) {
      v<-t(values)
   } else v<-values
							  
   nbcol<-dim(v)[2]
   nbrow<-dim(v)[1]
   
   if ((nbcol==1) && (!parameter.action$SHIFT)) warning("Only one variable and action SHIFT not chosen...")
   
   if(parameter.action$SHIFT)) {
      tmp<-analyseC.shift(value=v,date=dates,shift=1)
      v2<-tmp$value_o
      date2<-tmp$date_o
      #nobs<-min(dim(value)[1],dim(value2)[1])
      #value1<-valueTS[1:nobs,]
      #value2<-value2[1:nobs,]
      #date1<-dateTS[1:nobs]
      #date2<-date2[1:nobs]
   } else {
      value2<-NULL
      date2<-NULL
   }
   
   if (parameter.action$ENTROPY) entropy<-analyseC.entropy(value1=value1,date1=date1,value2=value2,date2=date2,filterTime=parameter.action$filterTime)						  

   
   # Results
   results.ENTROPY<-list()
   results.ENTROPY.SHIFT<-list()
   
   if (parameter.action$ENTROPY) {
      if (!(parameter.action$SHIFT)) results.ENTROPY<-entropy
	  if (parameter.action$SHIFT)    results.ENTROPY.SHIFT<-entropy						  
      
   }

   
   results<-list(ENTROPY=results.ENTROPY,
                 ENTROPY.SHIFT=results.ENTROPY.SHIFT
				 )

   return(results)
 
}