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
                   action=c("TEMPORAL","MI"), #c("MI","SHIFT",...)
				   period=NULL #"YEARLY", "MONTHLY", 2011, 06,...
				   ) {

   #read action parameters							  
   parameter.action<-analyseC.readaction(action)

   #Spatial or temporal classification						 
   if (parameter.action$TEMPORAL) {
      if (is.matrix(values)) { 
	     values1<-t(values)  
	  } else values1<-as.matrix(values)
	  
   } else {
      if (is.matrix(values)) { 
	     values1<-values
	  } else values1<-as.matrix(values)
   }
   
   nbcol<-dim(values1)[2]
   nbrow<-dim(values1)[1]
   
   #short warning
   if ((nbcol==1) && (!parameter.action$SHIFT)) warning("Only one variable and action SHIFT not chosen...")
            
   #period
   if (!is.null(period)) {
         period2<-analyseC.period(date1=dates,period)
   } else period2<-NULL
   
   #action		 
   if(parameter.action$SHIFT) {
      tmp<-analyseC.shift(value=values1,date=dates,shift=1)
      values2<-tmp$value_o
      date2<-tmp$date_o
      nobs<-min(dim(values1)[1],dim(values2)[1])
      values1<-values1[1:nobs,]
      values2<-values2[1:nobs,]
      date1<-dates[1:nobs]
   } else {
      dates1<-dates
      values2<-NULL
   }	  
   
   if (parameter.action$MI) mi<-analyseC.mi(values1=values1,date1=dates1,values2=values2,period=period2)						  

   
   # Results
   results.MI<-list()
   results.MI.SHIFT<-list()
   
   if (parameter.action$MI) {
      if (!(parameter.action$SHIFT)) results.MI<-mi
	  if (parameter.action$SHIFT)    results.MI.SHIFT<-mi 
      
   }

   results<-list(MI=results.MI,
                 MI.SHIFT=results.MI.SHIFT
				 )

   return(results)
 
}