#' analyseTS.class
#'
#' classification analysis
#' @param valueTS, format v1,...,vn
#' @param dateTS
#' @param action
#' @keywords entropy
#' @export
#' @examples
#' analyseTS.class()
analyseTS.class<-function(valueTS,dateTS,
                                  action="BASIC" #c("BASIC","SHIFT","YEARLY","MONTHLY")
							  ) {

   #read action parameters							  
   parameter.action<-analyseTS.readaction(action)

   # how many timeserie to be analyzed							  
   valueTS<-as.matrix(valueTS)
   nbTS<-dim(valueTS)[2]
   
   if ((nbTS==1) && (!parameter.action$SHIFT)) warning("Only one timeserie and action SHIFT not chosen...")
      
   if (parameter.action$BASIC) basic<-analyseTS.basic(value=valueTS,date=dateTS,filterTime=parameter.action$filterTime,SHIFT=parameter.action$SHIFT)						  

   
   # Results
   results.BASIC<-list()
   results.BASIC.SHIFT<-list()
   
   if (parameter.action$BASIC) {
      if (!(parameter.action$SHIFT)) results.BASIC<-basic
	  if (parameter.action$SHIFT)    results.BASIC.SHIFT<-basic						  
      
   }

   
   results<-list(BASIC=results.BASIC,
                 BASIC.SHIFT=results.BASIC.SHIFT
				 )

   return(results)
 
}