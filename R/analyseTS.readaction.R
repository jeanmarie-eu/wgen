#' analyseTS.readaction
#'
#' read actions to be done
#' @param action
#' @keywords entropy
#' @export
#' @examples
#' analyseTS.readaction()
analyseTS.readaction<-function(action){

   if (exists("BASIC") | !exists("BASIC"))   BASIC<-FALSE	
   if (exists("SHIFT") | !exists("SHIFT"))   SHIFT<-FALSE
   if (exists("YEARLY") | !exists("YEARLY"))  YEARLY<-FALSE
   if (exists("MONTHLY") | !exists("MONTHLY")) MONTHLY<-FALSE
   
   if (length(action) > 0) {
      for (i in 1:length(action)) assign(action[i], TRUE) 
   } else stop("action required: 'BASIC', 'SHIFT', 'YEARLY', 'MONTHLY',...")  
  	 
   filterTime<-"NONE"
   if (YEARLY)  filterTime<-"YEARLY"
   if (MONTHLY) filterTime<-"MONTHLY"
   
   #to be improved to make more automatic and independant
   results<-list(BASIC=BASIC,
                 SHIFT=SHIFT,
				 filterTime=filterTime
				 )
   return(results)

}
