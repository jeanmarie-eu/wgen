#' analyseC.readaction
#'
#' read actions to be done
#' @param action
#' @keywords classification analysis
#' @export
#' @examples
#' analyseC.readaction()
analyseC.readaction<-function(action){

   if (exists("TEMPORAL") | !exists("TEMPORAL"))   TEMPORAL<-FALSE	
   if (exists("ENTROPY") | !exists("ENTROPY"))     ENTROPY<-FALSE	
   if (exists("SHIFT") | !exists("SHIFT"))         SHIFT<-FALSE
   if (exists("YEARLY") | !exists("YEARLY"))       YEARLY<-FALSE
   if (exists("MONTHLY") | !exists("MONTHLY"))     MONTHLY<-FALSE
   
   if (length(action) > 0) {
      for (i in 1:length(action)) assign(action[i], TRUE) 
   } else stop("action required: 'ENTROPY', 'SHIFT', 'YEARLY', 'MONTHLY',...")  
  	 
   filterTime<-"NONE"
   if (YEARLY)  filterTime<-"YEARLY"
   if (MONTHLY) filterTime<-"MONTHLY"
   
   #to be improved to make more automatic and independant
   results<-list(TEMPORAL=TEMPORAL,
                 ENTROPY=ENTROPY,
                 SHIFT=SHIFT,
				 filterTime=filterTime
				 )
   return(results)

}
