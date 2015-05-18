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
   if (exists("MI") | !exists("MI"))               MI<-FALSE	
   if (exists("SHIFT") | !exists("SHIFT"))         SHIFT<-FALSE
   
   if (length(action) > 0) {
      for (i in 1:length(action)) assign(action[i], TRUE) 
   } else stop("action required: 'MI', 'SHIFT', ...")  

   
   #to be improved to make more automatic and independant
   results<-list(TEMPORAL=TEMPORAL,
                 MI=MI,
                 SHIFT=SHIFT
				 )
   return(results)

}
