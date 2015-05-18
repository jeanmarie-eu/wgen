#' analyseC.period
#'
#' period
#' @param date1
#' @param period
#' @keywords classification analysis
#' @export
#' @examples
#' analyseC.period()
analyseC.period<-function(date1,period) {
   
   if (period=="YEARLY")  char<-c(1,4)
   if (period=="MONTHLY") char<-c(6,7)
   #...
   
   tmp<-as.numeric(substring(date1, char[1], char[2]))
   period_seq<-seq(min(tmp),max(tmp))
   nb<-length(period_seq)
   
   results<-list(char=char,
                 period_seq=period_seq)
				 
   return(results)

}   

	     