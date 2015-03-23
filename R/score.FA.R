#' score.FA
#'
#' False Alarm
#' @param data1
#' @param data2
#' @param ths1 threshold#1
#' @param ths2 threshold#2
#' @keywords score
#' @export
#' @examples
#' score.FA()
score.FA<-function(data1, #FA
                          data2,
						  ths1,ths2) {
   
   if (length(ths1)==2) temp<-((data1>ths1[1]) & (data1<=ths1[2]))
   if (length(ths1)==1) temp<-(data1<=ths1)
   
   if (length(ths2)==2) temp2<-((data2>ths2[1]) & (data2<=ths2[2]))
   if (length(ths2)==1) temp2<-(data2>ths2)

   #BOOL<- apply( temp,1,"&")
   BOOL<-Reduce(`&`, as.data.frame(t(temp)))
   results<-BOOL & temp2
   return(results)
}