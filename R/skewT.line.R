#' skewT.line
#'
#' plot a Temperature radiosounding on the skewT graph, choice: Temperature, Wet bulb temperature, Dew point temperature, Potential Wet bulb temperature, Virtual Temperature 
#' @param p_min
#' @param p_max
#' @param T_min
#' @param T_max
#' @param T
#' @param p
#' @param T_name
#' @keywords skewT
#' @export
#' @examples
#' skewT.line()
skewT.line<-function(p_min,p_max,T_min,T_max,T,p,T_name){
   CTE<-skewT.ab(T_min,T_max,p_max,p_min)
   tmp_x<-skewT.x(CTE$a,CTE$b,CTE$origin,T,p)
   if (T_name=="T") lines(tmp_x,p,col="red",lwd=2.5)          #Temperature
   if (T_name=="Twb") lines(tmp_x,p,col="blue",lwd=2.5)       #Wet bulb temperature 
   if (T_name=="Td") lines(tmp_x,p,col="blue",lwd=2.5,lty=2)  #Dew point temperature 
   if (T_name=="Twbp") lines(tmp_x,p,col="black",lwd=2,lty=1)  #Potential Wet bulb temperature
   if (T_name=="Tv") lines(tmp_x,p,col="gray",lwd=2,lty=1)  #Virtual Temperature
}