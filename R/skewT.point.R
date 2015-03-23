#' skewT.point
#'
#' plot a Temperature point on the skewT graph, choice: Temperature, Wet bulb temperature, Dew point temperature, Potential Wet bulb temperature, Virtual Temperature 
#' @param p_min
#' @param p_max
#' @param T_min
#' @param T_max
#' @param T
#' @param p
#' @keywords skewT
#' @export
#' @examples
#' skewT.point()
skewT.point<-function(p_min,p_max,T_min,T_max,T,p){
   CTE<-skewT.ab(T_min,T_max,p_max,p_min)
   tmp_x<-skewT.x(CTE$a,CTE$b,CTE$origin,T,p)
   points(tmp_x,p)
}