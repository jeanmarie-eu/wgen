#' meteorology.sh
#'
#' the specific humidity, in function of the water vapor pressure of the saturated air (e) and the pressure of the saturated air (p-e)
#' @param e
#' @param p
#' @keywords meteorology
#' @export
#' @examples
#' meteorology.sh()
meteorology.sh<-function(e,p){
   results<-(1000*(0.62197*e))/(p-(1-0.62197)*e)
   return(results)
}