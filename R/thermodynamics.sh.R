#' thermodynamics.sh
#'
#' the specific humidity, in function of the water vapor pressure of the saturated air (e) and the pressure of the saturated air (p-e)
#' @param e
#' @param p
#' @keywords thermodynamics
#' @export
#' @examples
#' thermodynamics.sh()
thermodynamics.sh<-function(e,p){
   results<-(1000*(0.62197*e))/(p-(1-0.62197)*e)
   return(results)
}