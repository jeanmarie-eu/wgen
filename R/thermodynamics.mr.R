#' thermodynamics.mr
#'
#' the mixing ratio of the mass vapor to the mass of dry air, in function of the water vapor pressure of the saturated air (e) and the pressure of the saturated air (p-e)
#' @param e
#' @param p
#' @keywords thermodynamics
#' @export
#' @examples
#' thermodynamics.mr()
thermodynamics.mr<-function(e,p){
   results<-thermodynamics.constants$eps*e/(p-e)
   return(results)
}