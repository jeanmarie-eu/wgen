#' meteorology.mr
#'
#' the mixing ratio of the mass vapor to the mass of dry air, in function of the water vapor pressure of the saturated air (e) and the pressure of the saturated air (p-e)
#' @param e
#' @param p
#' @keywords meteorology
#' @export
#' @examples
#' meteorology.mr()
meteorology.mr<-function(e,p){
   results<-meteorology.constants$eps*e/(p-e)
   return(results)
}