#' thermodynamics.Tp
#'
#' Potential temperature: temperature that the parcel would acquire if adiabatically brought to a standard reference pressure
#' @param T
#' @param p
#' @keywords thermodynamics
#' @export
#' @examples
#' thermodynamics.Tp()
thermodynamics.Tp<-function(T,p){
   results<-(T+thermodynamics.constants$K)*((1000/p)^(thermodynamics.constants$R_sd/thermodynamics.constants$C_pd))-thermodynamics.constants$K
   return(results)
}