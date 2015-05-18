#' thermodynamics.Tda
#'
#' temperature on the dry adiabatic; poisson's equation
#' @param T
#' @param p
#' @keywords thermodynamics
#' @export
#' @examples
#' thermodynamics.Tda()
thermodynamics.Tda<-function(T,p){
   results<-(T+thermodynamics.constants$K)*((p/1000)^(thermodynamics.constants$R_sd/thermodynamics.constants$C_pd))-thermodynamics.constants$K
   return(results)
}