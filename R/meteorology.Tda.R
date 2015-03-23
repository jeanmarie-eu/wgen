#' meteorology.Tda
#'
#' temperature on the dry adiabatic; poisson's equation
#' @param T
#' @param p
#' @keywords meteorology
#' @export
#' @examples
#' meteorology.Tda()
meteorology.Tda<-function(T,p){
   results<-(T+meteorology.constants$K)*((p/1000)^(meteorology.constants$R_sd/meteorology.constants$C_pd))-meteorology.constants$K
   return(results)
}