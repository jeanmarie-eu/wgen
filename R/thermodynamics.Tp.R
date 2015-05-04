#' meteorology.Tp
#'
#' Potential temperature: temperature that the parcel would acquire if adiabatically brought to a standard reference pressure
#' @param T
#' @param p
#' @keywords meteorology
#' @export
#' @examples
#' meteorology.Tp()
meteorology.Tp<-function(T,p){
   results<-(T+meteorology.constants$K)*((1000/p)^(meteorology.constants$R_sd/meteorology.constants$C_pd))-meteorology.constants$K
   return(results)
}