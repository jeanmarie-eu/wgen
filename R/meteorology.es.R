#' meteorology.es
#'
#' the staturated and saturated vapour pressure; expression based on Bolton(1980)
#' REM: Other more complex formulae for saturation vapour pressure on liquid water and on ice can be obtained through polinomical adjustments (Flatan et al. 1992) 
#' @param T
#' @keywords meteorology
#' @export
#' @examples
#' meteorology.es()
meteorology.es<-function(T){
   results<-6.112*exp((17.67*T)/(T+243.5))
   return(results)
}