#' thermodynamics.e
#'
#' the non-staturated and saturated vapour pressure; expression based on Bolton(1980)
#' REM: Other more complex formulae for saturation vapour pressure on liquid water and on ice can be obtained through polinomical adjustments (Flatan et al. 1992) 
#' @param Td
#' @keywords thermodynamics
#' @export
#' @examples
#' thermodynamics.e()
thermodynamics.e<-function(Td){
   results<-thermodynamics.es(Td)
   return(results)
}