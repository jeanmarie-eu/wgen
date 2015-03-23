#' meteorology.Td
#'
#' Dew Point Temperature
#' @param rh
#' @param T
#' @keywords meteorology
#' @export
#' @examples
#' meteorology.Td()
meteorology.Td<-function(rh,T) {
   results<-(rh/100)^(1/8)*(112+0.9*T)+0.1*T-112
   return(results)
}