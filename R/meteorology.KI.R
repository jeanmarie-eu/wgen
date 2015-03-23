#' meteorology.KI
#'
#' KI: K-index: (850 hPa temperature - 500 hPa temperature) + 850 hPa dew point - 700 hPa dew point depression
#' @param T
#' @param Td
#' @param p
#' @keywords meteorology
#' @export
#' @examples
#' meteorology.KI()
meteorology.KI<-function(T,Td,p){
   T850<-T[which(p==850)]
   T700<-T[which(p==700)]
   T500<-T[which(p==500)]
   Td850<-Td[which(p==850)]
   Td700<-Td[which(p==700)]
   
   results<-T850-T500+Td850-(T700-Td700)
   return(results)
}