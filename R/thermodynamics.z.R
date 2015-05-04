#' meteorology.z
#'
#' elevation
#' @param z
#' @param T
#' @keywords meteorology
#' @export
#' @examples
#' meteorology.z()
meteorology.z<-function(z,T){ #elevation
   results<-(-meteorology.constants$R*(T+meteorology.constants$K)/meteorology.constants$g)*log10(p/meteorology.constants$p0)
   #results<-1013.25*(1-0.0065*z/(meteorology.K+15))^5.255
   return(results)
}
