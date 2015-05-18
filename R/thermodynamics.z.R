#' thermodynamics.z
#'
#' elevation
#' @param p
#' @param T
#' @keywords thermodynamics
#' @export
#' @examples
#' thermodynamics.z()
thermodynamics.z<-function(p,T){ 
   results<-(-1)*
            (thermodynamics.constants$R*(T+thermodynamics.constants$K)/thermodynamics.constants$g)*
            log10(p/thermodynamics.constants$p0)
   return(results)
}
