#' meteorology.Tmrp
#'
#' temperature on a given mr and p; table 1 on page 7 of stipanuk (1973)
#' @param mr
#' @param p
#' @keywords meteorology
#' @export
#' @examples
#' meteorology.Tmrp()
meteorology.Tmrp<-function(mr, p) { 
   c1 <- 0.049864645499999999
   c2 <- 2.4082965000000001
   c3 <- 7.0747499999999999
   c4 <- 38.9114
   c5 <- 0.091499999999999998
   c6 <- 1.2035
   tmp <- log10((mr*p)/(meteorology.constants$eps*1000+mr))
   results<-10^(c1*tmp+c2)-c3+c4*((10^(c5*tmp)-c6)^2)-meteorology.constants$K
   return(results)
}