#' thermodynamics.Tv
#'
#' virtual temperature
#' @param T
#' @param mr
#' @keywords thermodynamics
#' @export
#' @examples
#' thermodynamics.Tv()
thermodynamics.Tv<-function(T,mr){
   Tv<-T+mr/6 # approximation
}