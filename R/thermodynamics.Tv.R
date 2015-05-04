#' meteorology.Tv
#'
#' virtual temperature
#' @param T
#' @param mr
#' @keywords meteorology
#' @export
#' @examples
#' meteorology.Tv()
meteorology.Tv<-function(T,mr){
   Tv<-T+mr/6 # approximation
}