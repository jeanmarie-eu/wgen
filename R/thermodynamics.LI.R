#' meteorology.LI
#'
#' LI: Lifted Index: T500-Tmoistadiab500
#' @param T
#' @param Twbp
#' @param p
#' @keywords meteorology
#' @export
#' @examples
#' meteorology.LI()
meteorology.LI<-function(T,Twbp,p){
   T500<-T[which(p==500)]
   Tmoistadiab500<-meteorology.moistadiab(Twbp,500)
  
   results<-T500-Tmoistadiab500
   return(results)
}