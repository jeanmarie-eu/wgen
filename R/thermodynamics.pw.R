#' meteorology.pw
#'
#' precipitable water (in mm) up to a minimum pressure
#' @param p
#' @param Td
#' @param T
#' @param minp
#' @keywords meteorology
#' @export
#' @examples
#' meteorology.pw()
meteorology.pw<-function(p,Td,T,minp=400) {
   e<-meteorology.e(Td)
   sh<-meteorology.sh(e,p)
   sh2<-sh
   sh2[1:(length(sh)-1)]<-sh[2:length(sh)]
   sh2[length(sh)]<-NA
   p2<-p
   p2[1:(length(p)-1)]<-p[2:length(p)]
   p2[length(p)]<-NA

   tmp<-0.1*(sh+sh2)/2*(p-p2)/meteorology.constants$g
   results<-sum(tmp[which(p>minp)],na.rm=TRUE)
   return(results)
}