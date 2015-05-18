#' thermodynamics.pw
#'
#' precipitable water (in mm) up to a minimum pressure
#' @param p
#' @param Td
#' @param T
#' @param minp
#' @keywords thermodynamics
#' @export
#' @examples
#' thermodynamics.pw()
thermodynamics.pw<-function(p,Td,T,minp=400) {
   e<-thermodynamics.e(Td)
   sh<-thermodynamics.sh(e,p)
   sh2<-sh
   sh2[1:(length(sh)-1)]<-sh[2:length(sh)]
   sh2[length(sh)]<-NA
   p2<-p
   p2[1:(length(p)-1)]<-p[2:length(p)]
   p2[length(p)]<-NA

   tmp<-0.1*(sh+sh2)/2*(p-p2)/thermodynamics.constants$g
   results<-sum(tmp[which(p>minp)],na.rm=TRUE)
   return(results)
}