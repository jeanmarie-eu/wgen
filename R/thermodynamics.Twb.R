#' thermodynamics.Twb
#'
#' Wet bulb temperature
#' @param T
#' @param mr
#' @param p
#' @keywords thermodynamics
#' @export
#' @examples
#' thermodynamics.Twb()
thermodynamics.Twb<-function(T,mr,p){ #T,Td,mr,p scalar
   if (!is.na(T) & !is.na(mr) & !is.na(p)) {
      tmp<-thermodynamics.lcl(T,mr,p)
      tmp2<-seq(-40,40,0.1) #temperature
      error<-abs(tmp$Tlcl-thermodynamics.moistadiab(tmp2,tmp$plcl))
      results<-thermodynamics.moistadiab(tmp2[which(error==min(error))], p)
   } else{
      results<-NA
   }   
   return(results)
}