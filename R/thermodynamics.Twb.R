#' meteorology.Twb
#'
#' Wet bulb temperature
#' @param T
#' @param mr
#' @param p
#' @keywords meteorology
#' @export
#' @examples
#' meteorology.Twb()
meteorology.Twb<-function(T,mr,p){ #T,Td,mr,p scalar
   if (!is.na(T) & !is.na(mr) & !is.na(p)) {
      tmp<-meteorology.lcl(T,mr,p)
      tmp2<-seq(-40,40,0.1) #temperature
      error<-abs(tmp$Tlcl-meteorology.moistadiab(tmp2,tmp$plcl))
      results<-meteorology.moistadiab(tmp2[which(error==min(error))], p)
   } else{
      results<-NA
   }   
   return(results)
}