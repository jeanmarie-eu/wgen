#' meteorology.Twbp
#'
#' Potential Wet bulb temperature
#' @param T
#' @param p
#' @keywords meteorology
#' @export
#' @examples
#' meteorology.Twbp()
meteorology.Twbp<-function(T,p) { #T,p scalar
   if (!is.na(T) & !is.na(p)) {
      tmp<-seq(-40,40,0.1) #temperature
      error<-rep(NA,length(tmp))
      error<-abs(T-meteorology.moistadiab(tmp, p))
	  Twbp<-tmp[which(error==min(error))]
   }
   return(Twbp)
} 