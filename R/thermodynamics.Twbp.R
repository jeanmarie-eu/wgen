#' thermodynamics.Twbp
#'
#' Potential Wet bulb temperature
#' @param T
#' @param p
#' @keywords thermodynamics
#' @export
#' @examples
#' thermodynamics.Twbp()
thermodynamics.Twbp<-function(T,p) { #T,p scalar
   if (!is.na(T) & !is.na(p)) {
      tmp<-seq(-40,40,0.1) #temperature
      error<-rep(NA,length(tmp))
      error<-abs(T-thermodynamics.moistadiab(tmp, p))
	  Twbp<-tmp[which(error==min(error))]
   }
   return(Twbp)
} 