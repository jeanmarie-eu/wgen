#' thermodynamics.lcl
#'
#' lcl: Lifted Condensation Level, pressure and temperature
#' Because shallow convection mixes the boundry layer before afternoon convection is initiated, 
#' it makes sense to graphically estimate mean values of mixing ratio and potential temperature and 
#' to use them to determine the LCL, LFC, and EL. 
#' @param T
#' @param mr
#' @param p
#' @keywords thermodynamics
#' @export
#' @examples
#' thermodynamics.lcl()
thermodynamics.lcl<-function(T,mr,p) { #T,mr,p scalar
   if (!is.na(T) & !is.na(mr) & !is.na(p)) { 
      Tp<-thermodynamics.Tp(T,p)
      tmp<-seq(1050,100,-0.01) #pressure
      error<-abs(thermodynamics.Tmrp(mr=mr,tmp)-thermodynamics.Tda(Tp,tmp))
      plcl<-tmp[which(error==min(error))]
      Tlcl<-thermodynamics.Tmrp(mr,plcl)
      mrlcl<-mr	  
   } else {
     plcl<-NA
	 Tlcl<-NA
	 mrlcl<-NA
   }   
   return(list(plcl=plcl,Tlcl=Tlcl,mrlcl=mrlcl))
}
