#' thermodynamics.lfcel
#'
#' lfc: Level of Free Convection
#' el: Equilibrium Level
#' Because shallow convection mixes the boundry layer before afternoon convection is initiated, 
#' it makes sense to graphically estimate mean values of mixing ratio and potential temperature and 
#' to use them to determine the LCL, LFC, and EL. 
#' @param Twbp
#' @param plcl
#' @param T
#' @param p
#' @keywords thermodynamics
#' @export
#' @examples
#' thermodynamics.lfcel()
thermodynamics.lfcel<-function(Twbp,plcl,T,p) { #Twb,plcl scalar
   if (!is.na(Twbp) & !is.na(plcl)) {
      
	  moistadiab<-rep(NA,length(p))
      for (i in 1: length(moistadiab)) moistadiab[i]<-thermodynamics.moistadiab(Twbp, p[i])
	  indice_cape<-which((moistadiab-T)>0)

	  if (length(indice_cape)>0) {
	     plfc<-p[indice_cape][1]
	     pel<-p[indice_cape][length(indice_cape)]
	  } 
   } else {
      plfc<-NA
	  pel<-NA
   }	  
   return(list(plfc=plfc,pel=pel)) 
}
