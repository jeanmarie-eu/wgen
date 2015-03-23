
#' meteorology.BoundaryLayer
#'
#' # Because shallow convection mixes the boundry layer before afternoon convection is initiated, 
#' it makes sense to graphically estimate mean values of mixing ratio and potential temperature and 
#' to use them to determine the LCL, LFC, and EL. 
#' @param T
#' @param Td
#' @param Tp
#' @param mr
#' @param p
#' @keywords meteorology
#' @export
#' @examples
#' meteorology.BoundaryLayer()
meteorology.BoundaryLayer<-function(T,Td,Tp,mr,p){
  indice<-which(p>=(p[1]-100))
  T_bl<-mean(T[indice],na.rm=TRUE)
  Td_bl<-mean(Td[indice],na.rm=TRUE)
  Tp_bl<-mean(Tp[indice],na.rm=TRUE)
  mr_bl<-mean(mr[indice],na.rm=TRUE)
  p_bl<-mean(p[indice],na.rm=TRUE)
  
  results<-list(T_bl=T_bl,
                Td_bl=Td_bl,
				Tp_bl=Tp_bl,
				mr_bl=mr_bl,
				p_bl=p_bl)
  return(results)
}
