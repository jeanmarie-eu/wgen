#' meteorology.moistadiab
#'
#' saturated adiabatic 
#' COPYRIGHT REMARK:  Due to a lack of time, this function has been adapted from the R-package "radiosonde"
#' http://www.image.ucar.edu/Software/RadioSonde/
#' @param T
#' @param p
#' @keywords meteorology
#' @export
#' @examples
#' meteorology.moistadiab()
meteorology.moistadiab<-function(T,p){ #p is a scalar
	T1<-meteorology.Tda(T,p)
	e1<-meteorology.wobf(T1)-meteorology.wobf(T)
	rate<-rep(1,length(T1))
	dlt<-rep(1,length(T1))
	T2<-T1
	T2p<-T2
	e2<-e1
	while(sum(abs(dlt)>0.10000000000000001)>=1) {
	   indice<-which(abs(dlt)>0.10000000000000001)
	   T2[indice]<-T1[indice]-e1[indice]*rate[indice]
	   T2p[indice]<-meteorology.Tp(T2[indice],p)
	   e2[indice]<-T2p[indice]+meteorology.wobf(T2[indice])-meteorology.wobf(T2p[indice])-T[indice]
	   dlt[indice]<-e2[indice]*rate[indice]
	   rate[indice]<-(T2[indice]-T1[indice])/(e2[indice]-e1[indice])
	   T1[indice]<-T2[indice]
	   e1[indice]<-e2[indice]
	}
	results<-T2-dlt
	return(results)
}