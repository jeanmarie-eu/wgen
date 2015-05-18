#' thermodynamics.wobf
#'
#' wobf 
#' COPYRIGHT REMARK:  Due to a lack of time, this function has been adapted from the R-package "radiosonde"
#' http://www.image.ucar.edu/Software/RadioSonde/
#' @param T
#' @keywords thermodynamics
#' @export
#' @examples
#' thermodynamics.wobf()
thermodynamics.wobf<-function(T) {
	x<-T-20
	pol<-x
	wbts<-x
	
	pol[which(x<=0)]<-1+x[which(x<=0)]*(-0.0088416604999999992+x[which(x<=0)]*(
			           0.00014714143000000001+x[which(x<=0)]*(-9.6719890000000006e-07+
			           x[which(x<=0)]*(-3.2607217000000002e-08+x[which(x<=0)]*(
			           -3.8598072999999999e-10)))))
					   
	pol[which(x>0)]<-1+x[which(x>0)]*(0.0036182989000000001+x[which(x>0)]*(-1.3603273e-05+
			         x[which(x>0)]*(4.9618921999999997e-07+x[which(x>0)]*(
			         -6.1059364999999998e-09+x[which(x>0)]*(3.9401550999999998e-11+
			         x[which(x>0)]*(-1.2588129e-13+x[which(x>0)]*(1.668828e-16)))))))
					 
   wbts[which(x<=0)]<-15.130000000000001/pol[which(x<=0)]^4

   wbts[which(x>0)]<-29.93/pol[which(x>0)]^4+0.95999999999999996*x[which(x>0)]-14.800000000000001   
	
   return(wbts)
}
