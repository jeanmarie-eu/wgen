#' analyseTS.XYfilterTime
#'
#' Analysis X and Y for every month or every year or regular one
#' @param X
#' @param Y
#' @param dateX
#' @param dateY
#' @param filterTime
#' @keywords entropy
#' @export
#' @examples
#' analyseTS.XYfilterTime()
analyseTS.XYfilterTime<-function(X,Y,dateX,dateY,filterTime) {
   
   if (filterTime=="YEARLY")  char<-c(1,4)
   if (filterTime=="MONTHLY") char<-c(6,7)    
   
   results<-c()
   if (  (filterTime=="YEARLY") || (filterTime=="MONTHLY") ) {
      tmp<-as.numeric(substring(dateX, char[1], char[2]))
      period<-seq(min(tmp),max(tmp))
      nb<-length(period)
    
	  tmp<-entropy.Xsumup(X)
	  nb_classX<-tmp$nb_classX
      tmp<-entropy.Xsumup(Y)
	  nb_classY<-tmp$nb_classX
	        
      for (i in 1 : nb){
	     X_tmp<-X[which((as.numeric(substring(dateX, char[1], char[2]))==period[i]))]	  
	     Y_tmp<-Y[which((as.numeric(substring(dateY, char[1], char[2]))==period[i]))]
         tmp<-analyseTS.XY(X=X_tmp,Y=Y_tmp)
	     counting<-tmp$counting
         Pij<-tmp$Pij
         Ni<-tmp$Ni 
		 mi<-tmp$mi
		 DU<-tmp$DU
		 results<-rbind(results,list(period=period[i],
		                          counting=counting,
                                  Pij=Pij,
				                  Ni=Ni,
				                  mi=mi,
								  DU=DU))
      }
   } else if (filterTime=="NONE") {
      tmp<-analyseTS.XY(X=X,Y=Y)
	  counting<-tmp$counting
      Pij<-tmp$Pij
      Ni<-tmp$Ni
      mi<-tmp$mi	
	  DU<-tmp$DU
      results<-rbind(results,list(counting=counting,
                                  Pij=Pij,
				                  Ni=Ni,
				                  mi=mi,
								  DU=DU))	  
   } else stop("The whole period, or YEARLY or MONTHLY only so far")
   	  

   return(results)   
}
