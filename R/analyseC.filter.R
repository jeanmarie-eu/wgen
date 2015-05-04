#' analyseC.filter
#'
#' Filter
#' @param X
#' @param Y
#' @param dateX
#' @param dateY
#' @param filter
#' @keywords classification analysis
#' @export
#' @examples
#' analyseC.filter()
analyseC.filter<-function(X,Y,dateX,dateY,filter) {
   
   if (filter=="YEARLY")  char<-c(1,4)
   if (filter=="MONTHLY") char<-c(6,7)    
   
   results<-c()
   if (  (filter=="YEARLY") || (filter=="MONTHLY") ) {
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
         tmp<-analyseTS.entropy.XY(X=X_tmp,Y=Y_tmp)
	     