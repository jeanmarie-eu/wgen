#' analyseTS.XY
#'
#' Analysis X and Y
#' @param X
#' @param Y
#' @keywords entropy
#' @export
#' @examples
#' analyseTS.XY()
analyseTS.XY<-function(X,Y) {

   #contingency, percentage ...etc
   sumup<-entropy.Xsumup(X)
   counting<-entropy.counting(X,Y) #contingency table
   Pij<-entropy.Pij(X,Y)           #probability Pij
   Ni<-entropy.Ni(X)               #number Ni
   mi<-entropy.mi(X,Y)             #Mutual Information
   DU<-entropy.DU(X,Y)             #Jaccard distance
   results<-list(nb_class=sumup$nb_class,
                 val_classX=sumup$val_classX,
                 counting=counting,
                 Pij=Pij,
				 Ni=Ni,
				 mi=mi,
				 DU=DU)
   return(results)
   
}