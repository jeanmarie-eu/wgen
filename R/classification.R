#' classification
#'
#' spatial or temporal classification of a space-time field
#' @param method so far som only
#' @param TEMPORAL default TRUE
#' @param values
#' @param nbclass default c(3,4)
#' @keywords classifcation
#' @export
#' @examples
#' classification()
classification<-function(method, #"som","pca"...
                         TEMPORAL=TRUE,   
						 values,
                         nbclass=c(3,4)) {

   #Spatial or temporal classification						 
   if (TEMPORAL) {
      data<-as.matrix(values)
   } else data<-t(as.matrix(values))
   
   #number of classes
   param_grid<-vector(length=2)
   param_grid[1]<-nbclass[1]
   param_grid[2]<-nbclass[2]
   nb_class<-param_grid[1]*param_grid[2]
   
   if (method=="som") { 
      som.caracteristics<-som(data, grid = somgrid(param_grid[1], param_grid[2], "hexagonal"),toroidal=TRUE,rlen = 500)			 
      
	  #node vectors
      node<-som.caracteristics$codes
	  #results classif
	  classif<-map(som.caracteristics,data)$unit.classif
	  
   } else {
      #to be written
   }
   
   results<-list(node=node,
                 classif=classif)
				 
   return(results)				 
   
}
