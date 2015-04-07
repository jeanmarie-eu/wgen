#' classification
#'
#' spatial or temporal classification of a space-time field
#' @param geoW format geoWeather
#' @param method so far som only
#' @param TEMPORAL default TRUE
#' @param nbclass default c(3,4)
#' @keywords classifcation
#' @export
#' @examples
#' classification()
classification<-function(geoW,   
                         method, #"som","pca"...
                         TEMPORAL=TRUE,   
						 nbclass=c(3,4),
						 FIGURE) {

   #Spatial or temporal classification						 
   if (TEMPORAL) {
      v<-t(as.matrix(geoW$values))
   } else v<-as.matrix(geoW$values)
   
   if (method=="som") { 
      som.caracteristics<-som(v, grid = somgrid(nbclass[1], nbclass[2], "hexagonal"),toroidal=TRUE,rlen = 500)			 
      
	  #node vectors
      node<-som.caracteristics$codes
	  #results classif
	  classif<-map(som.caracteristics,v)$unit.classif
	  
   } else {
      #to be written
   }
   
   
   if (FIGURE) {
      
	  op <- par(no.readonly = TRUE)
      par(mfrow=nbclass, mar=c(4,4,2.5,0.5), mgp = c(2,1,0)) #fig=c(0,0.8,0,1),
	  for (i_c in 1:(nbclass[1]*nbclass[2])){
         indice<-which(classif==i_c)
	     tmp<-apply(geoW$values[,indice],1,mean,na.rm=TRUE)
		 
		 #nodes average pattern
		 
		 #To do: Graph package to be used
		 ##############################################################
	     values=cbind(geoW$ground$grid,tmp)	 
	     Min_values<-min(geoW$values,na.rm=TRUE)
         Max_values<-max(geoW$values,na.rm=TRUE)
         colour_indice<-round(100*(values[,3]-Min_values)/(Max_values-Min_values))/100
         colour_indice[is.na(colour_indice)]<-0
         colour_indice<-1-colour_indice 
         colour_indice<-10^colour_indice/10
         colour_indice<-grey(colour_indice)
         plot(values[,1],values[,2],col=colour_indice,pch=15,cex=4,xlim=c(geoW$ground$xMin,geoW$ground$xMax),ylim=c(geoW$ground$yMin,geoW$ground$yMax),axes=FALSE,xlab="", ylab="",asp=1)
         par(new=TRUE)
         plot(geoW$parameter$adm_border,border="blue",lwd=1,xlim=c(geoW$ground$xMin,geoW$ground$xMax),ylim=c(geoW$ground$yMin,geoW$ground$yMax),asp=1)
	     box() 
         axis(2, ylim=c(geoW$ground$yMin,geoW$ground$yMax))
         #mtext("LAT",side=2,line=2.5)	 
         axis(1, ylim=c(geoW$ground$xMin,geoW$ground$xMax))
         #mtext("LON",side=1,line=2.5)
         ##############################################################

		 #graph.values_on_map(values=cbind(geoW$ground$grid,tmp),
         #               min_value=min(geoW$values,na.rm=TRUE),
		 #               max_value=max(geoW$values,na.rm=TRUE),
         #               INDICATOR=FALSE,
         #               study_area=FALSE,
		 #               xMin_study=geoW$ground$xMin,
	     #               xMax_study=geoW$ground$xMax,
         #               yMin_study=geoW$ground$yMin,
		 #               yMax_study=geoW$ground$yMax,
		 #               adm_border=geoW$parameter$adm_border,
         #               path=NULL,
		 #               title=NULL,
		 #               xlab="X coord",
		 #               ylab="Y coord",
		 #               values_unit_legend="",
		 #               width_mm=NULL,
		 #               height_mm=NULL  )
         ###################################################################
                   
      }
	  
   }
   
   results<-list(node=node,
                 classif=classif)
				 
   return(results)				 
   
}
