rm(list=ls())

###########################################################################
###########################################################################
##                                                                       ##
##   LIBRARIES AND DATA                                                  ##
##                                                                       ##
###########################################################################
###########################################################################

#install_github("jeanmarielepioufle/graph")
#install_github("jeanmarielepioufle/basic")
library(graph)
library(basic)
library(kohonen)
library(wgen)   
data(era40D2_Z925)
data(era40D2_Z500) 
data(era40D2_U850)
data(era40D2_V850)
data(era40D2_T850)
data(era40D2_RH850)

############################################################################
############################################################################
## CLASSIFICATION: 6*6 classes, toroidal                                  ##
##                                                                        ##
############################################################################
############################################################################

tmp<-classification(geoW=era40D2_Z925,method="som",TEMPORAL=TRUE,nbclass=c(n1=6,n2=6),omit=NULL,FIGURE=FALSE)
classifTS.era40D2_Z925<-list(value=tmp$classif,
                date=era40D2_Z925$date)
save(classifTS.era40D2_Z925,file="P:/github/wgen/data/classifTS.era40D2_Z925.rda")

tmp<-classification(geoW=era40D2_Z500,method="som",TEMPORAL=TRUE,nbclass=c(n1=6,n2=6),omit=NULL,FIGURE=FALSE)
classifTS.era40D2_Z500<-list(value=tmp$classif,
                date=era40D2_Z500$date)
save(classifTS.era40D2_Z500,file="P:/github/wgen/data/classifTS.era40D2_Z500.rda")

tmp<-classification(geoW=era40D2_U850,method="som",TEMPORAL=TRUE,nbclass=c(n1=6,n2=6),omit=NULL,FIGURE=FALSE)
classifTS.era40D2_U850<-list(value=tmp$classif,
                date=era40D2_U850$date)
save(classifTS.era40D2_U850,file="P:/github/wgen/data/classifTS.era40D2_U850.rda")

tmp<-classification(geoW=era40D2_V850,method="som",TEMPORAL=TRUE,nbclass=c(n1=6,n2=6),omit=NULL,FIGURE=FALSE)
classifTS.era40D2_V850<-list(value=tmp$classif,
                date=era40D2_V850$date)
save(classifTS.era40D2_V850,file="P:/github/wgen/data/classifTS.era40D2_V850.rda")

tmp<-classification(geoW=era40D2_T850,method="som",TEMPORAL=TRUE,nbclass=c(n1=6,n2=6),omit=NULL,FIGURE=FALSE)
classifTS.era40D2_T850<-list(value=tmp$classif,
                date=era40D2_T850$date)
save(classifTS.era40D2_T850,file="P:/github/wgen/data/classifTS.era40D2_T850.rda")

tmp<-classification(geoW=era40D2_RH850,method="som",TEMPORAL=TRUE,nbclass=c(n1=6,n2=6),omit=NULL,FIGURE=FALSE)
classifTS.era40D2_RH850<-list(value=tmp$classif,
                date=era40D2_RH850$date)
save(classifTS.era40D2_RH850,file="P:/github/wgen/data/classifTS.era40D2_RH850.rda")
