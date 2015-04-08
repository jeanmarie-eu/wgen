rm(list=ls())

###########################################################################
###########################################################################
##                                                                       ##
##   LIBRARIES AND DATA                                                  ##
##                                                                       ##
###########################################################################
###########################################################################

#install_github("jeanmarielepioufle/graph")
#install_github("jeanmarielepioufle/geoW")
#install_github("jeanmarielepioufle/simulation")
library(graph)
library(geoW)
library(simulation)
library(basic)
library(kohonen)
library(wgen)   
data(RRNSDNUD)
data(TAMRR)


############################################################################
############################################################################
## CLASSIFICATION: 4*4 classes, toroidal                                  ##
##                                                                        ##
############################################################################
############################################################################

#RRNSDNUD
RRNSDNUD_checked<-geoW.check(geoW=RRNSDNUD,variable="RR",percentage=30,SIM=TRUE,OUTLIER=FALSE)
#one extra class for precipitation: dry step
dry<-RRNSDNUD_checked$zero$zero_col
tmp<-classification(geoW=RRNSDNUD_checked,method="som",TEMPORAL=TRUE,nbclass=c(n1=4,n2=4),omit=dry,FIGURE=FALSE)
tmp$classif[dry]<-4*4+1

#TAMRR, might take some time
TAMRR_checked<-geoW.check(geoW=TAMRR,variable=NULL,percentage=30,SIM=TRUE,OUTLIER=FALSE)
tmp<-classification(geoW=TAMRR_checked,method="som",TEMPORAL=TRUE,nbclass=c(n1=4,n2=4),omit=NULL,FIGURE=TRUE)


