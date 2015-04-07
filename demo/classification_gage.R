rm(list=ls())

###########################################################################
###########################################################################
##                                                                       ##
##   LIBRARIES AND DATA                                                  ##
##                                                                       ##
###########################################################################
###########################################################################

#install_github("jeanmarielepioufle/graph")
library(graph)
library(geoW)
library(wgen)   
data(RRNSDNUD)
data(TAMRR)

############################################################################
############################################################################
## CLASSIFICATION: 6*6 classes, toroidal                                  ##
##                                                                        ##
############################################################################
############################################################################
RRNSDNUD_checked<-geoW.check(geoW=RRNSDNUD,variable="RR",percentage=30,COL=TRUE,ROW=FALSE,SIM=TRUE)

#nblcass=4*4+1: +1 because precipitation
tmp<-classification(geoW=RRNSDNUD_checked,method="som",TEMPORAL=TRUE,nbclass=c(n1=4,n2=4),FIGURE=TRUE)



