rm(list=ls())

###########################################################################
###########################################################################
##                                                                       ##
##   LIBRARIES AND DATA                                                  ##
##                                                                       ##
###########################################################################
###########################################################################

#library("devtools")
#install_github("jeanmarielepioufle/graph")
#install_github("jeanmarielepioufle/geoW")
#install_github("jeanmarielepioufle/basic")
#install_github("metno/wgen")
library(graph)
library(geoW)
library(basic)
library(kohonen)
library(wgen)   

data(classifTS.era40D2_Z925)
data(classifTS.era40D2_Z500) 
data(classifTS.era40D2_U850)
data(classifTS.era40D2_V850)
data(classifTS.era40D2_T850)
data(classifTS.era40D2_RH850)


############################################################################
############################################################################
## CLASSIFICATION ANALYSIS                                                ##
##     - MI                                                            ##
##     - YEARLY                                                           ##
##     - MONTHLY                                                          ##
##                                                                        ##
############################################################################
############################################################################
values<-rbind(classifTS.era40D2_RH850$value,
              classifTS.era40D2_T850$value,
              classifTS.era40D2_U850$value,
			  classifTS.era40D2_V850$value,
			  classifTS.era40D2_Z500$value,
			  classifTS.era40D2_Z925$value)
			  
tobedone<-c("TEMPORAL","MI")#,"YEARLY")
tmp<-analyseC(values=values,
                   dates=classifTS.era40D2_Z925$date$dateTS,
                   action=tobedone,
				   period=NULL
				   )

tobedone<-c("TEMPORAL","MI")#,"YEARLY")
tmp2<-analyseC(values=values,
                   dates=classifTS.era40D2_Z925$date$dateTS,
                   action=tobedone,
				   period="MONTHLY"
				   )				   
				   
				   
	


	
				   
				   
tmp<-analyseC$BASIC[1:dim(analyseC$BASIC)[1],c(1,2,6,7)]
mi_vector<-matrix(NA,nr=nrow(tmp),nc=ncol(tmp))
colnames(tmp)
for (i in 1:nrow(tmp)) for (j in 1:ncol(tmp)) mi_vector[i,j]<-tmp[[i,j]] 
mi_vector<-mi_vector[sort.list(mi_vector[,3],decreasing = TRUE),]
mi_vector<-mi_vector[-which(mi_vector[,1]==mi_vector[,2]),]
mi_vector[,3]<-round(mi_vector[,3], 7)
mi_vector<-mi_vector[-which(duplicated(mi_vector[,3])),]

#ONLY for unic class TS
#nbe<-Ni*(1-diag(Pij))           #nb of event
#de<-Ni/nbe                      #average duration of one event

   
   

