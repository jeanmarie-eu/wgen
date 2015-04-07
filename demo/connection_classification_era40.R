rm(list=ls())

###########################################################################
###########################################################################
##                                                                       ##
##   LIBRARIES AND DATA                                                  ##
##                                                                       ##
###########################################################################
###########################################################################
library(wgen)   

data(era40_msl_area_study_D2_matrix) 
#data(era40_Z950_area_study_D2_matrix) 
#data(era40_Z500_area_study_D2_matrix) 
#data(era40_U850_area_study_D2_matrix) 
#data(era40_V850_area_study_D2_matrix) 
#data(era40_T850_area_study_D2_matrix) 
#data(era40_RH850_area_study_D2_matrix) 

############################################################################
############################################################################
## CLASSIFICATION                                                         ##
##                                                                        ##
############################################################################
############################################################################

tmp<-classification(method="som",TEMPORAL=TRUE,values=ncdf_12h_matrix,nbclass=c(n1=6,n2=6))
#tmp$node
classifTS<-tmp$classif
classifdateTS<-dateTS



############################################################################
############################################################################
## CLASSIFICATION ANALYSIS                                                ##
##     - BASIC                                                            ##
##     - YEARLY                                                           ##
##     - MONTHLY                                                          ##
##     - COMPARISON using MUTUAL INFORMATION (entropy)                    ##
##                                                                        ##
############################################################################
############################################################################

#############################################
# HIGHLIGHTING CONNECTIONS IN BETWEEN TYPES #
############################################# 

type_ncdf<-"era40"
variable_file<-c("Z925","Z500","U850","V850","T850","RH850","PW") #c("msl","Z925","Z500","RH850","U850")
variable_ncdf<-c("z","z","u","v","t","r","tcwv")            #c("msl","z","z","r","u")

setwd(paste(source_path,"R-scripts/classification_analysis/",sep=""))
valueTS_p<-c()
namefile<-""
for (i in 1:length(variable_file)) {
   load(file= paste(path_Classification,variable_file[i],"_",type_ncdf,"_typeTS.RData",sep=""))
   load(file= paste(path_Classification,variable_file[i],"_",type_ncdf,"_dateTS.RData",sep=""))
   namefile<-paste(namefile,"_",variable_file[i],sep="")
   valueTS_p<-cbind(valueTS_p,as.matrix(typeTS))
   dateTS_p<-typedateTS[1:dim(valueTS_p)[1]]
}
namefile<-paste("analyseTS",namefile,"_",type_ncdf,sep="")
action_p<-c("BASIC")#,"YEARLY")
source("analyse_classification.R")



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

   
   
type_ncdf<-"era40"
variable_file<-c("Z925","Z500","U850","V850","T850","RH850","PW") #c("msl","Z925","Z500","RH850","U850")
variable_ncdf<-c("z","z","u","v","t","r","tcwv")            #c("msl","z","z","r","u")


setwd(paste(source_path,"R-scripts/classification_analysis/",sep=""))
valueTS_p<-c()
namefile<-""
for (i in 1:length(variable_file)) {
   load(file= paste(path_Classification,variable_file[i],"_",type_ncdf,"_typeTS.RData",sep=""))
   load(file= paste(path_Classification,variable_file[i],"_",type_ncdf,"_dateTS.RData",sep=""))
   namefile<-paste(namefile,"_",variable_file[i],sep="")
   valueTS_p<-cbind(valueTS_p,as.matrix(typeTS))
   dateTS_p<-typedateTS[1:dim(valueTS_p)[1]]
}
namefile<-paste("analyseTS",namefile,"_",type_ncdf,"_",sep="")
action<-c("BASIC",,,)
source("analyse_classification.R")
   


