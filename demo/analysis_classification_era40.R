rm(list=ls())

###########################################################################
###########################################################################
##                                                                       ##
##   LIBRARIES AND DATA                                                  ##
##                                                                       ##
###########################################################################
###########################################################################
library(wgen)   

data(era40D2_msl) 
data(era40D2_Z950)
data(era40D2_Z500) 
data(era40D2_U850)
data(era40D2_V850)
data(era40D2_T850)
data(era40D2_RH850)




variable_ncdf<-c("z","z")
variable_file<-c("Z925","Z500")
variable_ncdf<-c("z","z")
variable_file<-c("Z925","Z500")

#variable_file<-c("U850")
#variable_ncdf<-c("u")
load(file = paste("P:/github/wgen/data/er40_",variable_file[i],"_study_areaD2_matrix.Rdata",sep="")) 
load(file = paste("P:/github/wgen/data/er40_",variable_file[i],"_study_areaD2_dateTS.Rdata",sep="")) 
load(file = paste(path_era,type_data,"_",variable_file[i],"_",area_case_study$area_name,"_dateTS.Rdata",sep="")) 

#variable_file<-c("U850","V850","RH850","PW","T850") #c("msl","Z925","Z500","RH850")
#variable_ncdf<-c("u","v","r","tcwv","t")              #c("msl","z","z","r")
#variable_file<-c("U850","V850","RH850","PW","T850") #c("msl","Z925","Z500","RH850")
#variable_ncdf<-c("u","v","r","tcwv","t")              #c("msl","z","z","r")
#variable_file<-c("U850","V850","RH850","PW","T850") #c("msl","Z925","Z500","RH850")
#variable_ncdf<-c("u","v","r","tcwv","t")              #c("msl","z","z","r")
#variable_file<-c("U850","V850","RH850","PW","T850") #c("msl","Z925","Z500","RH850")
#variable_ncdf<-c("u","v","r","tcwv","t")              #c("msl","z","z","r")
#variable_file<-c("U850","V850","RH850","PW","T850") #c("msl","Z925","Z500","RH850")
#variable_ncdf<-c("u","v","r","tcwv","t")              #c("msl","z","z","r")
#variable_file<-c("U850","V850","RH850","PW","T850") #c("msl","Z925","Z500","RH850")
#variable_ncdf<-c("u","v","r","tcwv","t")              #c("msl","z","z","r")
 

   matrixTS<-ncdf_12h_matrix
   matrixTS<-cbind(matrixTS,ncdf_12h_matrix)



############################################################################
############################################################################
## CLASSIFICATION                                                         ##
##                                                                        ##
############################################################################
############################################################################

tmp<-classification(method="som",TEMPORAL=TRUE,values=MSL,nbclass=c(n1=6,n2=6))
#tmp$node
classifTS<-tmp$classif
classifdateTS<-dateTS



############################################################################
############################################################################
## CLASSIFICATION ANALYSIS                                                ##
##     - BASIC                                                            ##
##     - YEARLY                                                           ##
##     - MONTHLY                                                          ##
##                                                                        ##
############################################################################
############################################################################

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
   


