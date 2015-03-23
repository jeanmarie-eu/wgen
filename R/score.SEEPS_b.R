#' score.SEEPS_b
#'
#' Seeps
#' score SEEPS, cf Rodwell et al 2010; Haiden et al 2012
#' 0:perfect
#' the higher value, the less skilled
#' @param matrix_data observed
#' @param matrix_data2 forecasted
#' @param ths
#' @keywords score
#' @export
#' @examples
#' score.SEEPS_b()
score.SEEPS_b<-function(matrix_data, #observed
				matrix_data2,#forecasted
				p1,
				ths) {

#contingency table prpbability
#               obs
#    |  zero  |   >0 & <=q2  | >q2
#f   |   a           b          c     
#o   |   d           e          f   
#r   |   g           h          i   
contingency_matrix<-matrix(NA,3,3)

##ths<-c(0.01,q2)
			
#ct_a<-length(which (	(matrix_data==0)                      & (matrix_data2==0)) ) #NEG
ct_a<-length(which (	score.ContingencyCase(data1=matrix_data, 
                                         data2=matrix_data2,
										 ths1=ths[1],
										 ths2=ths[1],
										 caseCT="NEG")))
										 
#ct_b<-length(which (	((matrix_data>0) & (matrix_data<=q2)) & (matrix_data2==0)) ) #FA1
ct_b<-length(which (	score.ContingencyCase(data1=matrix_data,
                                         data2=matrix_data2,
										 ths1=ths,
										 ths2=ths[1],
										 caseCT="MISSING")))
										 
#ct_c<-length(which (	(matrix_data>q2)                      & (matrix_data2==0)) ) #FA2 
ct_c<-length(which (	score.ContingencyCase(data1=matrix_data,
                                         data2=matrix_data2,
										 ths1=ths[2],
										 ths2=ths[1],
										 caseCT="MISSING")))

#ct_d<-length(which (	(matrix_data==0)                      & ((matrix_data2>0) & (matrix_data2<=q2))) )
ct_d<-length(which (	score.ContingencyCase(data1=matrix_data,
                                         data2=matrix_data2,
										 ths1=ths[1],
										 ths2=ths,
										 caseCT="FA")))
										 
#ct_e<-length(which (	((matrix_data>0) & (matrix_data<=q2)) & ((matrix_data2>0) & (matrix_data2<=q2))) )
ct_e<-length(which (	score.ContingencyCase(data1=matrix_data, 
                                         data2=matrix_data2,
										 ths1=ths,
										 ths2=ths,
										 caseCT="HIT")))
										 
#ct_f<-length(which (	(matrix_data>q2)                      & ((matrix_data2>0) & (matrix_data2<=q2))) )
ct_f<-length(which (	score.ContingencyCase(data1=matrix_data,
                                         data2=matrix_data2,
										 ths1=ths[2],
										 ths2=ths,
										 caseCT="MISSING")))

#ct_g<-length(which (	(matrix_data==0)                      & (matrix_data2>q2)) )
ct_g<-length(which (	score.ContingencyCase(data1=matrix_data,
                                         data2=matrix_data2,
										 ths1=ths[1],
										 ths2=ths[2],
										 caseCT="FA")))
										 
#ct_h<-length(which (	((matrix_data>0) & (matrix_data<=q2)) & (matrix_data2>q2)) )
ct_h<-length(which (	score.ContingencyCase(data1=matrix_data,
                                         data2=matrix_data2,
										 ths1=ths,
										 ths2=ths[2],
										 caseCT="FA")))
										 
#ct_i<-length(which (	(matrix_data>q2)                      & (matrix_data2>q2)) )
ct_i<-length(which (	score.ContingencyCase(data1=matrix_data,
                                         data2=matrix_data2,
										 ths1=ths[2],
										 ths2=ths[2],
										 caseCT="HIT")))

contingency_matrix[1,1]<-ct_a/sum(ct_a,ct_d,ct_g)#/sum(ct_a,ct_b,ct_c)#
contingency_matrix[1,2]<-ct_b/sum(ct_a,ct_d,ct_g)#/sum(ct_a,ct_b,ct_c)#/sum(ct_b,ct_e,ct_h)
contingency_matrix[1,3]<-ct_c/sum(ct_c,ct_f,ct_i)#/sum(ct_a,ct_b,ct_c)#/sum(ct_c,ct_f,ct_i)
contingency_matrix[2,1]<-ct_d/sum(ct_a,ct_d,ct_g)#/sum(ct_d,ct_e,ct_f)#/sum(ct_a,ct_d,ct_g)
contingency_matrix[2,2]<-ct_e/sum(ct_b,ct_e,ct_h)#/sum(ct_d,ct_e,ct_f)#/sum(ct_b,ct_e,ct_h)
contingency_matrix[2,3]<-ct_f/sum(ct_c,ct_f,ct_i)#/sum(ct_d,ct_e,ct_f)#/sum(ct_c,ct_f,ct_i)
contingency_matrix[3,1]<-ct_g/sum(ct_a,ct_d,ct_g)#/sum(ct_g,ct_h,ct_i)#/sum(ct_a,ct_d,ct_g)
contingency_matrix[3,2]<-ct_h/sum(ct_b,ct_e,ct_h)#/sum(ct_g,ct_h,ct_i)#/sum(ct_b,ct_e,ct_h)
contingency_matrix[3,3]<-ct_i/sum(ct_c,ct_f,ct_i)#/sum(ct_g,ct_h,ct_i)#/sum(ct_c,ct_f,ct_i)

#Scoring matrix
scoring_matrix<-c(   0,                   1/(1-p1),        4/(1-p1),     
                     1/p1,                0,               3/(1-p1),   
                     1/p1+3/(2+p1),       3/(2+p1),        0)  
		 
dim(scoring_matrix)<-c(3,3)
scoring_matrix<-0.5*t(scoring_matrix)

seeps<-sum(contingency_matrix*scoring_matrix)
seeps_matrix<-contingency_matrix*scoring_matrix

results<-list(seeps=seeps,seeps_matrix=seeps_matrix)
return(results)
}
