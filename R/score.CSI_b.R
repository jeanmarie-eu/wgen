#' score.CSI_b
#'
#' Critical Success Index
#' The CSI, also known as threat score, ranges from 0 to 1 where 0 indicates no skill and 1 is a perfect score. It
#' measures the fraction of the observed and/or forecasted precipitation that was correctly predicted. Unlike the
#' POD and the FAR, it takes into account both false alarms and misses, and is therefore a more balanced score
#' @param matrix_data observed
#' @param matrix_data2 forecasted
#' @param ths
#' @keywords score
#' @export
#' @examples
#' score.CSI_b()
score.CSI_b<-function(matrix_data, #observed
				matrix_data2,#forecasted,
				ths) {

#2x2 contingency table
#               obs
#f    |   Y      |   N                |
#o   Y|   Hit1   |  FA                |     
#r   N|   Missing|  correct negatives |   
       
contingency_matrix<-matrix(NA,2,2)
		
Hit   <-length(which (	score.ContingencyCase(data1=matrix_data,
                                         data2=matrix_data2,
										 ths1=ths,
										 ths2=ths,
										 caseCT="HIT")))

FA   <-length(which (	score.ContingencyCase(data1=matrix_data,
                                         data2=matrix_data2,
										 ths1=ths,
										 ths2=ths,
										 caseCT="FA")))

Missing   <-length(which (	score.ContingencyCase(data1=matrix_data,
                                         data2=matrix_data2,
										 ths1=ths,
										 ths2=ths,
										 caseCT="MISSING")))
										 
Neg<-length(which (	score.ContingencyCase(data1=matrix_data,
                                         data2=matrix_data2,
										 ths1=ths,
										 ths2=ths,
										 caseCT="NEG")))
										 

results<-(Hit)/(Hit+Missing+FA)
return(results)
}
