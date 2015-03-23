#' score.ContingencyCase
#'
#' score.ContingencyCase
#' @param data1
#' @param data2
#' @param ths1 threshold#1
#' @param ths2 threshold#2
#' @param caseCT default "HIT" #"FA", "MISSING", "NEG"
#' @keywords score
#' @export
#' @examples
#' score.ContingencyCase()
score.ContingencyCase<-function(data1,
                          data2,
						  ths1,
						  ths2,
						  caseCT="HIT" #FA, MISSING, NEG
						  ) {


   if (caseCT=="HIT")      results<-score.HIT(data1,data2,ths1,ths2) #hit
   if (caseCT=="MISSING")  results<-score.MISSING(data1,data2,ths1,ths2) #missing
   if (caseCT=="FA")       results<-score.FA(data1,data2,ths1,ths2) #FA
   if (caseCT=="NEG")      results<-score.NEG(data1,data2,ths1,ths2) #NEG
   return(results)
}
