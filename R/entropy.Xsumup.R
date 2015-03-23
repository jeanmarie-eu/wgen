#' entropy.Xsumup
#'
#' sum-up of X
#' @param X
#' @keywords entropy
#' @export
#' @examples
#' entropy.Xsumup()
entropy.Xsumup<-function(X){

   nb_classX<-length(rle(sort(X))$values)
   val_classX<-rle(sort(X))$values

   results<-list(nb_classX=nb_classX,
                 val_classX=val_classX)
   return(results)
}