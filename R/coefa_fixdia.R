#' Fix(Replace) the diagonal values in the matrix
#'
#' The diagonal of the matrix is replaced by 1 ,so that the matrix can be
#' tested for KMO and Bartlett's test.
#' @usage coefa_fixdia(x,test=FALSE,sz)
#' @param x A matrix that needs to replace the value of the diagonal in matrix.
#' @param test A logical value.If test=TRUE,KMO and Bartlett's test will be done.
#' @param sz The total sample sizes of include studies.
#' @details The diagonal of the similarity matrix will be adjusted by this
#' function, COEFA results will be more accurate, and KMO and Bartlett' Test
#'  tests can be carried out.
#'
#'  If test=FALSE,you will obtain a more accurate
#'  result without KMO and Bartlett's test. If test= TRUE ,you would obtain
#'  a result containing KMO and Bartlett's Test ,similarity matrix fixed.
#'
#' @return A matrix or a list containing KMO and Bartlett's test.
#' @export
#' @examples #similarity matrix——matrices_acm
#' \donttest{fixedsmatrix<-coefa_fixdia(matrices_acm,sz=100)}
coefa_fixdia<-function(x,test=FALSE,sz){
  if(test==FALSE){
  j<-nrow(x)
  for(i in 1:j){
    x[i,i]<-1
  }
  return(x)}
  if(test==TRUE){
    j<-nrow(x)
    for(i in 1:j){
    x[i,i]<-1
  }
    K<-psych::KMO(x)
    B<-psych::cortest.bartlett(x,n=sz)
    Test1<-list(K,B,x)
    names(Test1)<-c("KMO","Bartlett's Test","Fixed similarity matrix")
  }
  return(Test1)
}
