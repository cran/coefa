#' Summary
#'
#' The function coefa_summary provides a preliminary preparation and suggestion
#' for the later factor analysis using the aggregated co-occurrence matrix.
#' And the results, the scree plot and Kaiser's criterion will be plotted by
#' this function.
#' @usage coefa_summary(object,fa,omitWarning=FALSE)
#' @param object A matrix of aggregated co-occurrence matrix.
#' @param fa The method of extract factor for aggregated co-occurrence matrix.
#' Principal components (fa="pc"),principal axis factor analysis (fa="fa),or both
#' of them could be choose.
#' @param omitWarning The default is not to omit warning.If omitWarning is default,
#' suggestions(Scree plot and Kaiser's criterion, parallel analysis) for
#' subsequent factor analysis will not be provided when the matrix is not
#' positive definite.If omitWarning=TRUE,warning will be skipped and suggestions
#' will be forced.
#' @details It should be noted that we should be alert to the positive
#' definiteness of the aggregated matrix. If the matrix is non-positive
#' definite, we should choose the factor extraction method carefully or we
#' should take other solutions (remove questions appropriately, or smooth
#' the matrix).
#' @return A list containing the matrix cases produced by each process of coefa.
#' @export
#'
#' @references
#' Cao,Y., & Zhang, Y. (2017). Multivariate statistic methods in psychology and
#' education. Beijing: Peking university press.158.
#'
#' @examples
#' #summary
#' coefa_summary(matrices_acm,fa="fa")
coefa_summary<-function(object,fa,omitWarning=FALSE){
  #Compute the eigenvalues of the matrix，test whether the matrix is positive definite.
  ev<-eigen(object)
  nev<-ev$values
  lengthe<-sum(nev>0)
  if(lengthe==length(nev)){
    fa.parallel(object,n.obs = 240,n.iter = 100,fa = fa)
    note<-("The matrix is the positivly definite")
    note
  }else{if(omitWarning==TRUE){
    fa.parallel(object,n.obs = 240,n.iter = 100,fa = fa)
    warning("The matrix is not positivly definite,EFA using ULS should be employed.")
  }else{warning("The matrix is not positivly definite,EFA using ULS should be employed.")

  }
  }
}
