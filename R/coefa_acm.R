#' Aggregate co-occurrence matrices
#'
#' This function is used to aggregate those co-occurrence matrices to form
#' an overall(pooled) co-occurrence matrix.The users can get two kinds of
#' aggregated (i.e. pooled) co-occurrence matrices: the non-weighted aggregated
#' co-occurrence matrix, and the weighted aggregated co-occurrence matrix.
#' And the users can make their choices through the setting of corresponding
#' parameters.
#' @usage coefa_acm(x,sz,samplesized =c(TRUE,FALSE))
#' @param x A list containing multiple co-occurrence matrices.
#' @param sz A vector containing multiple sample sized of original study.
#' The elements of the vector should be in the same order as the original study,
#'  that is, the first element is the sample size of the first original study,
#'  and the second element is the sample size of the second original study....
#' @param samplesized A logical value is used to select the weighting method.
#' If "samplesized"=TRUE, the final aggregated co-occurrence matrix will be
#' obtained by weighting the sample size. If "samplesized"=FALSE, the size of
#' the sample size is not considered in the process of summarizing the
#' co-occurrence matrix, that is, the co-occurrence matrix is added and divided
#' by the number of matrices.
#' @return A matrix formed by adding all the co-occurrence matrices,or adding
#' them after weighting by sample size.
#' @export
#'
#' @references
#' Shafer,A. B.(2005). Meta-analysis of the Brief Psychiatric Rating Scale
#' factor structure. Psychological Assessment, 17(3),324--335.
#'
#' Shafer,A. B. (2006). Meta-analysis of the factor structures of four depression
#'  questionnaires: Beck, CES-D, Hamilton, and Zung. Journal of clinical
#'  psychology, 62(1), 123--146.
#' @examples
#' #Suppose that matrices.gcm is the co-ocurrence matrices.
#' ##Note:This is just an example.
#' ##The real co-occurrence matrix should be generated from actual studies.
#' mx1.tflm<-matrix(c(1,0,0,1,1,0),nrow=2,byrow=2)
#' mx2.tflm<-matrix(c(1,0,0,1),nrow=2,byrow=2)
#' matrices.tflm<-list(mx1.tflm,mx2.tflm)
#' matrices.gcm<-coefa_gcm(matrices.tflm)
#' #Import a sample size collection containing the original study.
#' sz<-c(100,200)
#' #Aggregate multiple co-occurrence matrices.
#' matrices.acm<-coefa_acm(matrices.gcm,sz,samplesized = TRUE)
#' matrices.acm
coefa_acm<-function(x,sz,samplesized=c(TRUE,FALSE)){
  #Sample sizes for each study were multiplied by their corresponding matrices.
  if(samplesized==TRUE){
    i<-length(x)
    j<-length(sz)
    while(i>0&j>0){
      x[[i]]<-x[[i]]*sz[j]
      i<-i-1
      j<-j-1
    }
    x
    acmt<-Reduce("+",x)
    acm<-acmt/sum(sz)
    return(acm)
  }
  else if(samplesized==FALSE){
    i<-length(x)
    acm<-Reduce("+",x)/i
    return(acm)
  }
}
