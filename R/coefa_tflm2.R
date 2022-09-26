#' Trimming factor loading matrices(The cutoff range can be selected(0~1))
#'
#' The function can provide the same effect as the coefa_tflm().The difference
#' is that its cutoff value has a more flexible optional range (0~1).It should
#' be noted that the flexible cutoff selection range may cause some problems.
#' coefa_tflm() is recommended for stable performance
#' @param x A list of multiple original factor loading matrices included without
#' NA.
#' @param methodE The methods for trimming multiple factor loading matrices from
#'  the original study."s" and "ls" are two methods of trimming the original factor
#'  loading matrix, which are widely used in coefa studies.The s = Shafer’s(2005)
#'  method; ls = the Loeber and Schmaling’s method(1985).
#' @param cutoff The Criteria for trimming factor loadings matrices.
#'
#' @return A list contained those trimmed factor loading matrices.
#' @export
#'
#' @references
#' Loeber,R., & Schmaling, K. B. (1985). Empirical evidence for overt and covert
#' patterns of antisocial conduct problems: a metaanalysis. Journal of abnormal
#'  child psychology, 13(2), 337--353.
#'
#' Shafer,A. B.(2005). Meta-analysis of the Brief Psychiatric Rating Scale
#' factor structure. Psychological Assessment, 17(3),324--335.
#'
#' Shafer,A. B. (2006). Meta-analysis of the factor structures of four depression
#'  questionnaires: Beck, CES-D, Hamilton, and Zung. Journal of clinical
#'  psychology, 62(1), 123--146.
#'
#' @examples
#' mx1<-matrix(c(0.1,0.2,0.3,0.4),nrow=2)
#' mx2<-matrix(c(0.4,0.3,0.2,0.1),nrow=2)
#' list1<-list(mx1,mx2)
#' #Trim matrices using the Shafer's method ,cutoff is 0.4.
#' matrices.tflm<-coefa_tflm2(list1,methodE="s",cutoff=0.4)
coefa_tflm2<-function(x,methodE=c("s","ls"),cutoff){
  #Loeber and Schmaling's method
  af <- function(x,y){
    for (i in 1:nrow(x)){
      for(j in 1:ncol(x)){
        if (x[i,j]>=y){
          x[i,j]=1
        }
        else{x[i,j]=0}
      }
    }
    return(x)
  }
  #Shafer's method
  AF <- function(x,y){
    for (i in 1:nrow(x)){
      ma=max(x[i,])
      for(j in 1:ncol(x)){
        if (x[i,j]==ma & x[i,j]>=y){
          x[i,j]=1
        }
        else{x[i,j]=0}
      }
    }
    return(x)
  }
  if(methodE=="s"){
    for (i in 1:length(x)) {
      x[[i]]<-AF(x[[i]],y = cutoff)
    }
    re2<-x
  }
  else if(methodE=="ls"){
    for (i in 1:length(x)) {
      x[[i]]<-af(x[[i]],y = cutoff)
    }
    re2<-x
  }
  return(re2)
}
