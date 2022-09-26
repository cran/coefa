#' Trimming factor loading matrices
#'
#' The original factor loading matrices after removing nulls will be trimmed through
#' this function.At the end all the matrices only have two elements,0 or 1.
#' @usage coefa_tflm(x,methodE=c("s","ls"),cutoff=c(0.3,0.4,0.5))
#' @param x A list of multiple original factor loading matrices included without
#' NA.
#' @param methodE The methods for trimming multiple factor loading matrices from
#'  the original study."s" and "ls" are two methods of trimming the original factor
#'  loading matrix, which are widely used in coefa studies.The s = Shafer’s(2005)
#'  method; ls = the Loeber and Schmaling’s method(1985).
#' @param cutoff The Criteria for trimming factor loadings matrices. When greater
#'  than the cutoff value, a factor loading is assigned as 1; otherwise, it
#'  assigned as 0. And the cutoff value can be given as 0.3, or 0.4, or 0.5.
#' @details This function is mainly used to discard the intensity information of
#' the original factor loading matrices and to retain the direction information
#' which make it into binary data.
#'
#' During the coefa trimming process,the strength information of the factor
#' loading matrices are given up,and the direction information is preserved.
#' "s" and "ls" are two different methods in the coefa study.
#'
#' If method="s", the factor loading matrices will be trimmed using the
#' Shafer's(2005)method, that is, only the highest salient loadings of each variable
#' in each factor are retained. If method="ls", the factor loading matrices will
#' be trimmed using the Loeber and Schmaling's (1985) method,all insignificant
#' factor loadings will be valued as 0.
#'
#' Cutoff is just some fixed criterion for trimming the factor loading matrix.
#' For example:if cutoff=0.3,elements in the factor loading matrix greater
#' than 0.3 will be given the value of 1,and elements less than 0.3 will be
#' replaced with 0.
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
#' @examples
#' mx1<-matrix(c(0.1,0.2,0.3,0.4),nrow=2)
#' mx2<-matrix(c(0.4,0.3,0.2,0.1),nrow=2)
#' list1<-list(mx1,mx2)
#' #Trim matrices using the Shafer's method ,cutoff is 0.3.
#' matrices.tflm<-coefa_tflm(list1,methodE="s",cutoff=0.3)
coefa_tflm<-function(x,methodE=c("s","ls"),cutoff=c(0.3,0.4,0.5)){
  #Loeber and Schmaling's method
  ##cutoff=0.3
  af1 <- function(x){
    for (i in 1:nrow(x)){
      for(j in 1:ncol(x)){
        if (x[i,j]>=0.3){
          x[i,j]=1
        }
        else{x[i,j]=0}
      }
    }
    return(x)
  }
  ##cutoff=0.4
  af2 <- function(x){
    for (i in 1:nrow(x)){
      for(j in 1:ncol(x)){
        if (x[i,j]>=0.4){
          x[i,j]=1
        }
        else{x[i,j]=0}
      }
    }
    return(x)
  }
  ##cutoff=0.5
  af3 <- function(x){
    for (i in 1:nrow(x)){
      for(j in 1:ncol(x)){
        if (x[i,j]>=0.5){
          x[i,j]=1
        }
        else{x[i,j]=0}
      }
    }
    return(x)
  }
  #Shafer's method
  ##cutoff=0.3
  AF1 <- function(x){
    for (i in 1:nrow(x)){
      ma=max(x[i,])
      for(j in 1:ncol(x)){
        if (x[i,j]==ma & x[i,j]>=0.3){
          x[i,j]=1
        }
        else{x[i,j]=0}
      }
    }
    return(x)
  }
  ##cutoff=0.4
  AF2 <- function(x){
    for (i in 1:nrow(x)){
      ma=max(x[i,])
      for(j in 1:ncol(x)){
        if (x[i,j]==ma & x[i,j]>=0.4){
          x[i,j]=1
        }
        else{x[i,j]=0}
      }
    }
    return(x)
  }
  ##cutoff=0.5
  AF3 <- function(x){
    for (i in 1:nrow(x)){
      ma=max(x[i,])
      for(j in 1:ncol(x)){
        if (x[i,j]==ma & x[i,j]>=0.5){
          x[i,j]=1
        }
        else{x[i,j]=0}
      }
    }
    return(x)
  }
  if(methodE=="s"){
    for (i in 1:length(x)) {
      if(cutoff=="0.3"){
        x[[i]]<-AF1(x[[i]])
      }else if(cutoff=="0.4"){
        x[[i]]<-AF2(x[[i]])
      }else if(cutoff=="0.5"){
        x[[i]]<-AF3(x[[i]])
      }
    }
    re2<-x
  }
  else if(methodE=="ls"){
    for (i in 1:length(x)) {
      if(cutoff=="0.3"){
        x[[i]]<-af1(x[[i]])
      }else if(cutoff=="0.4"){
        x[[i]]<-af2(x[[i]])
      }else if(cutoff=="0.5"){
        x[[i]]<-af3(x[[i]])
      }
    }
    re2<-x
  }
  return(re2)
}
