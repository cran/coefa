## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
#load the library
library(coefa)
data("spence8")

#Supposing that the data is import by coefa_read
matrices.withoutNa<-spence8
#Only the first two factor loading matrices of spence8 are shown here
matrices.withoutNa[c(1,2)]

## ----eval=FALSE---------------------------------------------------------------
#  #import data into R from a folder
#  matrices.withoutNa<-coefa_read(type = "xlsx")

## ----results='hide'-----------------------------------------------------------
#Use the Shafer method to assign a value to the matrix of de-null values to generate a factor loading matrix after assignment
matrices.tflm<-coefa_tflm2(matrices.withoutNa,methodE = "ls",cutoff = 0.3)

## -----------------------------------------------------------------------------
#Only the first two factor loading matrices of spence8 are shown here
matrices.tflm[c(1,2)]

## -----------------------------------------------------------------------------
#Generate co-occurrence matrices
matrices.gcm<-coefa_gcm(matrices.tflm)
#Only the first two factor loading matrices of spence8 are shown here
matrices.gcm[c(1,2)]

## -----------------------------------------------------------------------------
#Import the size of sample,sz means the sample sizes from the first study to the last study is 252,750,461,285,224,425,1520,591.
sz<-c(252,750,461,285,224,425,1520,591)
#Aggregate multiple co-occurrence matrices
matrices.acm<-coefa_acm(matrices.gcm,sz,samplesized = TRUE)
matrices.acm

## ----eval=FALSE---------------------------------------------------------------
#  coefa_summary(matrices.acm,fa="fa")

## ----fig.width=10,fig.height=10-----------------------------------------------
coefa_fa(matrices.acm,nfactors = 6,methodcoefa = "EFA",rotate = "varimax",fm="uls")

