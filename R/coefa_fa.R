#' PCA and EFA
#'
#' Choosing an appropriate method to extract factor from the aggregated
#' co-occurrence matrix.
#' @param R a matrix. R is the pooled co-occurrence matrix obtained from the last step.
#' @param nfactors Number of factors could be extracted.
#' @param methodcoefa The method choosing factor model(i.e.PCA or EFA)."PCA" is
#'  principal component analysis,"EFA" is exploratory factor analysis.
#' @param rotate The rotate parameter has many options to be chosen. They are:
#' "none", "varimax", "quartimax", "bentlerT", "equamax", "varimin", "geominT"
#' and "bifactor" are orthogonal rotations. "Promax", "promax", "oblimin",
#' "simplimax", "bentlerQ, "geominQ" and "biquartimin" and "cluster".According
#'  to Shafer (2005, 2006), the only correct choice is “varimax”, because the
#'  aggregated co-occurrence matrix is different from a correlation matrix or
#'  a covariance matrix in essence. The default is to do a oblimin transformation.
#' @param fm This parameter also has many options.When fm="minres", it will do
#'  a minimum residual, as will fm="uls". Both of them use a first derivative.
#'  What should be noted is that the fm="uls" is recommend by experts because
#'  it don’t request the normality and positive as two prerequisites.The fm="ols"
#'   differs very slightly from "minres" in that it minimizes the entire residual
#'  matrix using an OLS procedure but uses the empirical first derivative.
#'  This will be slower.The fm="wls" will do a weighted least squares (WLS) solution;
#'   the fm="gls" does a generalized weighted least squares (GLS); and the fm="pa"
#'   will do the principal factor solution, fm="ml" will do a maximum likelihood
#'   factor analysis. The fm="minchi" will minimize the sample size weighted chi
#'   square when treating pairwise correlations with different number of subjects
#'    per pair. The fm ="minrank" will do a minimum rank factor analysis.
#'    The "old.min" will do minimal residual the way it was done prior to April,
#'    2017. The fm="alpha" will starts an alpha factor analysis as described in
#'    Kaiser and Coffey (1965).
#' @details It should be noted that we should be alert to the positive
#' definiteness of the aggregated matrix. If the matrix is non-positive
#' definite, we should choose the factor extraction method carefully or we
#' should take other solutions (remove questions appropriately, or smooth
#' the matrix).
#' @return a data frame
#' @export
#'
#' @examples
#' #Choosing EFA method to extract factor.
#' coefa_fa(matrices_acm,nfactors=6,methodcoefa="EFA",rotate="varimax",fm="ml")
coefa_fa<-function(R,nfactors,methodcoefa,rotate,fm){
  if(methodcoefa=="EFA"){

    D<-fa(r=R,nfactors = nfactors,rotate = rotate,fm=fm)
    factor.plot(D)
    psych::fa.diagram(D)
    fa(r=R,nfactors = nfactors,rotate = rotate,fm=fm)
  }
  else if(methodcoefa=="PCA"){
    R<-principal(r=R,nfactors = nfactors,rotate = rotate)
    factor.plot(R)
    psych::fa.diagram(R)
    principal(r=R,nfactors = nfactors,rotate = rotate)
  }
}
