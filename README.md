
<!-- README.md is generated from README.Rmd. Please edit that file -->

# coefa

The goal of `coefa` is to provide a calculation program for the method
of Meta Analysis of Factor Analysis Based on Co-occurrence Matrices. The
`coefa` package is an effective tools which can be used to solve the
factor structure (i.e. inner structure of a construct, or scale)debate
in several disciplines, such as psychology,
psychiatry,management,education et al.

## Installation

You can install the development version of `coefa` like so:

``` r
install.packages("coefa")
```

## Usage

This is a basic example which shows you how to solve a inner structure
debate:

### Read

First, the factor loading matrices data from a line of primary studies
should be imported into the R environment.These data(factor loading
matrices)should be stored using the type of list in R.This work can be
executed manually or using using the `coefa_read()`function of the
`coefa` package.

``` r
library(coefa)
#> 载入需要的程辑包：openxlsx
#> 载入需要的程辑包：psych
```

The function `coefa_read()` provides an effective way to read data files
from a folder in a computer. The user can read several types of files by
specifying the parameters of the function. What should be noted is that
the miss values in data files will be replaced with the number of 0.

``` r
#Supposing that the type of data are xlsx files.
matrices.withoutNa<-coefa_read(type = "xlsx")
```

\*\*NOTE：Although the`coefa_read()` function can help you quickly read
the data in the folder, there are two points should be noted: (1) The
path of the stored file should be consistent with your workspace. (2)
The file in the folder should have the same file formats, and different
files should be set with different parameters.

### Trim factor loading matrices

In the step, all the factor loading matrices will be trimmed using the
Shafer’s (2005) method or the Loeber and Schmaling’s method(1985). And
the cutoff values (e.g., 0.3,0.4,0.5) can be given here according to the
users’ consideration.

``` r
#Suppose matrices.withoutNa is obtained by coefa_read function
mx1<-matrix(c(0.1,0.2,0.3,0.4,0.5,0.6),nrow = 3,byrow = TRUE)
mx2<-matrix(c(0.6,0.5,0.4,0.3,0.2,0.1),nrow = 3,byrow = TRUE)
matrices.withoutNa<-list(mx1,mx2)
#Take the Loeber&Schmaling(1985) method, the cutoff value is 0.4 as an example.The result is that values in the matrix greater than or equal to 0.4 will become 1, and less than 0.4 will become 0.
matrices.tflm<-coefa_tflm(matrices.withoutNa,methodE = "ls",cutoff = 0.4)
```

### Generate co-occurrence matrices

In this step, the function `coefa_gcm()` will be used to generate the
co-occurrence matrix.

``` r
matrices.gcm<-coefa_gcm(matrices.tflm)
```

### Aggregate co-occurrence matrices

In this step,aggregted co-occurrence matrix will be obtained using the
`coefa_acm()` in which the aggregation algorithm will be executed. Here,
you can set the parameter `samplesize = TURE` to add the weights to all
studies. The sample sizes will be valued by the `sz1` variable If
`samplesize = FALSE`, no weight will be considered in the aggregation
process. When this step finished, a final aggregated co-occurrence
matrix (weighted or unweighted by sample size) will be calculated.

``` r
#Assume that the sample sizes of the factor loading matrices for the two studies are 100 and 200, respectively.
sz1<-c(100,200)
matrices.acm<-coefa_acm(matrices.gcm,sz=sz1,samplesized=TRUE)
```

### Summary

The function `coefa_summary()` provides a preliminary screening and
suggestion for the later factor analysis. The results of Scree plot and
Kaiser’s criterion will be plotted by this function. Furthermore, this
function will test the aggregated co-occurrence matrix, and return that
whether it is a positive matrix not.

``` r
coefa_summary(matrices.acm,fa="pc")
```

### Factor analysis

Finally,the function `coefa_fa` will provide the choice for factor
extraction under the condition of co-occurrence matrix. The MDS, EFA,
PCA can be a choice, and the function will generate a plot for your
choice. A typical setting comes as follows.

``` r
coefa_fa(matrices.acm,nfactors = 6,methodcoefa = "EFA",rotate = "varimax",fm="pa")
```
