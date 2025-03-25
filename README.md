
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rob

<!-- badges: start -->
<!-- badges: end -->

The goal of rob is to generate experimentation run orders for factorial designs using the assignment-expansion method, considering a balance between reducing bias and level changes.

## Installation

``` r
# install.packages("pak")
pak::pak("RomarioContoL/rob")
```

## Example

``` r
library(rob)
z<-c(2,2,2,2,2,2)
runorder(z)
z<-c(4,3,2,3,2)
runorder(z)
z<-c(3,3,2,4)
runorder(z)
```
