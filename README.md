
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GeneSet

## Introduction

`GeneSet` is a package that represents gene sets in a tibble format with
the `tbl_geneset` class. Gene sets are read in and converted into a
tibble format. Typial `dplyr` operations can be performed on the tibble
gene set. Basic useage of the package will be shown here, please refer
to the vignette and help pages for the functions used for more advanced
usage and examples.

## Installation

Intall the most recent version from Bioconductor:

``` r
if(!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("GeneSet")
```

The development version is also available for install from GitHub:

``` r
BiocManager::install("Kayla-Morrell/GeneSet")
```

Then load `GeneSet`:

``` r
library(GeneSet)
```

## Input

`GeneSet` can create a `tbl_geneset` using two different input methods.
Here we will demonstate the most common approach, by inputting named
character vectors of gene sets. `tbl_geneset()` returns the vectors as a
`tbl_geneset` tibble containing two columns, `gene` and `set`.

``` r
tbl <- tbl_geneset(set1 = letters, set2 = LETTERS)
tbl
#> # A tbl_geneset: 52 x 2
#>    gene  set  
#>    <chr> <fct>
#>  1 a     set1 
#>  2 b     set1 
#>  3 c     set1 
#>  4 d     set1 
#>  5 e     set1 
#>  6 f     set1 
#>  7 g     set1 
#>  8 h     set1 
#>  9 i     set1 
#> 10 j     set1 
#> # … with 42 more rows
```

## Implemented functions

From here, `GeneSet` can perform many `dplyr` functions such as
`filter()`, `select()`, `mutate()`, `group_by()`, `ungroup()`,
`summarise()`, and `arrange()`.

``` r
library(dplyr)
```

``` r
tbl <- tbl_geneset(set1 = letters, set2 = LETTERS)
tbl
#> # A tbl_geneset: 52 x 2
#>    gene  set  
#>    <chr> <fct>
#>  1 a     set1 
#>  2 b     set1 
#>  3 c     set1 
#>  4 d     set1 
#>  5 e     set1 
#>  6 f     set1 
#>  7 g     set1 
#>  8 h     set1 
#>  9 i     set1 
#> 10 j     set1 
#> # … with 42 more rows
tbl %>% filter(gene == "a" | gene == "A")
#> # A tbl_geneset: 2 x 2
#>   gene  set  
#>   <chr> <fct>
#> 1 a     set1 
#> 2 A     set2
tbl %>% mutate(pval = rnorm(dim(tbl)[1]))
#> # A tbl_geneset: 52 x 3
#>    gene  set     pval
#>    <chr> <fct>  <dbl>
#>  1 a     set1   1.13 
#>  2 b     set1   1.28 
#>  3 c     set1  -0.646
#>  4 d     set1   1.00 
#>  5 e     set1   0.920
#>  6 f     set1   2.28 
#>  7 g     set1  -1.16 
#>  8 h     set1  -1.60 
#>  9 i     set1  -0.531
#> 10 j     set1   0.402
#> # … with 42 more rows
tbl %>% group_by(set) %>% summarise(n = n())
#> # A tibble: 2 x 2
#>   set       n
#>   <fct> <int>
#> 1 set1     26
#> 2 set2     26
```

A more indepth case study example is provided for the vignette
demonstrating the use of `GeneSet` with an experiment dataset `airway`
from the package `airway`.

# Future aims

The next steps for `GeneSet` is to develop a data structure to
coordinate additional tables. Currently there is a table for the
`geneset` annotation, but we would like to introduce a table for `gene`
annotations and one for `set` annotations. The end goal would be to
provide the user a flat table that shows all information from the three
tables (a triple tibble, or tribble).
