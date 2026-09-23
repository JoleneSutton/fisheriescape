# Fisheriescape scores

## Overview

This vignette generates weekly fishery-specific and cumulative
fisheriescape scores from the site scores and CEU scores generated in
the previous vignettes.

## R packages

### CRAN R packages

``` r

library(dplyr)
library(terra)
library(ggplot2)
library(tidyterra)
```

### GitHub R packages

The following packages are all available from
<https://jolenesutton.github.io>.

``` r

library(fisheriescape) 
```

## Get site score and CEU scores

``` r

df.ss<-fs.data.site.score
df.ceu<-fs.data.ceu

dim(df.ss)
dim(df.ceu)

head(df.ss)
head(df.ceu)
#> [1] 15311    10
#> [1] 680   7
#> # A tibble: 6 × 10
#> # Groups:   nafodiv, fishery, gear.name, fa, year, sw [1]
#>   nafodiv fishery   gear.name fa     year sw    GRID_ID count sum.count.fa.yr.sw
#>   <chr>   <chr>     <chr>     <chr> <dbl> <chr> <chr>   <int>              <int>
#> 1 4S      Atlantic… longline  GQ_G…  2015 19    AVH-10…     2                 10
#> 2 4S      Atlantic… longline  GQ_G…  2015 19    AVH-10…     1                 10
#> 3 4S      Atlantic… longline  GQ_G…  2015 19    AVU-10…     4                 10
#> 4 4S      Atlantic… longline  GQ_G…  2015 19    AVV-10…     1                 10
#> 5 4S      Atlantic… longline  GQ_G…  2015 19    AVV-10…     1                 10
#> 6 4S      Atlantic… longline  GQ_G…  2015 19    AVX-10…     1                 10
#> # ℹ 1 more variable: ss <dbl>
#>   nafodiv   fishery gear.name         fa year sw       ceu
#> 1      4S Snow crab      trap  GQ_CFA_12 2015 20  16.42857
#> 2      4S Snow crab      trap GQ_CFA_12B 2015 14 148.28571
#> 3      4S Snow crab      trap GQ_CFA_12B 2015 15 511.00000
#> 4      4S Snow crab      trap GQ_CFA_12B 2015 16 771.00000
#> 5      4S Snow crab      trap GQ_CFA_12B 2015 17 765.00000
#> 6      4S Snow crab      trap GQ_CFA_12B 2015 18 642.00000
```

## Calculate fisheriescape scores

Fisheriescape is calculated as the product of the site score and CEU.

``` r


df.fs<-left_join(df.ss,df.ceu)


df.fs$fs<-df.fs$ss*df.fs$ceu

# Because spatial grid cells can be associated with multiple fishing areas we need to sum fs for each grid cell for each fishery and sw
# We no longer need to group by NAFO division
df.fs<-df.fs|>
  group_by(fishery,gear.name,sw,GRID_ID)|>
  summarize(fs=sum(fs))


summary(df.fs)
#>       fishery          gear.name             sw             GRID_ID     
#>  Length   :14017   Length   :14017   Length   :14017   Length   :14017  
#>  N.unique :    3   N.unique :    3   N.unique :   34   N.unique : 4862  
#>  N.blank  :    0   N.blank  :    0   N.blank  :    0   N.blank  :    0  
#>  Min.nchar:    9   Min.nchar:    4   Min.nchar:    2   Min.nchar:    7  
#>  Max.nchar:   16   Max.nchar:    8   Max.nchar:    2   Max.nchar:    8  
#>                                                                         
#>        fs           
#>  Min.   :  0.06122  
#>  1st Qu.: 22.54321  
#>  Median : 36.41875  
#>  Mean   : 45.83992  
#>  3rd Qu.: 60.85714  
#>  Max.   :956.95264
```

| fishery          | gear.name | sw  | GRID_ID  |        fs |
|:-----------------|:----------|:----|:---------|----------:|
| Atlantic halibut | longline  | 15  | BCI-1096 | 1.0714286 |
| Atlantic halibut | longline  | 15  | BCJ-1097 | 0.3571429 |
| Atlantic halibut | longline  | 15  | BCK-1096 | 0.3571429 |
| Atlantic halibut | longline  | 15  | BCM-1096 | 0.3571429 |
| Atlantic halibut | longline  | 15  | BCN-1103 | 2.4285714 |
| Atlantic halibut | longline  | 15  | BCO-1101 | 1.6190476 |

head(df.fs) {.table .table
style="width: auto !important; margin-left: auto; margin-right: auto;"}
