# Fill NAs (replace NAs) in duplicate rows. Use with caution (see examples)!!

Fill NAs (replace NAs) in duplicate rows. Use with caution (see
examples)!!

## Usage

``` r
fs_fill_rows(df, group.cols, fill.cols)
```

## Arguments

- df:

  A data frame

- group.cols:

  Names of columns to group by.

- fill.cols:

  Names of columns that should be filled.

## Value

A data frame

## Examples

``` r
Lines <- "ID Value1 Value2 Value3 Value4 Value5 Value6
1 A B C z z NA
1 A B C y NA z
2 A B C NA x NA
2 A B C x NA NA
3 A B C x NA NA
3 A B C x y z"
(DF <- read.table(text = Lines, header = TRUE, as.is = TRUE))
#>   ID Value1 Value2 Value3 Value4 Value5 Value6
#> 1  1      A      B      C      z      z   <NA>
#> 2  1      A      B      C      y   <NA>      z
#> 3  2      A      B      C   <NA>      x   <NA>
#> 4  2      A      B      C      x   <NA>   <NA>
#> 5  3      A      B      C      x   <NA>   <NA>
#> 6  3      A      B      C      x      y      z
fs_fill_rows(DF,1,2:ncol(DF))# works. Note columns "Value5" and "Value6" for ID "1"
#> # A tibble: 4 × 7
#> # Groups:   ID [3]
#>      ID Value1 Value2 Value3 Value4 Value5 Value6
#>   <int> <chr>  <chr>  <chr>  <chr>  <chr>  <chr> 
#> 1     1 A      B      C      z      z      z     
#> 2     1 A      B      C      y      z      z     
#> 3     2 A      B      C      x      x      NA    
#> 4     3 A      B      C      x      y      z     
fs_fill_rows(DF,"ID",2:ncol(DF))# works.  Note columns "Value5" and "Value6" for ID "1"
#> # A tibble: 4 × 7
#> # Groups:   ID [3]
#>      ID Value1 Value2 Value3 Value4 Value5 Value6
#>   <int> <chr>  <chr>  <chr>  <chr>  <chr>  <chr> 
#> 1     1 A      B      C      z      z      z     
#> 2     1 A      B      C      y      z      z     
#> 3     2 A      B      C      x      x      NA    
#> 4     3 A      B      C      x      y      z     
fs_fill_rows(DF,c(1,5),2:ncol(DF)) #works. Compare to previous examples.
#> # A tibble: 5 × 7
#> # Groups:   ID, Value4 [5]
#>      ID Value1 Value2 Value3 Value4 Value5 Value6
#>   <int> <chr>  <chr>  <chr>  <chr>  <chr>  <chr> 
#> 1     1 A      B      C      z      z      NA    
#> 2     1 A      B      C      y      NA     z     
#> 3     2 A      B      C      NA     x      NA    
#> 4     2 A      B      C      x      NA     NA    
#> 5     3 A      B      C      x      y      z     
fs_fill_rows(DF,"ID","Value1:Value6")# does nothing
#> # A tibble: 6 × 7
#> # Groups:   ID [3]
#>      ID Value1 Value2 Value3 Value4 Value5 Value6
#>   <int> <chr>  <chr>  <chr>  <chr>  <chr>  <chr> 
#> 1     1 A      B      C      z      z      NA    
#> 2     1 A      B      C      y      NA     z     
#> 3     2 A      B      C      NA     x      NA    
#> 4     2 A      B      C      x      NA     NA    
#> 5     3 A      B      C      x      NA     NA    
#> 6     3 A      B      C      x      y      z     
```
