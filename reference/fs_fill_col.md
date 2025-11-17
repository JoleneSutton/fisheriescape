# Fill NAs (replace NAs) in a column based on group summaries.

Fill NAs (replace NAs) in a column based on group summaries.

## Usage

``` r
fs_fill_col(df, group.cols, update.col, fun)
```

## Arguments

- df:

  A data frame

- group.cols:

  Names of columns to group by.

- update.col:

  Names of column that should be summarized.

- fun:

  The function for summarizing (e.g., mean, max etc.)

## Value

A data frame

## Examples

``` r
#df<-fs_get_data(years=2016:2017,species.sought=144,gclass=1,gearcode=41,nafo=c('4t','4s'))
#test<-fs_fill_col(df,group.cols=c('year','nafodiv'),update.col='nugear',fun=mean)
#summary(df$nugear)
#summary(test$nugear)
#test.fail<-fs_fill_col(df,group.cols=c('year','nafodiv'),update.col=c('nugear','amtgear'),fun=mean)
```
