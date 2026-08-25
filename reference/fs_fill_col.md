# Fill NAs (replace NAs) in a column based on group summaries.

**\[deprecated\]** This function was deprecated. Please use
`fs_fill_missing` instead.

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
