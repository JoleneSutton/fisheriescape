# Fisheriescape pivot table 1 in path to CEU calculations.

Fisheriescape pivot table 1 in path to CEU calculations.

## Usage

``` r
fs_pivot1(df, keep.cols, gear.col, hour.col = NULL, day.col = NULL)
```

## Arguments

- df:

  A data frame

- keep.cols:

  Names of columns in df to retain in output, if different than
  'trip.id','fleet', 'gear.col','hour.col, or 'day.col'

- gear.col:

  Name of gear column to summarize. Required.

- hour.col:

  Name of hours column to summarize. Optional.

- day.col:

  Name of day column to summarize. Optional.
