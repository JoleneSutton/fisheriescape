# Fisheriescape path to CEU, summarize trips within fishing areas

See Table 2 in "Building a Fisheriescape: mapping the threat of marine
wildlife entanglement in vertical fishing lines in the Gulf of St.
Lawrence, Canada".

## Usage

``` r
fs_summarize_trips(
  df,
  gear.type = NULL,
  group.cols = NULL,
  gear.col = NULL,
  hour.col = NULL,
  day.col = NULL
)
```

## Arguments

- df:

  A data frame

- gear.type:

  Must be either 'trap' or 'non-trap'. Required.

- group.cols:

  Names of columns of grouping variables. Needs to include the trip id
  and fishing area columns.

- gear.col:

  Name of gear column to summarize. Required.

- hour.col:

  Name of hours column to summarize. Required.

- day.col:

  Name of day column to summarize. Required if gear.type is non-trap.

## Value

A data frame

## Examples

``` r
if (FALSE) { # \dontrun{
trips<-fs_summarize_trips(df=ziff,
                          gear.type="trap",
                          group.cols=c('fishery.name','gear.name','trip.id','fleet'),
                          gear.col='gear.amount',
                          hour.col='hours.fished')
} # }
```
