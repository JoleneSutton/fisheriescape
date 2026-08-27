# Assign missing fleets based on nearest date within range for same cfv.

**\[deprecated\]** This function was deprecated. Please use
`fs_summarize_trips` instead.

## Usage

``` r
fs_fleet_by_date(df, match.type = "exact", num.days = NULL)
```

## Arguments

- df:

  A data frame containing columns: cfv, year, fleet, dateland in format
  "%Y%m%d"

- match.type:

  Match with dates that are either 'exact', 'same.year',
  'previous.year'. Default is 'exact' date. For 'same.year' and
  'previous.year', 'num.days' must be specified.

- num.days:

  Plus and minus days around date to search.
