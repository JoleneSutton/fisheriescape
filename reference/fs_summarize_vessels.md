# Fisheriescape path to CEU, summarize vessels (or licences) within each fishing area and time period

See Table 2 in "Building a Fisheriescape: mapping the threat of marine
wildlife entanglement in vertical fishing lines in the Gulf of St.
Lawrence, Canada".

## Usage

``` r
fs_summarize_vessels(
  df,
  gear.type = NULL,
  vessel.col = NULL,
  week.col = NULL,
  year.col = NULL,
  fishing.area.col = NULL,
  also.grp = NULL
)
```

## Arguments

- df:

  The dataframe after running `fs_summarize_trips` and `fs_fill_missing`

- gear.type:

  Must be either 'trap' or non.trap'.

- vessel.col:

  Name of vessel or licence column.

- week.col:

  Name of week column.

- year.col:

  Name of year column.

- fishing.area.col:

  Name of fishing area column.

- also.grp:

  Optional additional columns to group by. E.g., fishery, gear
