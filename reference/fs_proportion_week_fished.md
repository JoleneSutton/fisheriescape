# Fisheriescape path to CEU, calculate the proportion of each week fished

See Table 2 in "Building a Fisheriescape: mapping the threat of marine
wildlife entanglement in vertical fishing lines in the Gulf of St.
Lawrence, Canada".

## Usage

``` r
fs_proportion_week_fished(
  df = NULL,
  fish.area.summary = NULL,
  gear.type = NULL,
  week.col = NULL,
  fishing.area.col = NULL,
  also.grp = NULL
)
```

## Arguments

- df:

  The original data frame of fishing records that includes columns
  dateland and ctchdate, each formatted as YYYY-MM-DD

- fish.area.summary:

  The dataframe resulting from `fs_summarize_fishing_areas`

- gear.type:

  Must be either 'trap' or non.trap'.

- week.col:

  Name of week column. Names must match between df and fish.area.summary

- fishing.area.col:

  Name of fishing area column. Names must match between df and
  fish.area.summary

- also.grp:

  Optional additional columns to group by. E.g., fishery, gear. Names
  must match between df and fish.area.summary
