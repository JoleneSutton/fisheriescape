# Fisheriescape path to CEU, fill NAs (replace NAs) after summarizing trips

Replaces NAs with either group means or group maximums. See Table 2 in
"Building a Fisheriescape: mapping the threat of marine wildlife
entanglement in vertical fishing lines in the Gulf of St. Lawrence,
Canada".

## Usage

``` r
fs_fill_missing(
  df,
  gear.type = NULL,
  vessel.col = NULL,
  year.col = NULL,
  week.col = NULL,
  also.grp = NULL
)
```

## Arguments

- df:

  The data frame produced by function `fs_summarize_trips`, for which
  duplicate rows have been removed.

- gear.type:

  Must be either 'trap' or 'non-trap'.

- vessel.col:

  Names of column of vessel (or licence) ids.

- year.col:

  Name of column of years.

- week.col:

  Name of column of weeks.

- also.grp:

  Names of other columns to group by. Optional. E.g., 'fishery', 'gear'

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

# restrict columns and remove duplicate rows
COLS<-c('fishery.name','gear.name','fleet','trip.id',
       "cfv",'year','sw',
       'sum.gear','max.hours')

trips2<-trips[,COLS]
trips2<-distinct(trips2)

trips3<-fs_fill_missing(trips2,
                       gear.type='trap',
                       vessel.col='cfv',
                       year.col='year',
                       week.col='sw',
                       also.grp=c("fishery.name" ,"gear.name"))

} # }
```
