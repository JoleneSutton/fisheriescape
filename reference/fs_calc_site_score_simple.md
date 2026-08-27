# Calculates the fisheriescape site scores after all records have been assigned to a spatial reference grid.

Calculates the fisheriescape site scores after all records have been
assigned to a spatial reference grid.

## Usage

``` r
fs_calc_site_score_simple(
  df,
  fishing.area.col = NULL,
  year.col = NULL,
  week.col = NULL,
  grid.col = NULL
)
```

## Arguments

- df:

  A data frame with columns for fishing area id, year, week, and spatial
  reference grid cell id.

- fishing.area.col:

  The name of the column with fishing area ids.

- year.col:

  The name of the column with years.

- week.col:

  The name of the column with weeks.

- grid.col:

  The name of the column with spatial reference grid cell ids.
