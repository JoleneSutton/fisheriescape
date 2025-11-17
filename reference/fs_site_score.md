# Fisheriescape calculate site score

Fisheriescape calculate site score

## Usage

``` r
fs_site_score(df, fa.poly, grid = NULL)
```

## Arguments

- df:

  The for.site.score data frame.

- fa.poly:

  The fishing area polygon. CRS should be albers. Must contain a column
  called 'fleet'.

- grid:

  Grid shapefile. If NULL, defaults to
  'dfo_hex_coffen_smout_cropped.shp'.
