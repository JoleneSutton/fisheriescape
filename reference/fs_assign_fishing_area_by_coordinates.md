# A function to help associate each record to a fishing area.

A function to help associate each record to a fishing area.

## Usage

``` r
fs_assign_fishing_area_by_coordinates(df, crs, polygon, nearest = TRUE)
```

## Arguments

- df:

  A data frame with columns "x" and "y" containing coordinates.

- crs:

  The coordinate reference system.

- polygon:

  A shapefile (class: SpatVector, geometry:polygons). The first column
  should be fishing area ids.

- nearest:

  For records with coordinates that do not overlap with polygons, do you
  want to return the name of the nearest polygon? Logical. Default is
  TRUE.
