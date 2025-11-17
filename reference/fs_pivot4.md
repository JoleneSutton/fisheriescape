# Fisheriescape not really a pivot, but should be done after pivot table 3 in path to CEU calculations.

Fisheriescape not really a pivot, but should be done after pivot table 3
in path to CEU calculations.

## Usage

``` r
fs_pivot4(
  df1,
  trap.fishery,
  pivot3,
  fleet.col,
  prov.col = NULL,
  area.note.col = NULL
)
```

## Arguments

- df1:

  The ziff data frame.

- trap.fishery:

  Is it a trap fishery? Default is 'yes'.

- pivot3:

  The output from fs_pivot3

- fleet.col:

  Name of fleet column. Should match between df and pivot3

- prov.col:

  Currently NULL

- area.note.col:

  Currently NULL
