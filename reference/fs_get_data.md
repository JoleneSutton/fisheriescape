# Get and process ziff data for fisheriescape project.

Get and process ziff data for fisheriescape project.

## Usage

``` r
fs_get_data(
  years = NULL,
  species.sought = NULL,
  nafo = NULL,
  gclass = NULL,
  gearcode = NULL
)
```

## Arguments

- years:

  Which years? Required

- species.sought:

  Which species sought statac code? Required

- nafo:

  Which NAFO divisions? Case insensitive

- gclass:

  Which gear class?

- gearcode:

  Which gear code?

## Examples

``` r
#fs_get_data(years = 2017, species.sought = 705, nafo=c('4r'), gclass='1', gearcode=c('62','99'))
```
